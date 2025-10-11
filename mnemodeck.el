;;; mnemodeck.el --- Spaced repetition system for language learning -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Yilin Zhang
;; Keywords: tools
;; Version: 0.1.0
;; URL: https://github.com/yilin-zhang/mnemodeck
;; Package-Requires: ((emacs "30.1") (fsrs "6.0"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; MnemoDeck is a spaced repetition tool for language learners to build
;; vocabulary without much card-construction overhead.  It builds on top of FSRS
;; scheduling, and gives you enough control to shape the workflow inside Emacs.

;;; Code:

(require 'ansi-color)
(require 'calendar)
(require 'cl-lib)
(require 'fsrs)
(require 'json)
(require 'seq)
(require 'sqlite)
(require 'subr-x)
(require 'tabulated-list)
(require 'url)

;; -----------------------------------------------------
;;; Global
;; -----------------------------------------------------

(defgroup mnemodeck nil
  "A spaced repetition system using the FSRS algorithm."
  :group 'applications)

(defcustom mnemodeck-directory
  (expand-file-name "mnemodeck/" user-emacs-directory)
  "Base directory for all MnemoDeck data files."
  :type 'directory
  :group 'mnemodeck)

;; -----------------------------------------------------
;;; Scheduler
;; -----------------------------------------------------

(defgroup mnemodeck-scheduler nil
  "Scheduler for MnemoDeck."
  :group 'mnemodeck)

(defvar mnemodeck--fsrs-scheduler nil
  "Cached FSRS scheduler instance for MnemoDeck.")

;; Shared configuration is defined in mnemodeck.el.

(defcustom mnemodeck-desired-retention 0.9
  "Desired retention (between 0.0 and 1.0) for scheduling reviews.
Higher values (closer to 1.0) mean more frequent reviews.
Lower values allow longer intervals but higher risk of forgetting."
  :type 'float
  :set (lambda (symbol value)
         (set-default symbol value)
         (setq mnemodeck--fsrs-scheduler nil))
  :group 'mnemodeck-scheduler)

(defcustom mnemodeck-learning-steps '((10 :minute) (1 :day))
  "Learning steps for FSRS.
Each step is a list of (AMOUNT UNIT), where UNIT is :sec/:minute/:hour/:day.
Set to nil to disable the learning stage."
  :type '(repeat (list (number :tag "Amount")
                       (choice (const :sec)
                               (const :minute)
                               (const :hour)
                               (const :day))))
  :set (lambda (symbol value)
         (set-default symbol value)
         (setq mnemodeck--fsrs-scheduler nil))
  :group 'mnemodeck-scheduler)

(defcustom mnemodeck-relearning-steps '((10 :minute))
  "Relearning steps for FSRS after a lapse.
Each step is a list of (AMOUNT UNIT), where UNIT is :sec/:minute/:hour/:day.
Set to nil to disable relearning steps."
  :type '(repeat (list (number :tag "Amount")
                       (choice (const :sec)
                               (const :minute)
                               (const :hour)
                               (const :day))))
  :set (lambda (symbol value)
         (set-default symbol value)
         (setq mnemodeck--fsrs-scheduler nil))
  :group 'mnemodeck-scheduler)

(defcustom mnemodeck-day-rollover-hour 4
  "Hour of day (0-23) that starts a new review day."
  :type 'integer
  :group 'mnemodeck-scheduler)

(defvar mnemodeck--counter '(:reviewed 0 :due-review 0 :due-learning 0 :new 0)
  "Counter for reviewed, due-review, due-learning, and new cards.")

(cl-defstruct (mnemodeck-card-meta)
  ;; Card metadata keyed by word string.
  (added-date (fsrs-now))
  (last-review nil)
  (due (fsrs-now))
  (state :learning)
  (step 0)
  (stability nil)
  (difficulty nil)
  (hint nil))

(defun mnemodeck-card-meta-is-new (meta)
  "Check if the card described by META is new (never reviewed)."
  (null (mnemodeck-card-meta-last-review meta)))

(defun mnemodeck--clamp (value min-val max-val)
  "Clamp VALUE to be between MIN-VAL and MAX-VAL."
  (min max-val (max min-val value)))

(defun mnemodeck--shuffle-list (lst)
  "Shuffle LST randomly."
  (let* ((vec (vconcat lst))
         (len (length vec)))
    ;; Fisher-Yates shuffle for unbiased results.
    (dotimes (i len)
      (let* ((j (+ i (random (- len i))))
             (tmp (aref vec i)))
        (aset vec i (aref vec j))
        (aset vec j tmp)))
    (append vec nil)))

(defun mnemodeck--json-parse-safe (json-string context)
  "Parse JSON-STRING, reporting errors with CONTEXT."
  (condition-case err
      (json-parse-string json-string :object-type 'alist)
    (error
     (message "%s: %s" context (error-message-string err))
     nil)))

(defun mnemodeck--get-fsrs-scheduler ()
  "Return a configured FSRS scheduler for MnemoDeck."
  (or mnemodeck--fsrs-scheduler
      (setq mnemodeck--fsrs-scheduler
            (fsrs-make-scheduler
             :desired-retention mnemodeck-desired-retention
             :learning-steps mnemodeck-learning-steps
             :relearning-steps mnemodeck-relearning-steps
             :enable-fuzzing-p nil))))

(defun mnemodeck--normalize-fsrs-state (state)
  "Normalize STATE into an FSRS keyword."
  (cond
   ((keywordp state) state)
   ((stringp state)
    (intern (if (string-prefix-p ":" state) state (concat ":" state))))
   (t nil)))

(defun mnemodeck--fsrs-state-string (state)
  "Return the serialized string representation of STATE."
  (when state
    (let ((name (symbol-name state)))
      (if (string-prefix-p ":" name) (substring name 1) name))))

(defun mnemodeck--fsrs-rating-from-grade (grade)
  "Convert numeric GRADE to FSRS rating keyword."
  (pcase grade
    (1 :again)
    (2 :hard)
    (3 :good)
    (4 :easy)
    (_ (error "Invalid grade: %s" grade))))

(defun mnemodeck--day-start-time (&optional time)
  "Return the start time of the review day containing TIME."
  (let* ((time (or time (current-time)))
         (decoded (decode-time time))
         (day (nth 3 decoded))
         (month (nth 4 decoded))
         (year (nth 5 decoded))
         (rollover (mnemodeck--clamp mnemodeck-day-rollover-hour 0 23))
         (day-start (encode-time 0 0 rollover day month year)))
    (if (time-less-p time day-start)
        (time-subtract day-start (days-to-time 1))
      day-start)))

(defun mnemodeck--next-day-start-time (&optional time)
  "Return the next review day start time after TIME."
  (time-add (mnemodeck--day-start-time time) (days-to-time 1)))

(defun mnemodeck--time->fsrs-timestamp (time)
  "Return TIME formatted as an FSRS timestamp string."
  (fsrs-now time))

(defun mnemodeck--card-meta->fsrs-card (word meta)
  "Create an FSRS card for WORD from card META."
  (let ((card-id (abs (sxhash word))))
    (fsrs-make-card
     :card-id card-id
     :state (or (mnemodeck-card-meta-state meta) :learning)
     :step (mnemodeck-card-meta-step meta)
     :stability (mnemodeck-card-meta-stability meta)
     :difficulty (mnemodeck-card-meta-difficulty meta)
     :due (or (mnemodeck-card-meta-due meta) (fsrs-now))
     :last-review (mnemodeck-card-meta-last-review meta))))

(defun mnemodeck--apply-fsrs-card (meta card)
  "Update card META in-place from FSRS CARD and return META."
  (setf (mnemodeck-card-meta-state meta) (fsrs-card-state card))
  (setf (mnemodeck-card-meta-step meta) (fsrs-card-step card))
  (setf (mnemodeck-card-meta-stability meta) (fsrs-card-stability card))
  (setf (mnemodeck-card-meta-difficulty meta) (fsrs-card-difficulty card))
  (setf (mnemodeck-card-meta-due meta) (fsrs-card-due card))
  (setf (mnemodeck-card-meta-last-review meta) (fsrs-card-last-review card))
  meta)

(defun mnemodeck--update-card-with-grade (word meta grade)
  "Update card META for WORD based on GRADE rating (1-4)."
  (let* ((scheduler (mnemodeck--get-fsrs-scheduler))
         (rating (mnemodeck--fsrs-rating-from-grade grade))
         (review-time (fsrs-now))
         (card (mnemodeck--card-meta->fsrs-card word meta))
         (new-card (cl-nth-value 0
                                 (fsrs-scheduler-review-card
                                  scheduler card rating review-time))))
    (mnemodeck--apply-fsrs-card meta new-card))
  meta)

;; -----------------------------------------------------
;;; Database (db)
;; -----------------------------------------------------

(defgroup mnemodeck-db nil
  "Database for MnemoDeck."
  :group 'mnemodeck)

(defcustom mnemodeck-db-file
  (expand-file-name "mnemodeck.sqlite" mnemodeck-directory)
  "Path to the SQLite database file."
  :type 'file
  :group 'mnemodeck-db)

(defcustom mnemodeck-review-order
  '((:sort :due :asc :learning)
    (:shuffle :review)
    (:sort :added :desc :new))
  "Review order for due cards.

Each entry is one of:

  (:shuffle TARGETS)
  (:sort FIELD ORDER TARGETS)

TARGETS can be a single type or a list of types.  Types are:
`:learning', `:review', and `:new'.  `:learning' includes relearning cards.

FIELD can be `:due', `:added', `:last-review', `:difficulty', or `:stability'.
For `:learning' or `:new' targets, only `:due' and `:added' are supported.
ORDER can be `:asc' or `:desc'."
  :type '(repeat sexp)
  :group 'mnemodeck-review)

(defvar mnemodeck-db--conn nil
  "SQLite connection for MnemoDeck.")

(defconst mnemodeck-db--edit-sort-columns
  '(("Word" . "word")
    ("Hint" . "hint")
    ("Added" . "added_date")
    ("Due" . "due")
    ("State" . "state")
    ("Stability" . "stability")
    ("Difficulty" . "difficulty"))
  "Mapping of edit table headers to database columns.")

(defun mnemodeck-db--normalize-word (word)
  "Trim WORD and signal an error if empty."
  (let ((trimmed (string-trim (or word ""))))
    (if (string-empty-p trimmed)
        (error "Word cannot be empty")
      trimmed)))

(defun mnemodeck-db--normalize-hint (hint)
  "Trim HINT and return nil if empty."
  (when hint
    (let ((trimmed (string-trim hint)))
      (unless (string-empty-p trimmed)
        trimmed))))

(defun mnemodeck-db--normalize-row (row)
  "Normalize ROW values for word and hint fields."
  (when row
    (pcase-let ((`(,word ,added ,last-review ,due ,state ,step ,stability ,difficulty ,hint) row))
      (let ((normalized-word (mnemodeck-db--normalize-word word))
            (normalized-hint (mnemodeck-db--normalize-hint hint)))
        (list normalized-word
              added last-review due state step stability difficulty
              normalized-hint)))))

(defun mnemodeck-db--ensure-db-dir ()
  "Ensure the database directory exists."
  (let ((dir (file-name-directory mnemodeck-db-file)))
    (unless (file-exists-p dir)
      (make-directory dir t))))

(defun mnemodeck-db--ensure ()
  "Ensure SQLite connection and schema are initialized."
  (unless mnemodeck-db--conn
    (mnemodeck-db--ensure-db-dir)
    (setq mnemodeck-db--conn (sqlite-open mnemodeck-db-file))
    (sqlite-execute mnemodeck-db--conn "PRAGMA journal_mode = TRUNCATE;")
    (sqlite-execute mnemodeck-db--conn "PRAGMA foreign_keys = ON;")
    (sqlite-execute mnemodeck-db--conn
                    "CREATE TABLE IF NOT EXISTS cards (
                       word TEXT PRIMARY KEY,
                       added_date TEXT NOT NULL,
                       last_review TEXT,
                       due TEXT NOT NULL,
                       archived_at TEXT,
                       state TEXT NOT NULL,
                       step INTEGER,
                       stability REAL,
                       difficulty REAL,
                       hint TEXT
                     );")
    (sqlite-execute mnemodeck-db--conn
                    "CREATE INDEX IF NOT EXISTS idx_cards_due ON cards(due);"))
  mnemodeck-db--conn)

(defun mnemodeck-db--disconnect ()
  "Close the SQLite connection used by MnemoDeck."
  (when mnemodeck-db--conn
    (sqlite-close mnemodeck-db--conn)
    (setq mnemodeck-db--conn nil)))

(defun mnemodeck-db--session-window-open-p ()
  "Return non-nil when a review/edit session buffer is still open."
  (cl-some
   (lambda (buffer)
     (and (buffer-live-p buffer)
          (with-current-buffer buffer
            (or (derived-mode-p 'mnemodeck-review-mode)
                (derived-mode-p 'mnemodeck-edit-mode)))))
   (buffer-list)))

(defun mnemodeck-db--disconnect-if-idle ()
  "Disconnect DB when no review/edit session windows are open."
  (unless (mnemodeck-db--session-window-open-p)
    (mnemodeck-db--disconnect)))

(defun mnemodeck-db--select-card (word)
  "Return the card row for WORD or nil."
  (let ((conn (mnemodeck-db--ensure)))
    (mnemodeck-db--normalize-row
     (car (sqlite-select conn
                         "SELECT word, added_date, last_review, due, state, step, stability, difficulty, hint
                          FROM cards WHERE word = ?;"
                         (list word))))))

(defun mnemodeck-db--upsert-card (word card-meta)
  "Insert or update WORD with CARD-META in the database."
  (let* ((word (mnemodeck-db--normalize-word word))
         (hint (mnemodeck-db--normalize-hint (mnemodeck-card-meta-hint card-meta)))
         (conn (mnemodeck-db--ensure)))
    (setf (mnemodeck-card-meta-hint card-meta) hint) ; update to the normalized hint
    (sqlite-execute
     conn
     "INSERT INTO cards (word, added_date, last_review, due, state, step, stability, difficulty, hint)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
      ON CONFLICT(word) DO UPDATE SET
        added_date = excluded.added_date,
        last_review = excluded.last_review,
        due = excluded.due,
        state = excluded.state,
        step = excluded.step,
        stability = excluded.stability,
        difficulty = excluded.difficulty,
        hint = excluded.hint;"
     (list word
           (mnemodeck-card-meta-added-date card-meta)
           (mnemodeck-card-meta-last-review card-meta)
           (mnemodeck-card-meta-due card-meta)
           (mnemodeck--fsrs-state-string (mnemodeck-card-meta-state card-meta))
           (mnemodeck-card-meta-step card-meta)
           (mnemodeck-card-meta-stability card-meta)
           (mnemodeck-card-meta-difficulty card-meta)
           hint))))

(defun mnemodeck-db--update-hint (word hint)
  "Update WORD's hint with HINT in the database, return normalized hint."
  (let* ((word (mnemodeck-db--normalize-word word))
         (hint (mnemodeck-db--normalize-hint hint))
         (conn (mnemodeck-db--ensure)))
    (sqlite-execute conn "UPDATE cards SET hint = ? WHERE word = ?;"
                    (list hint word))
    hint))

(defun mnemodeck-db--delete-card (word)
  "Delete WORD from the database."
  (let ((conn (mnemodeck-db--ensure)))
    (sqlite-execute conn "DELETE FROM cards WHERE word = ?;" (list word))))

(defun mnemodeck-db--archive-card (word archived-at)
  "Mark WORD as archived at ARCHIVED-AT."
  (let ((conn (mnemodeck-db--ensure)))
    (sqlite-execute conn "UPDATE cards SET archived_at = ? WHERE word = ?;"
                    (list archived-at word))))

(defun mnemodeck-db--unarchive-card (word)
  "Clear WORD's archived flag."
  (let ((conn (mnemodeck-db--ensure)))
    (sqlite-execute conn "UPDATE cards SET archived_at = NULL WHERE word = ?;"
                    (list word))))

(defun mnemodeck-db--update-word (old-word new-word)
  "Rename OLD-WORD to NEW-WORD in the database, return normalized new word."
  (let ((conn (mnemodeck-db--ensure))
        (old-word (mnemodeck-db--normalize-word old-word))
        (new-word (mnemodeck-db--normalize-word new-word)))
    (when (string-equal old-word new-word)
      (cl-return-from mnemodeck-db--update-word old-word))
    (when (mnemodeck-db--select-card new-word)
      (error "Word \"%s\" already exists in the deck" new-word))
    (sqlite-execute conn "UPDATE cards SET word = ? WHERE word = ?;"
                    (list new-word old-word))
    new-word))

(defun mnemodeck-db--edit-filter-sql (filter)
  "Return (SQL . PARAMS) for FILTER."
  (pcase filter
    ('review (cons " WHERE archived_at IS NULL AND state = 'review'" nil))
    ('learning (cons " WHERE archived_at IS NULL AND state IN ('learning', 'relearning')" nil))
    ('archived (cons " WHERE archived_at IS NOT NULL" nil))
    (_ (cons " WHERE archived_at IS NULL" nil))))

(defun mnemodeck-db--edit-order-sql (sort-key)
  "Return SQL ORDER BY clause for SORT-KEY."
  (let* ((column (and sort-key (car sort-key)))
         (reverse (and sort-key (cdr sort-key)))
         (db-column (cdr (assoc-string column mnemodeck-db--edit-sort-columns)))
         (db-column (or db-column "word"))
         (direction (if reverse "DESC" "ASC"))
         (order-expr (if (member db-column '("stability" "difficulty"))
                         (format "COALESCE(%s, 0)" db-column)
                       (format "COALESCE(%s, '')" db-column))))
    ;; Tie-break equal sort values by insertion order so later-added cards stay
    ;; after earlier ones (later inserts usually have larger rowid values).
    (format " ORDER BY %s %s, rowid %s" order-expr direction direction)))

(defun mnemodeck-db--select-cards (&optional filter sort-key)
  "Return cards filtered by FILTER and sorted by SORT-KEY."
  (let ((conn (mnemodeck-db--ensure)))
    (pcase-let ((`(,where . ,params) (mnemodeck-db--edit-filter-sql filter)))
      (mapcar
       #'mnemodeck-db--normalize-row
       (sqlite-select conn
                      (concat
                       "SELECT word, added_date, last_review, due, state, step, stability, difficulty, hint
                        FROM cards"
                       where
                       (mnemodeck-db--edit-order-sql sort-key)
                       ";")
                      params)))))

(defun mnemodeck-db--row->card-meta (row)
  "Convert ROW into a `mnemodeck-card-meta' instance."
  (when row
    (pcase-let ((`(,_word ,added-date ,last-review ,due ,state ,step ,stability ,difficulty ,hint) row))
      (let* ((scheduler (mnemodeck--get-fsrs-scheduler))
             (state (mnemodeck--normalize-fsrs-state state))
             (added-date (or added-date (fsrs-now)))
             (last-review last-review)
             (due (or due (fsrs-now)))
             (is-new (null last-review))
             (state (or state (if is-new :learning :review)))
             (step (if is-new (or step 0) step))
             (stability (and (numberp stability) (> stability 0) stability))
             (difficulty (and (numberp difficulty) (> difficulty 0) difficulty))
             (stability (if (and (not is-new) (null stability))
                            (fsrs-scheduler-initial-stability scheduler :good)
                          stability))
             (difficulty (if (and (not is-new) (null difficulty))
                             (fsrs-scheduler-initial-difficulty scheduler :good)
                           difficulty)))
        (make-mnemodeck-card-meta
         :added-date added-date
         :last-review last-review
         :due due
         :state state
         :step step
         :stability stability
         :difficulty difficulty
         :hint hint)))))

(defun mnemodeck-db--review-normalize-targets (targets)
  "Normalize TARGETS into a list of review types."
  (cond
   ((keywordp targets) (list targets))
   ((listp targets) targets)
   (t (error "Invalid review targets: %S" targets))))

(defun mnemodeck-db--review-validate-order (order)
  "Signal error if ORDER is invalid for `mnemodeck-review-order'."
  (let ((all-targets '()))
    (dolist (step order)
      (pcase step
        (`(:shuffle ,step-targets)
         ;; Track targets as they appear so we can detect duplicates later.
         (setq all-targets
               (append (mnemodeck-db--review-normalize-targets step-targets) all-targets)))
        (`(:sort ,field ,order ,step-targets)
         (unless (memq order '(:asc :desc))
           (error "Invalid sort order: %S" order))
         (let* ((step-targets (mnemodeck-db--review-normalize-targets step-targets))
                ;; Learning/new cards only support due/added sorting.
                (allowed (if (or (memq :learning step-targets)
                                 (memq :new step-targets))
                             '(:due :added)
                           '(:due :added :last-review :difficulty :stability))))
           (unless (memq field allowed)
             (error "Invalid sort field: %S" field))
           (setq all-targets (append step-targets all-targets))))
        (_ (error "Invalid review order step: %S" step))))
    ;; Validate final target list and reject duplicate targets.
    (dolist (target all-targets)
      (unless (memq target '(:learning :review :new))
        (error "Invalid review target: %S" target)))
    (let ((seen (make-hash-table :test 'eq)))
      (dolist (target all-targets)
        (when (gethash target seen)
          (error "Review target already used: %S" target))
        (puthash target t seen)))))

(defun mnemodeck-db--review-sort-clause (field order)
  "Return ORDER BY clause for FIELD and ORDER."
  (let* ((column (pcase field
                   (:due "due")
                   (:added "added_date")
                   (:last-review "last_review")
                   (:difficulty "difficulty")
                   (:stability "stability")
                   (_ (error "Unknown sort field: %S" field))))
          (direction (if (eq order :asc) "ASC" "DESC")))
    ;; Tie-break equal sort values by insertion order so later-added cards stay
    ;; after earlier ones (later inserts usually have larger rowid values).
    (format " ORDER BY %s %s, rowid %s" column direction direction)))

(defun mnemodeck-db--review-target-clause (target now)
  "Return SQL clause and params for TARGET at NOW."
  ;; Review cards are due by day; learning cards are due by timestamp.
  (let ((review-cutoff (mnemodeck--time->fsrs-timestamp
                        (mnemodeck--next-day-start-time now)))
        (learning-cutoff (mnemodeck--time->fsrs-timestamp now)))
    (pcase target
      (:learning
       (cons "state IN ('learning', 'relearning')
              AND last_review IS NOT NULL
              AND due <= ?" (list learning-cutoff)))
      (:review
       (cons "state = 'review'
              AND last_review IS NOT NULL
              AND due <= ?" (list review-cutoff)))
      (:new
       (cons "last_review IS NULL
              AND due <= ?" (list review-cutoff)))
      (_ (error "Unknown review target: %S" target)))))

(defun mnemodeck-db--due-items (targets now field order)
  "Return due items for TARGETS at NOW, optionally sorted by FIELD ORDER."
  (let* ((conn (mnemodeck-db--ensure))
         (clauses-and-params (mapcar (lambda (target)
                                       (mnemodeck-db--review-target-clause target now))
                                     targets))
         ;; Build a single WHERE clause with the params in matching order.
         (clauses (mapcar #'car clauses-and-params))
         (params (apply #'append (mapcar #'cdr clauses-and-params)))
         (where (string-join clauses " OR "))
         (order-clause (when field (mnemodeck-db--review-sort-clause field order)))
         (sql (format
               "SELECT word, due, added_date, last_review, stability, difficulty
                FROM cards
                WHERE archived_at IS NULL
                  AND (%s)%s;"
               where (or order-clause "")))
         (rows (sqlite-select conn sql params)))
    (mapcar (lambda (row)
              (cl-loop for key in '(:word :due :added :last-review :stability :difficulty)
                       for val in row
                       append (list key val)))
            rows)))

(defun mnemodeck-db--review-step-items (step now)
  "Return ITEMS for STEP at NOW.
STEP can be a shuffle or sort clause."
  (pcase step
    (`(:shuffle ,targets)
     (let* ((step-targets (mnemodeck-db--review-normalize-targets targets))
            (step-items (mnemodeck-db--due-items step-targets now nil nil)))
       ;; Shuffle happens after SQL filtering (no ORDER BY).
       (mnemodeck--shuffle-list step-items)))
    (`(:sort ,field ,order ,targets)
      (unless (memq order '(:asc :desc))
        (error "Unknown sort order: %S" order))
      (let* ((step-targets (mnemodeck-db--review-normalize-targets targets))
             (step-items (mnemodeck-db--due-items step-targets now field order)))
        ;; Sorting is done by SQL ORDER BY for this step.
        step-items))
    (_ (error "Invalid review order step: %S" step))))

(defun mnemodeck-db--select-due-words ()
  "Return words due for review according to `mnemodeck-review-order'."
  (let* ((now (current-time))
         (items '()))
    (mnemodeck-db--review-validate-order mnemodeck-review-order)
    (dolist (step mnemodeck-review-order)
      (let ((step-items (mnemodeck-db--review-step-items step now)))
        ;; Preserve step ordering while concatenating.
        (setq items (append items step-items))))
    (mapcar (lambda (item) (plist-get item :word)) items)))

(defun mnemodeck-db--count (sql params)
  "Return the count for SQL query with PARAMS."
  (let* ((conn (mnemodeck-db--ensure))
         (row (car (sqlite-select conn sql params))))
    (if row (car row) 0)))

(defun mnemodeck-db--counts ()
  "Return counter plist from database state."
  (let* ((now (current-time))
         (day-start (mnemodeck--time->fsrs-timestamp
                     (mnemodeck--day-start-time now)))
         (review-cutoff (mnemodeck--time->fsrs-timestamp
                         (mnemodeck--next-day-start-time now)))
         (learning-cutoff (mnemodeck--time->fsrs-timestamp now))
         (reviewed (mnemodeck-db--count
                    "SELECT COUNT(*) FROM cards
                     WHERE archived_at IS NULL
                       AND last_review IS NOT NULL
                       AND last_review >= ?
                       AND last_review < ?;"
                    (list day-start review-cutoff)))
         (due-review (mnemodeck-db--count
                      "SELECT COUNT(*) FROM cards
                       WHERE archived_at IS NULL
                         AND last_review IS NOT NULL
                         AND state = 'review'
                         AND due <= ?;"
                      (list review-cutoff)))
         (due-learning (mnemodeck-db--count
                        "SELECT COUNT(*) FROM cards
                         WHERE archived_at IS NULL
                           AND last_review IS NOT NULL
                           AND state IN ('learning', 'relearning')
                           AND due <= ?;"
                        (list learning-cutoff)))
         (new (mnemodeck-db--count
               "SELECT COUNT(*) FROM cards WHERE archived_at IS NULL AND last_review IS NULL;"
               nil)))
    (list :reviewed reviewed :due-review due-review :due-learning due-learning :new new)))

;; Calendar queries

(defun mnemodeck-db--due-counts-by-date (day-start cutoff)
  "Return due-card counts grouped by date.

DAY-START and CUTOFF are time values that bound the query.

Return a plist with keys:
- :rows    list of (DATE-STRING COUNT) rows
- :overdue count of cards due before DAY-START."
  (let* ((conn (mnemodeck-db--ensure))
         (day-start-ts (mnemodeck--time->fsrs-timestamp day-start))
         (cutoff-ts (mnemodeck--time->fsrs-timestamp cutoff))
         (offset (format "%+d hours"
                         (- (mnemodeck--clamp mnemodeck-day-rollover-hour 0 23))))
         (rows (sqlite-select
                conn
                "SELECT date(due, 'localtime', ?) AS due_date, COUNT(*)
                 FROM cards
                 WHERE archived_at IS NULL
                   AND last_review IS NOT NULL
                   AND due >= ?
                   AND due < ?
                 GROUP BY due_date;"
                (list offset day-start-ts cutoff-ts)))
         (overdue-row
          (car (sqlite-select
                conn
                "SELECT COUNT(*) FROM cards
                 WHERE archived_at IS NULL
                   AND last_review IS NOT NULL
                   AND due < ?;"
                (list day-start-ts))))
         (overdue-count (if overdue-row (car overdue-row) 0)))
    (list :rows rows :overdue overdue-count)))

;; JSON export and import

(defun mnemodeck-db--timestamp-utc (&optional time)
  "Return TIME formatted as a UTC timestamp for filenames."
  (format-time-string "%Y%m%dT%H%M%SZ" (or time (current-time)) "UTC0"))

(defun mnemodeck-db--export-default-file ()
  "Return the default JSON export file path."
  (expand-file-name
   (format "mnemodeck-export-%s.json"
           (mnemodeck-db--timestamp-utc))
   mnemodeck-directory))

(defun mnemodeck-db--json-alist-get (record key)
  "Return KEY value from RECORD alist, accepting symbol or string keys."
  (or (alist-get key record nil nil #'equal)
      (alist-get (symbol-name key) record nil nil #'equal)))

(defun mnemodeck-db--import-record->card (record)
  "Convert JSON RECORD alist to (WORD META ARCHIVED-AT)."
  (unless (listp record)
    (error "Invalid JSON record: expected object, got %S" record))
  (let* ((now (fsrs-now))
         (word (mnemodeck-db--normalize-word
                (mnemodeck-db--json-alist-get record 'word)))
         (state (or (mnemodeck--normalize-fsrs-state
                     (mnemodeck-db--json-alist-get record 'state))
                    :learning))
         (step-raw (mnemodeck-db--json-alist-get record 'step))
         ;; Step is meaningful for learning/relearning.
         ;; Keep review cards at nil when step is missing.
         (step (if (numberp step-raw)
                   step-raw
                 (if (eq state :review) nil 0)))
         (meta (make-mnemodeck-card-meta
                :added-date (or (mnemodeck-db--json-alist-get record 'added_date) now)
                :last-review (mnemodeck-db--json-alist-get record 'last_review)
                :due (or (mnemodeck-db--json-alist-get record 'due) now)
                :state state
                :step step
                :stability (mnemodeck-db--json-alist-get record 'stability)
                :difficulty (mnemodeck-db--json-alist-get record 'difficulty)
                :hint (mnemodeck-db--normalize-hint
                       (mnemodeck-db--json-alist-get record 'hint))))
         (archived-at (mnemodeck-db--json-alist-get record 'archived_at)))
    (list word meta archived-at)))

(defun mnemodeck-db--import-read-conflict-choice (word)
  "Prompt conflict action for WORD.
Return (CURRENT-ACTION . GLOBAL-ACTION), where each action is
`:skip' or `:overwrite'.  GLOBAL-ACTION is non-nil only when user
confirms an \"all\" behavior."
  (let ((global-choice nil)
        (resolved nil))
    (while (not resolved)
      (let ((choice (read-char-choice
                     (format "Word \"%s\" exists: [s]kip, [o]verwrite, skip [a]ll, overwrite [A]ll: "
                             word)
                     '(?s ?o ?a ?A))))
        (pcase choice
          (?s
           (setq resolved :skip))
          (?o
           (setq resolved :overwrite))
          (?a
           (if (yes-or-no-p "Apply \"skip all\" for remaining conflicts? ")
               (setq global-choice :skip resolved :skip)
             (message "Canceled \"skip all\"; choose for current word.")))
          (?A
           (if (yes-or-no-p "Apply \"overwrite all\" for remaining conflicts? ")
               (setq global-choice :overwrite resolved :overwrite)
             (message "Canceled \"overwrite all\"; choose for current word."))))))
    (cons resolved global-choice)))

(defun mnemodeck-db--apply-import-card (word meta archived-at)
  "Upsert WORD with META, then apply ARCHIVED-AT flag."
  (mnemodeck-db--upsert-card word meta)
  (if archived-at
      (mnemodeck-db--archive-card word archived-at)
    (mnemodeck-db--unarchive-card word)))

(defun mnemodeck-db--import-json-file (file)
  "Import cards from JSON FILE.
Return a plist with :added, :overwritten, and :skipped."
  (let ((global-conflict-action nil)
        (added 0)
        (overwritten 0)
        (skipped 0))
    (unless (file-exists-p file)
      (user-error "Import file does not exist: %s" file))
    (with-temp-buffer
      (insert-file-contents file)
      (let ((records (json-parse-buffer :object-type 'alist
                                        :array-type 'list
                                        :null-object nil
                                        :false-object nil)))
        (unless (listp records)
          (error "Import JSON must be an array of card objects"))
        (dolist (record records)
          (pcase-let ((`(,word ,meta ,archived-at)
                       (mnemodeck-db--import-record->card record)))
            (if (mnemodeck-db--select-card word)
                (let ((action (or global-conflict-action
                                  (let ((decision (mnemodeck-db--import-read-conflict-choice word)))
                                    (setq global-conflict-action (or (cdr decision) global-conflict-action))
                                    (car decision)))))
                  (pcase action
                    (:skip
                     (cl-incf skipped))
                    (:overwrite
                     (mnemodeck-db--apply-import-card word meta archived-at)
                     (cl-incf overwritten))
                    (_
                     (error "Unknown import action: %S" action))))
              (mnemodeck-db--apply-import-card word meta archived-at)
              (cl-incf added))))))
    (list :added added :overwritten overwritten :skipped skipped)))

;;;###autoload
(defun mnemodeck-db-export-json (&optional file)
  "Export all cards to JSON FILE.
When called interactively, prompt for FILE and default to a timestamped
file under `mnemodeck-directory'."
  (interactive
   (let ((default (mnemodeck-db--export-default-file)))
     (list (read-file-name "Export JSON to: "
                           (file-name-directory default)
                           nil nil
                           (file-name-nondirectory default)))))
  (unless (file-exists-p mnemodeck-db-file)
    (user-error "No database file found; nothing to export"))

  (let* ((rows (sqlite-select
                (mnemodeck-db--ensure)
                "SELECT word, added_date, last_review, due, archived_at, state,
                        step, stability, difficulty, hint
                 FROM cards
                 ORDER BY added_date ASC, word ASC;"))
         (fields '(word added_date last_review due archived_at state
                        step stability difficulty hint))
         (payload (mapcar (lambda (row)
                            (cl-mapcar #'cons fields row))
                          rows))
         (json-encoding-pretty-print t)
         (json-encoding-default-indentation "  "))

    (make-directory (file-name-directory file) t)
    (let ((coding-system-for-write 'utf-8-unix))
      (with-temp-file file
        (insert (json-encode payload))))

    (when (called-interactively-p 'any)
      (message "Exported %d cards to %s" (length payload) file))))

;;;###autoload
(defun mnemodeck-db-import-json (&optional file)
  "Import cards from JSON FILE into the database.
When called interactively, prompt for FILE under `mnemodeck-directory'."
  (interactive
   (let ((default (expand-file-name "mnemodeck-import.json" mnemodeck-directory)))
     (list (read-file-name "Import JSON from: "
                           (file-name-directory default)
                           nil t
                           (file-name-nondirectory default)))))
  (let* ((file (or file (expand-file-name "mnemodeck-import.json" mnemodeck-directory)))
         (result (mnemodeck-db--import-json-file file)))
    (when (called-interactively-p 'any)
      (message "Import finished: %d added, %d overwritten, %d skipped"
               (plist-get result :added)
               (plist-get result :overwritten)
               (plist-get result :skipped)))
    result))

;; Backups

(defcustom mnemodeck-backup-directory
  (expand-file-name "backups" mnemodeck-directory)
  "Directory for MnemoDeck database backups."
  :type 'file
  :group 'mnemodeck-db)

(defcustom mnemodeck-backup-retain-days 30
  "Number of days to keep database backups."
  :type 'integer
  :group 'mnemodeck-db)

(defcustom mnemodeck-backup-prune-min-count 10
  "Minimum number of backups before pruning old ones."
  :type 'integer
  :group 'mnemodeck-db)

(defcustom mnemodeck-backup-prune-max-count nil
  "Prune when backup count exceeds this number.
When nil, this threshold is disabled."
  :type '(choice (const :tag "Disabled" nil) integer)
  :group 'mnemodeck-db)

(defcustom mnemodeck-backup-prune-confirm t
  "Whether to confirm before pruning backups."
  :type 'boolean
  :group 'mnemodeck-db)

(defcustom mnemodeck-backup-restore-completion-setup
  (lambda ()
    '((vertico-sort-override-function . identity)))
  "Function returning temporary completion bindings for backup restore.
When non-nil, it is called with no arguments inside
`mnemodeck-db-restore` and should return an alist of
\(SYMBOL . VALUE) pairs to bind dynamically around `completing-read`."
  :type 'function
  :group 'mnemodeck-db)

(defun mnemodeck-db--backup-target (backup-dir base timestamp)
  "Return a unique backup filename in BACKUP-DIR using BASE and TIMESTAMP."
  (let ((suffix 0))
    (cl-loop for candidate = (expand-file-name
                              (format "%s-%s%s.sqlite"
                                      base
                                      timestamp
                                      (if (zerop suffix) "" (format "-%d" suffix)))
                              backup-dir)
             while (file-exists-p candidate)
             do (cl-incf suffix)
             finally return candidate)))

(defun mnemodeck-db--backup-prune (backup-dir base)
  "Prune old backups in BACKUP-DIR for BASE when thresholds are met."
  ;; Only proceed if config values are valid.
  ;; This keeps us from pruning on misconfigured or zero-ish values.
  (when (and (integerp mnemodeck-backup-retain-days)
             (> mnemodeck-backup-retain-days 0)
             (integerp mnemodeck-backup-prune-min-count)
             (> mnemodeck-backup-prune-min-count 0))
    ;; Find all backup files for the current DB base name.
    (let* ((pattern (format "\\`%s-[0-9]\\{8\\}T[0-9]\\{6\\}Z\\.sqlite\\'"
                            (regexp-quote base)))
           (files (directory-files backup-dir t pattern))
           (count (length files))
           (max-exceeded (and (integerp mnemodeck-backup-prune-max-count)
                              (> mnemodeck-backup-prune-max-count 0)
                              (> count mnemodeck-backup-prune-max-count))))
      ;; Only prune if we have minimum count or exceeded maximum.
      (when (or (>= count mnemodeck-backup-prune-min-count) max-exceeded)
        ;; Calculate cutoff date and find old files.
        (let* ((cutoff-time (time-subtract (current-time)
                                           (days-to-time mnemodeck-backup-retain-days)))
               (files-by-age (sort (copy-sequence files)
                                   (lambda (a b)
                                     (time-less-p (file-attribute-modification-time (file-attributes a))
                                                  (file-attribute-modification-time (file-attributes b))))))
               (old-files (seq-filter (lambda (file)
                                        (time-less-p (file-attribute-modification-time (file-attributes file))
                                                     cutoff-time))
                                      files-by-age))
               (to-delete old-files))
          ;; If max exceeded, delete oldest files regardless of age.
          (when max-exceeded
            (let ((excess-count (- count mnemodeck-backup-prune-max-count)))
              (setq to-delete (seq-take files-by-age excess-count))))
          ;; Ask user (optional) and delete.
          (when (and to-delete
                     (or (not mnemodeck-backup-prune-confirm)
                         (yes-or-no-p (format "Prune %d backup(s) from %s? "
                                              (length to-delete)
                                              (abbreviate-file-name backup-dir)))))
            (dolist (file to-delete)
              ;; Always trash, just to be safe.
              (condition-case err
                  (delete-file file t)
                (error
                 (message "MnemoDeck: backup prune failed for %s: %s"
                          (abbreviate-file-name file)
                          (error-message-string err)))))))))))

(defun mnemodeck-db--backup ()
  "Create a database backup and prune old backups."
  (let* ((backup-dir (file-name-as-directory mnemodeck-backup-directory))
         (base (file-name-base mnemodeck-db-file))
         (timestamp (mnemodeck-db--timestamp-utc))
         (backup-file (mnemodeck-db--backup-target backup-dir base timestamp)))
    (make-directory backup-dir t)
    (copy-file mnemodeck-db-file backup-file t t t)
    (mnemodeck-db--backup-prune backup-dir base)))

(defun mnemodeck-db--backup-timestamp (file)
  "Return the backup timestamp for FILE, or nil if unavailable."
  (let ((pattern (format "\\`%s-\\([0-9]\\{8\\}T[0-9]\\{6\\}Z\\)"
                         (regexp-quote (file-name-base mnemodeck-db-file))))
        (filename (file-name-base file)))
    (when (string-match pattern filename)
      (condition-case nil
          (date-to-time (match-string 1 filename))
        (error nil)))))

(defun mnemodeck-db--backup-files ()
  "Return backup files sorted by newest timestamp first."
  (let* ((backup-dir (file-name-as-directory mnemodeck-backup-directory))
         (base (file-name-base mnemodeck-db-file))
         (pattern (format "\\`%s-[0-9]\\{8\\}T[0-9]\\{6\\}Z\\(-[0-9]+\\)?\\.sqlite\\'"
                          (regexp-quote base)))
         (files (when (file-directory-p backup-dir)
                  (directory-files backup-dir t pattern))))
    (sort (or files '())
          (lambda (a b)
            (let ((ta (or (mnemodeck-db--backup-timestamp a)
                          (file-attribute-modification-time (file-attributes a))))
                  (tb (or (mnemodeck-db--backup-timestamp b)
                          (file-attribute-modification-time (file-attributes b)))))
              (time-less-p tb ta))))))

(defun mnemodeck-db--backup-choice-label (file)
  "Return a display label for backup FILE."
  (format "%s (%s)"
          (file-name-base file)
          (format-time-string "%Y-%m-%d %H:%M:%S"
                              (file-attribute-modification-time
                               (file-attributes file)))))

(defun mnemodeck-db--read-backup-choice (choices default)
  "Read backup choice from CHOICES with DEFAULT using completion."
  (let* ((bindings (when (functionp mnemodeck-backup-restore-completion-setup)
                     (funcall mnemodeck-backup-restore-completion-setup)))
         (symbols (mapcar #'car bindings))
         (values (mapcar #'cdr bindings)))
    (if bindings
        (cl-progv symbols values
          (completing-read "Restore backup: " choices nil t nil nil default))
      (completing-read "Restore backup: " choices nil t nil nil default))))

;;;###autoload
(defun mnemodeck-db-backup ()
  "Create a database backup if the DB has changed since the last backup."
  (interactive)
  (if (not (file-exists-p mnemodeck-db-file))
      (when (called-interactively-p 'any)
        (message "No database file found; skipping backup"))
    (let* ((attrs (file-attributes mnemodeck-db-file))
           (mtime (file-attribute-modification-time attrs))
           (latest-backup (car (mnemodeck-db--backup-files)))
           (latest-mtime (when latest-backup
                           (file-attribute-modification-time
                            (file-attributes latest-backup)))))
      (if (equal mtime latest-mtime)
          (when (called-interactively-p 'any)
            (message "Backup not needed; database unchanged"))
        (mnemodeck-db--backup)
        (when (called-interactively-p 'any)
          (message "Backup created"))))))

;;;###autoload
(defun mnemodeck-db-restore ()
  "Restore the database from a selected backup file."
  (interactive)
  (let* ((files (mnemodeck-db--backup-files)))
    (unless files
      (user-error "No backups found in %s" mnemodeck-backup-directory))
    (let* ((choices (mapcar (lambda (file)
                              (cons (mnemodeck-db--backup-choice-label file) file))
                            files))
           (default (caar choices))
           (selection (mnemodeck-db--read-backup-choice choices default))
           (backup-file (cdr (assoc selection choices))))
      (unless backup-file
        (user-error "No backup selected"))
      (when (yes-or-no-p (format "Restore %s to %s? "
                                 (file-name-nondirectory backup-file)
                                 mnemodeck-db-file))
        (copy-file backup-file mnemodeck-db-file t t t)
        (message "Restored database from %s" (file-name-nondirectory backup-file))))))

;; Maintenance command

;;;###autoload
(defun mnemodeck-open-db-file ()
  "Open the SQLite database file.
Prefer `sqlite-mode-open-file' when available."
  (interactive)
  (if (fboundp 'sqlite-mode-open-file)
      (sqlite-mode-open-file mnemodeck-db-file)
    (find-file mnemodeck-db-file)))

;; -----------------------------------------------------
;;; Deck and words
;; -----------------------------------------------------

(defcustom mnemodeck-add-and-refresh t
  "When non-nil, re-adding an unreviewed word resets its timestamps."
  :type 'boolean
  :group 'mnemodeck)

(defvar mnemodeck-current-word nil
  "The current word being reviewed.")

(defvar mnemodeck-last-added-word nil
  "Most recently added word outside review.")

(defvar mnemodeck-add-card-batch-buffer-name "*MnemoDeck Batch Add*"
  "Buffer name used for batch card entry.")

(defvar mnemodeck-due-words nil
  "List of words that are due for review.")

(defvar-local mnemodeck-add-card-batch--on-confirm nil
  "Function called after batch add confirmation.
Receives a list of added words.")

(defvar-local mnemodeck-add-card-batch--on-cancel nil
  "Function called after batch add cancellation.")

(defvar mnemodeck-add-card-batch-mode-map
  (define-keymap
    "C-c C-c" #'mnemodeck-add-card-batch-confirm
    "C-c C-k" #'mnemodeck-add-card-batch-cancel)
  "Keymap for `mnemodeck-add-card-batch-mode'.")

(define-derived-mode mnemodeck-add-card-batch-mode text-mode "MnemoDeck-Batch"
  "Major mode for editing batch word entries.")

;; Helpers

(defun mnemodeck--refresh-counter ()
  "Refresh counters from database state."
  (setq mnemodeck--counter (mnemodeck-db--counts)))

(defun mnemodeck--refresh-due-words ()
  "Refresh due words list if review session is active."
  (setq mnemodeck-due-words (mnemodeck-db--select-due-words)))

(defun mnemodeck--load-card-meta (word)
  "Return card metadata for WORD, or nil if not found."
  (let ((row (mnemodeck-db--select-card word)))
    (when row
      (mnemodeck-db--row->card-meta row))))

(defun mnemodeck--require-current-word (action)
  "Return the current word, or report ACTION and return nil.
ACTION should be a verb phrase used in the fallback message."
  (when (not mnemodeck-current-word)
    (user-error "No word to %s" action))
  mnemodeck-current-word)

(defun mnemodeck--resolve-word (word &optional prompt)
  "Return WORD if non-nil, otherwise resolve from context.
If region is active, use its trimmed text.  Otherwise, use the current
review word in review mode, the word at the current edit line in edit
mode, or prompt with a default from word at point.  PROMPT is used when
a minibuffer prompt is needed."
  (if word
      word
    (let ((region-text (when (use-region-p)
                         (string-trim
                          (buffer-substring-no-properties
                           (region-beginning)
                           (region-end))))))
      (cond
       ((use-region-p)
        (if (string-empty-p region-text)
            (user-error "Selected text is empty")
          region-text))
       ((derived-mode-p 'mnemodeck-review-mode)
        (mnemodeck--require-current-word "use"))
       ((derived-mode-p 'mnemodeck-edit-mode)
        (or (tabulated-list-get-id)
            (user-error "No word on this line")))
       (t
        (let* ((default (thing-at-point 'word t))
               (input (read-string (or prompt "Word: ") default))
               (trimmed (string-trim input)))
          (if (string-empty-p trimmed)
              (user-error "Word cannot be empty")
            trimmed)))))))

(defun mnemodeck--require-card (word)
  "Return the card row for WORD, or signal a user error."
  (or (mnemodeck-db--select-card word)
      (user-error "No card found for \"%s\"" word)))

(defun mnemodeck--require-card-hint (word)
  "Get the hint for WORD's card."
  (let ((meta (mnemodeck--load-card-meta word)))
    (unless meta
      (user-error "No card found for \"%s\"" word))
    (mnemodeck-card-meta-hint meta)))

(defun mnemodeck--current-card-hint ()
  "Return the hint for the current review word, if any."
  (when mnemodeck-current-word
    (mnemodeck--require-card-hint mnemodeck-current-word)))

(defun mnemodeck--bind-keys (keymap bindings)
  "Bind BINDINGS (KEY . COMMAND) into KEYMAP."
  (dolist (binding bindings)
    (keymap-set keymap (car binding) (cdr binding))))

(defun mnemodeck--parse-iso-date (date-string)
  "Parse DATE-STRING as ISO 8601 date to internal time format."
  (when date-string
    (if (fboundp 'parse-iso8601-time-string)
        (parse-iso8601-time-string date-string)
      (encode-time (parse-time-string date-string)))))

;; Core editing functions

(defun mnemodeck-rate-card (word grade)
  "Update the card for WORD with review GRADE (1-4)."
  (let* ((row (mnemodeck--require-card word))
         (meta (mnemodeck-db--row->card-meta row))
         (updated (mnemodeck--update-card-with-grade word meta grade)))
    (mnemodeck-db--upsert-card word updated)
    (mnemodeck--refresh-counter)))

(defun mnemodeck-rename-word (old-word new-word)
  "Rename OLD-WORD to NEW-WORD and return the normalized new value."
  (let ((new-word (mnemodeck-db--update-word old-word new-word)))
    (when (and mnemodeck-current-word (string-equal old-word mnemodeck-current-word))
      (setq mnemodeck-current-word new-word))
    (when (and mnemodeck-last-added-word (string-equal old-word mnemodeck-last-added-word))
      (setq mnemodeck-last-added-word new-word))
    (when mnemodeck-due-words
      (setq mnemodeck-due-words
            (cl-substitute new-word old-word mnemodeck-due-words :test #'string=)))
    new-word))

(defun mnemodeck-update-card-hint (word hint)
  "Update the card for WORD with HINT."
  (mnemodeck--require-card word)
  (mnemodeck-db--update-hint word hint))

(defun mnemodeck-delete-card (word)
  "Delete the card for WORD from the deck."
  (mnemodeck-db--delete-card word)
  (when mnemodeck-due-words
    (setq mnemodeck-due-words (delete word mnemodeck-due-words)))
  (mnemodeck--refresh-counter))

(defun mnemodeck-archive-card (word)
  "Archive the card for WORD without deleting it."
  (mnemodeck-db--archive-card word (fsrs-now))
  (when mnemodeck-due-words
    (setq mnemodeck-due-words (delete word mnemodeck-due-words)))
  (mnemodeck--refresh-counter))

(defun mnemodeck-unarchive-card (word)
  "Unarchive the card for WORD and return it to the active deck."
  (mnemodeck-db--unarchive-card word)
  ;; No need to refresh `mnemodeck-due-words' here
  (mnemodeck--refresh-counter))

(defun mnemodeck-prompt-edit-card-fields (word &optional edit-word edit-hint)
  "Edit WORD fields based on EDIT-WORD and EDIT-HINT flags.
Return the updated word."
  (let ((row (mnemodeck--require-card word)))
    (pcase-let ((`(,_word ,_added ,_last ,_due ,_state ,_step ,_stability ,_difficulty ,hint) row))
      (when edit-word
        (let ((new-word (read-string (format "Word (%s): " word) word)))
          (unless (string-equal new-word word)
            (mnemodeck-rename-word word new-word)
            (setq word new-word))))
      (when edit-hint
        (let ((new-hint (read-string (format "Hint (%s): " word) (or hint ""))))
          (unless (string-equal new-hint (or hint ""))
            (mnemodeck-update-card-hint word new-hint))))
      word)))

;; Add-card flow

(defun mnemodeck--add-hint-precheck ()
  "Get the target word for adding hint, or signal an error if none."
  (or mnemodeck-last-added-word
      (user-error "No word to add a hint to")))

(defun mnemodeck--add-card (word)
  "Add WORD as a new card and return a status message."
  (setq word (mnemodeck-db--normalize-word word))
  (let* ((meta (mnemodeck--load-card-meta word))
         (is-new (and meta (mnemodeck-card-meta-is-new meta))))
    (setq mnemodeck-last-added-word word)
    (if (and meta
             (or (not is-new)
                 (not mnemodeck-add-and-refresh)))
        (format "Word \"%s\" already exists in the deck. " word)
      (let ((now (fsrs-now)))
        (unless meta
          (setq meta (make-mnemodeck-card-meta)))
        (setf (mnemodeck-card-meta-added-date meta) now)
        (setf (mnemodeck-card-meta-due meta) now)
        (mnemodeck-db--upsert-card word meta)
        ;; No need to refresh `mnemodeck-due-words' here
        (if is-new
            (format "Refreshed the added date of existing new word \"%s\". " word)
          (format "Added \"%s\" to the deck. " word))))))

(defun mnemodeck--add-card-prompt-next (status-msg)
  "Prompt for next action after adding a card with prepended STATUS-MSG."
  (let ((choice (read-char-choice
                 (concat status-msg "Add another card? (y/n, ENTER for yes, t for hint): ")
                 '(?y ?n ?\r ?t))))
    (pcase choice
      ((or ?y ?\r)
       (call-interactively #'mnemodeck-add-card))
      (?t
       (let ((status-msg (call-interactively #'mnemodeck-add-hint)))
         (when (memq (read-char-choice
                      (concat status-msg "Add another card? (y/n, ENTER for yes): ")
                      '(?y ?n ?\r))
                     '(?y ?\r))
           (call-interactively #'mnemodeck-add-card)))))))

;;;###autoload
(defun mnemodeck-add-card (word)
  "Add WORD as a new card.
After adding a card, prompts if you want to add another."
  (interactive "sWord to add: ")
  (let ((status-msg (mnemodeck--add-card word)))
    (when (called-interactively-p 'any)
      (mnemodeck--add-card-prompt-next status-msg))))

(defun mnemodeck-add-hint (hint)
  "Add HINT to the last added word."
  (interactive
   (let ((target (mnemodeck--add-hint-precheck)))
     (list
      (read-string (format "Hint for \"%s\": " target)
                   (mnemodeck--require-card-hint target)))))
  (let ((target (mnemodeck--add-hint-precheck)))
    (mnemodeck-update-card-hint target hint)
    (format "Updated the hint of \"%s\". " target)))

;; Batch add

(defun mnemodeck--batch-collect-words ()
  "Return a list of non-empty words from the current buffer."
  (split-string (buffer-string) "\n" t "[[:space:]]+"))

(defun mnemodeck-add-card-batch-confirm ()
  "Confirm batch import for the current buffer."
  (interactive)
  (let ((words (mnemodeck--batch-collect-words)))
    (dolist (word words)
      (mnemodeck-add-card word))
    (message "Imported %d words" (length words))
    (when (functionp mnemodeck-add-card-batch--on-confirm)
      (funcall mnemodeck-add-card-batch--on-confirm words)))
  (kill-buffer (current-buffer)))

(defun mnemodeck-add-card-batch-cancel ()
  "Cancel batch import and close the buffer."
  (interactive)
  (when (functionp mnemodeck-add-card-batch--on-cancel)
    (funcall mnemodeck-add-card-batch--on-cancel))
  (kill-buffer (current-buffer))
  (message "Batch import canceled"))

;;;###autoload
(defun mnemodeck-add-card-batch (&optional words &rest options)
  "Open a buffer for batch word entry.
WORDS, when non-nil, are inserted one per line.
OPTIONS is a plist supporting :buffer-name, :title, :on-confirm, :on-cancel,
and :message-prefix."
  (interactive)
  (let ((buffer (get-buffer-create
                 (or (plist-get options :buffer-name)
                     mnemodeck-add-card-batch-buffer-name))))
    (with-current-buffer buffer
      (mnemodeck-add-card-batch-mode)
      (setq header-line-format (or (plist-get options :title) "MnemoDeck Batch Add")
            mnemodeck-add-card-batch--on-confirm (plist-get options :on-confirm)
            mnemodeck-add-card-batch--on-cancel (plist-get options :on-cancel))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (when words
          (insert (string-join words "\n") "\n")))
      (goto-char (point-min)))
    (pop-to-buffer buffer)
    (message "%sType %s to confirm, %s to cancel."
             (if (string-empty-p (or (plist-get options :message-prefix) ""))
                 ""
               (concat (plist-get options :message-prefix) " "))
             (substitute-command-keys "\\[mnemodeck-add-card-batch-confirm]")
             (substitute-command-keys "\\[mnemodeck-add-card-batch-cancel]"))))

;; -----------------------------------------------------
;;; Edit
;; -----------------------------------------------------

(defgroup mnemodeck-edit nil
  "Edit mode for MnemoDeck."
  :group 'mnemodeck)

;; Faces

(defface mnemodeck-edit-word-face
  `((t :foreground ,(face-attribute 'ansi-color-red :foreground)
       :weight bold))
  "Face for displaying the word in edit lists."
  :group 'mnemodeck-edit)

(defface mnemodeck-edit-word-archived-face
  `((t :foreground ,(face-attribute 'ansi-color-cyan :foreground)
       :weight bold))
  "Face for displaying archived words in edit lists."
  :group 'mnemodeck-edit)

(defface mnemodeck-edit-hint-face
  `((t :inherit shadow))
  "Face for displaying the hint in edit lists."
  :group 'mnemodeck-edit)

(defface mnemodeck-edit-added-face
  `((t :foreground ,(face-attribute 'ansi-color-bright-blue :foreground)))
  "Face for displaying added timestamps in edit lists."
  :group 'mnemodeck-edit)

(defface mnemodeck-edit-last-review-face
  `((t :foreground ,(face-attribute 'ansi-color-bright-cyan :foreground)))
  "Face for displaying last review timestamps in edit lists."
  :group 'mnemodeck-edit)

(defface mnemodeck-edit-due-face
  `((t :foreground ,(face-attribute 'ansi-color-bright-green :foreground)))
  "Face for displaying due timestamps in edit lists."
  :group 'mnemodeck-edit)

(defface mnemodeck-edit-state-face
  `((t :foreground ,(face-attribute 'ansi-color-magenta :foreground)))
  "Face for displaying state values in edit lists."
  :group 'mnemodeck-edit)

(defface mnemodeck-edit-stability-face
  `((t :foreground ,(face-attribute 'ansi-color-green :foreground)))
  "Face for displaying stability values in edit lists."
  :group 'mnemodeck-edit)

(defface mnemodeck-edit-difficulty-face
  `((t :foreground ,(face-attribute 'ansi-color-yellow :foreground)))
  "Face for displaying difficulty values in edit lists."
  :group 'mnemodeck-edit)

(defface mnemodeck-edit-mark-face
  '((((background dark)) (:background "DarkGoldenrod4"))
    (t (:background "LightYellow1")))
  "Face for marked rows in the edit table."
  :group 'mnemodeck-edit)

;; Hooks

(defcustom mnemodeck-edit-start-hook nil
  "Hook run when mnemodeck edit session starts."
  :type 'hook
  :group 'mnemodeck-edit)

(defcustom mnemodeck-edit-quit-hook nil
  "Hook run when mnemodeck edit session quits."
  :type 'hook
  :group 'mnemodeck-edit)

;; Internal

(defvar mnemodeck-edit-buffer-name "*MnemoDeck Edit*"
  "Name of the buffer used for card editing.")

(defvar mnemodeck-edit--marked (make-hash-table :test 'equal)
  "Hash table of marked words in the edit view.")

(defvar mnemodeck-edit--mark-overlays (make-hash-table :test 'equal)
  "Hash table of word -> overlay for marked rows.")

(defvar mnemodeck-edit--filter 'all
  "Current filter for the edit table.
One of: all, review, learning, archived.")

(defconst mnemodeck-edit--columns
  '("Word" "Hint" "Added" "Last Review" "Due" "State" "Stability" "Difficulty")
  "Column names for the edit table.")

(defconst mnemodeck-edit--numeric-columns
  '("Stability" "Difficulty")
  "Columns that should be sorted numerically.")

(defconst mnemodeck-edit--time-sort-columns
  '("Added" "Last Review" "Due")
  "Columns that default to descending order when sorting.")

(defconst mnemodeck-edit--column-indices
  (let ((index 0)
        (table nil))
    (dolist (name mnemodeck-edit--columns (nreverse table))
      (push (cons name index) table)
      (setq index (1+ index))))
  "Alist mapping edit table column names to indices.")

;; Edit table formatting and sorting

(defun mnemodeck-edit--format-timestamp (timestamp)
  "Format TIMESTAMP for display in the edit table."
  (if (string-empty-p (or timestamp ""))
      ""
    (format-time-string "%Y-%m-%d %H:%M"
                        (mnemodeck--parse-iso-date timestamp))))

(defun mnemodeck-edit--entry-sort-string (entry column)
  "Return sortable string for ENTRY at COLUMN."
  (let* ((cell (aref (cadr entry) column))
         (value (or (get-text-property 0 'mnemodeck-sort-key cell)
                    (and (stringp cell) (substring-no-properties cell))
                    "")))
    (if (stringp value) value (format "%s" value))))

(defun mnemodeck-edit--entry-sort-number (entry column)
  "Return sortable number for ENTRY at COLUMN."
  (let* ((cell (aref (cadr entry) column))
         (value (or (get-text-property 0 'mnemodeck-sort-number cell)
                    (and (stringp cell) (substring-no-properties cell))
                    "")))
    (if (numberp value) value (string-to-number value))))

(defun mnemodeck-edit--restore-position (line win-line)
  "Restore point position in edit buffer using LINE and WIN-LINE."
  (let ((max-line (line-number-at-pos (point-max))))
    (goto-char (point-min))
    (forward-line (1- (min line max-line))))
  (when (and win-line (numberp win-line))
    (recenter win-line)))

(defmacro mnemodeck-edit--column-sorter (column)
  "Return a sorter lambda for COLUMN."
  `(lambda (a b)
     (let ((index (or (alist-get ,column mnemodeck-edit--column-indices nil nil #'string=)
                      (error "Unknown column: %s" ,column))))
       (if (member ,column mnemodeck-edit--numeric-columns)
           (< (mnemodeck-edit--entry-sort-number a index)
              (mnemodeck-edit--entry-sort-number b index))
         (string< (mnemodeck-edit--entry-sort-string a index)
                  (mnemodeck-edit--entry-sort-string b index))))))

(defmacro mnemodeck-edit--column-sort-command (column)
  "Return an interactive command to sort by COLUMN."
  `(lambda ()
     (interactive)
     ;; `tabulated-list-sort-key' is (COLUMN . DESC), where DESC is t/nil.
     (let* ((current (car tabulated-list-sort-key))
            ;; When sorting the same column, toggle direction.
            ;; Otherwise use the column's default direction.
            ;; Force DESC to be a strict boolean so sort key cdr is t/nil.
            (descending (if (equal current ,column)
                            (not (cdr tabulated-list-sort-key))  ; flip
                          (not (null (member ,column mnemodeck-edit--time-sort-columns))))))  ; to bool
       ;; Update the global sort key and immediately redraw the table.
       (setq tabulated-list-sort-key
             (cons ,column
                   descending))
       (mnemodeck-edit-refresh)
       ;; Report the selected column and direction for quick feedback.
       (message "Sort: %s (%s)"
                ,column
                (if descending "descending" "ascending")))))

;; Lightweight ratings from the edit table

(defun mnemodeck-edit--ensure-not-current (words)
  "Signal an error if WORDS include the current review word.
WORDS can be a single word string or a list of words."
  (let ((current mnemodeck-current-word))
    (when (and current
               (if (listp words)
                   (seq-find (lambda (word)
                               (string-equal word current))
                             words)
                 (string-equal words current)))
      (user-error "Current review word \"%s\" can only be modified in review mode" current))))

(defun mnemodeck-edit-rate-card ()
  "Rate the card at point, regardless of its current state."
  (interactive)
  (let* ((word (or (tabulated-list-get-id)
                   (user-error "No card on this line")))
         (line (line-number-at-pos))
         (win-line (count-screen-lines (window-start) (point)))
         (grade-options '((1 . "Again") (2 . "Hard") (3 . "Good") (4 . "Easy")))
         (prompt (concat (format "Rate \"%s\" " word)
                         (mapconcat (lambda (g)
                                      (format "[%d] %s" (car g) (cdr g)))
                                    grade-options " ")
                         ": "))
         (grade (- (read-char-choice prompt '(?1 ?2 ?3 ?4)) ?0))
         (label (alist-get grade grade-options "" nil #'=)))
    (mnemodeck-edit--ensure-not-current word)
    (when (eq mnemodeck-edit--filter 'archived)
      (mnemodeck-unarchive-card word))
    (mnemodeck-rate-card word grade)
    (mnemodeck-edit-refresh)
    (mnemodeck-edit--restore-position line win-line)
    (message "Rated \"%s\" as %s" word label)))

;; Edit table mode and commands

(defun mnemodeck-edit--entries ()
  "Return tabulated list entries for the edit buffer."
  (mapcar
   (lambda (row)
     (pcase-let ((`(,word ,added ,last-review ,due ,state ,_step ,stability ,difficulty ,hint) row))
       (let* ((state (mnemodeck--normalize-fsrs-state state))
              (word-face (if (eq mnemodeck-edit--filter 'archived)
                             'mnemodeck-edit-word-archived-face
                           'mnemodeck-edit-word-face))
              (hint (if hint
                        (replace-regexp-in-string "[\r\n]+" " ↵ " hint nil 'literal)
                      ""))
              (added (or added ""))
              (last-review (or last-review ""))
              (due (or due "")))
         (list word
               (vector
                (propertize word 'face word-face)
                (propertize hint 'face 'mnemodeck-edit-hint-face)
                (propertize (mnemodeck-edit--format-timestamp added)
                            'face 'mnemodeck-edit-added-face
                            'mnemodeck-sort-key added)
                (propertize (mnemodeck-edit--format-timestamp last-review)
                            'face 'mnemodeck-edit-last-review-face
                            'mnemodeck-sort-key last-review)
                (propertize (mnemodeck-edit--format-timestamp due)
                            'face 'mnemodeck-edit-due-face
                            'mnemodeck-sort-key due)
                (propertize (or (mnemodeck--fsrs-state-string state) "")
                            'face 'mnemodeck-edit-state-face)
                (propertize (if stability (format "%.3f" stability) "")
                            'face 'mnemodeck-edit-stability-face
                            'mnemodeck-sort-number (or stability 0))
                (propertize (if difficulty (format "%.3f" difficulty) "")
                            'face 'mnemodeck-edit-difficulty-face
                            'mnemodeck-sort-number (or difficulty 0)))))))
   (mnemodeck-db--select-cards mnemodeck-edit--filter tabulated-list-sort-key)))

(defun mnemodeck-edit-refresh ()
  "Refresh the card list buffer."
  (interactive)
  (setq tabulated-list-entries (delq nil (mnemodeck-edit--entries)))
  (tabulated-list-print t)
  (mnemodeck-edit--apply-marks))

(defun mnemodeck-edit--apply-marks ()
  "Apply marked-row faces to the edit buffer."
  (mnemodeck-edit--clear-mark-overlays)
  (save-excursion
    (goto-char (point-min))
    (forward-line 1)
    (while (not (eobp))
      (let ((word (tabulated-list-get-id)))
        (when (and word (gethash word mnemodeck-edit--marked))
          (mnemodeck-edit--add-mark-overlay word)))
      (forward-line 1))))

(defun mnemodeck-edit--marked-words ()
  "Return a list of marked words."
  (let (words)
    (maphash (lambda (word _value) (push word words)) mnemodeck-edit--marked)
    (nreverse words)))

(defun mnemodeck-edit--clear-marks ()
  "Clear all mark in the edit view."
  (clrhash mnemodeck-edit--marked))

(defun mnemodeck-edit--clear-mark-overlays ()
  "Remove all mark overlays in the edit view."
  (maphash (lambda (_word ov)
             (when (overlayp ov)
               (delete-overlay ov)))
           mnemodeck-edit--mark-overlays)
  (clrhash mnemodeck-edit--mark-overlays))

(defun mnemodeck-edit--add-mark-overlay (word)
  "Add a mark overlay for WORD on the current line."
  (let ((ov (make-overlay (line-beginning-position) (line-end-position))))
    (overlay-put ov 'face 'mnemodeck-edit-mark-face)
    (overlay-put ov 'mnemodeck-edit-mark t)
    (puthash word ov mnemodeck-edit--mark-overlays)))

(defun mnemodeck-edit-mark ()
  "Mark the card at point and move to the next line."
  (interactive)
  (let ((word (tabulated-list-get-id)))
    (unless word
      (user-error "No card on this line"))
    (puthash word t mnemodeck-edit--marked)
    (mnemodeck-edit--add-mark-overlay word)
    (forward-line 1)))

(defun mnemodeck-edit-unmark ()
  "Unmark the card at point and move to the next line."
  (interactive)
  (let ((word (tabulated-list-get-id)))
    (unless word
      (user-error "No card on this line"))
    (remhash word mnemodeck-edit--marked)
    (when-let ((ov (gethash word mnemodeck-edit--mark-overlays)))
      (delete-overlay ov)
      (remhash word mnemodeck-edit--mark-overlays))
    (forward-line 1)))

(defun mnemodeck-edit-unmark-all ()
  "Clear all mark in the edit view."
  (interactive)
  (mnemodeck-edit--clear-marks)
  (mnemodeck-edit--clear-mark-overlays)
  (message "Cleared all marks"))

(defun mnemodeck-edit-filter-review ()
  "Show only review cards in the edit table."
  (interactive)
  (setq mnemodeck-edit--filter 'review)
  (mnemodeck-edit-refresh)
  (message "Filter: review"))

(defun mnemodeck-edit-filter-learning ()
  "Show only learning cards in the edit table."
  (interactive)
  (setq mnemodeck-edit--filter 'learning)
  (mnemodeck-edit-refresh)
  (message "Filter: learning"))

(defun mnemodeck-edit-filter-toggle-archive ()
  "Toggle between archived cards and all cards in the edit table."
  (interactive)
  (setq mnemodeck-edit--filter
        (cond
         ((eq mnemodeck-edit--filter 'archived) 'all)
         ((eq mnemodeck-edit--filter 'all) 'archived)
         (t 'all)))
  (mnemodeck-edit-refresh)
  (message "Filter: %s" mnemodeck-edit--filter))

(defun mnemodeck-edit--edit-card-at-point (edit-word edit-hint)
  "Edit the card at point using EDIT-WORD and EDIT-HINT flags."
  (let ((word (tabulated-list-get-id))
        (line (line-number-at-pos))
        (win-line (count-screen-lines (window-start) (point))))
    (unless word
      (user-error "No card on this line"))
    (mnemodeck-edit--ensure-not-current word)
    (setq word (mnemodeck-prompt-edit-card-fields word edit-word edit-hint))
    (mnemodeck-edit-refresh)
    (mnemodeck-edit--restore-position line win-line)
    (message "Updated \"%s\"" word)))

(defun mnemodeck-edit-word ()
  "Edit the word at point."
  (interactive)
  (mnemodeck-edit--edit-card-at-point t nil))

(defun mnemodeck-edit-hint ()
  "Edit the hint at point."
  (interactive)
  (mnemodeck-edit--edit-card-at-point nil t))

(defun mnemodeck-edit-delete-card ()
  "Delete the card at point from the deck."
  (interactive)
  (let ((word (tabulated-list-get-id))
        (line (line-number-at-pos))
        (win-line (count-screen-lines (window-start) (point))))
    (unless word
      (user-error "No card on this line"))
    (mnemodeck-edit--ensure-not-current word)
    (when (yes-or-no-p (format "Delete \"%s\" from the deck? " word))
      (mnemodeck-delete-card word)
      (mnemodeck-edit-refresh)
      (mnemodeck-edit--restore-position line win-line)
      (message "Deleted \"%s\"" word))))

(defun mnemodeck-edit-delete ()
  "Delete marked cards, or the card at point."
  (interactive)
  (let ((marked (mnemodeck-edit--marked-words)))
    (if marked
        (progn
          (mnemodeck-edit--ensure-not-current marked)
          (when (yes-or-no-p (format "Delete %d marked cards? " (length marked)))
            (dolist (word marked)
              (mnemodeck-delete-card word))
            (mnemodeck-edit--clear-marks)
            (mnemodeck-edit-refresh)
            (message "Deleted %d cards" (length marked))))
      (mnemodeck-edit-delete-card))))

(defun mnemodeck-edit-archive-card ()
  "Archive the card at point."
  (interactive)
  (let ((word (tabulated-list-get-id))
        (line (line-number-at-pos))
        (win-line (count-screen-lines (window-start) (point))))
    (unless word
      (user-error "No card on this line"))
    (mnemodeck-edit--ensure-not-current word)
    (when (yes-or-no-p (format "Archive \"%s\" from review? " word))
      (mnemodeck-archive-card word)
      (mnemodeck-edit-refresh)
      (mnemodeck-edit--restore-position line win-line)
      (message "Archived \"%s\"" word))))

(defun mnemodeck-edit-unarchive-card ()
  "Unarchive the card at point."
  (interactive)
  (let ((word (tabulated-list-get-id))
        (line (line-number-at-pos))
        (win-line (count-screen-lines (window-start) (point))))
    (unless word
      (user-error "No card on this line"))
    (mnemodeck-edit--ensure-not-current word)
    (mnemodeck-unarchive-card word)
    (mnemodeck-edit-refresh)
    (mnemodeck-edit--restore-position line win-line)
    (message "Unarchived \"%s\"" word)))

(defun mnemodeck-edit-archive ()
  "Archive or unarchive marked cards, or the card at point."
  (interactive)
  (let ((marked (mnemodeck-edit--marked-words)))
    (if marked
        (progn
          (mnemodeck-edit--ensure-not-current marked)
          (when (yes-or-no-p (format "Archive %d marked cards? " (length marked)))
            (dolist (word marked)
              (mnemodeck-archive-card word))
            (mnemodeck-edit--clear-marks)
            (mnemodeck-edit-refresh)
            (message "Archived %d cards" (length marked))))
      (if (eq mnemodeck-edit--filter 'archived)
          (mnemodeck-edit-unarchive-card)
        (mnemodeck-edit-archive-card)))))

;; Edit mode setup

;;;###autoload
(defun mnemodeck-edit ()
  "Open the card list for editing."
  (interactive)
  (run-hooks 'mnemodeck-edit-start-hook)
  (let ((buffer (get-buffer-create mnemodeck-edit-buffer-name)))
    (with-current-buffer buffer
      (mnemodeck-edit-mode)
      (mnemodeck-edit-refresh))
    (switch-to-buffer buffer)))

(defun mnemodeck-edit-quit ()
  "Quit the edit buffer."
  (interactive)
  (run-hooks 'mnemodeck-edit-quit-hook)
  (quit-window)
  (mnemodeck-db--disconnect-if-idle))

;; Backup
(add-hook 'mnemodeck-edit-start-hook #'mnemodeck-db-backup)
(add-hook 'mnemodeck-edit-quit-hook #'mnemodeck-db-backup)

(defvar mnemodeck-edit-mode-map
  (define-keymap
    :parent tabulated-list-mode-map
    "e" #'mnemodeck-edit-word
    "t" #'mnemodeck-edit-hint
    "D" #'mnemodeck-edit-delete
    "/ r" #'mnemodeck-edit-filter-review
    "/ l" #'mnemodeck-edit-filter-learning
    "/ a" #'mnemodeck-edit-filter-toggle-archive
    "; w" (mnemodeck-edit--column-sort-command "Word")
    "; a" (mnemodeck-edit--column-sort-command "Added")
    "; l" (mnemodeck-edit--column-sort-command "Last Review")
    "; d" (mnemodeck-edit--column-sort-command "Due")
    "; s" (mnemodeck-edit--column-sort-command "Stability")
    "; f" (mnemodeck-edit--column-sort-command "Difficulty")
    "R" #'mnemodeck-edit-rate-card
    "A" #'mnemodeck-edit-archive
    "m" #'mnemodeck-edit-mark
    "u" #'mnemodeck-edit-unmark
    "U" #'mnemodeck-edit-unmark-all
    "<remap> <tabulated-list-sort>" #'mnemodeck-edit-refresh
    "g" #'mnemodeck-edit-refresh
    "+" #'mnemodeck-add-card
    "q" #'mnemodeck-edit-quit)
  "Keymap for `mnemodeck-edit-mode'.")

(define-derived-mode mnemodeck-edit-mode tabulated-list-mode "MnemoDeck-Edit"
  "Major mode for listing and editing MnemoDeck cards."
  (setq tabulated-list-format
        (vector
         (list "Word" 24 (mnemodeck-edit--column-sorter "Word"))
         (list "Hint" 40 t)
         (list "Added" 20 (mnemodeck-edit--column-sorter "Added"))
         (list "Last Review" 20 (mnemodeck-edit--column-sorter "Last Review"))
         (list "Due" 20 (mnemodeck-edit--column-sorter "Due"))
         (list "State" 10 t)
         (list "Stability" 10 (mnemodeck-edit--column-sorter "Stability"))
         (list "Difficulty" 10 (mnemodeck-edit--column-sorter "Difficulty"))))
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

;; -----------------------------------------------------
;;; Review
;; -----------------------------------------------------

(defgroup mnemodeck-review nil
  "Review mode for MnemoDeck."
  :group 'mnemodeck)

;; Parameters

(defcustom mnemodeck-review-daily-goal nil
  "Number of words to review per day.
When nil, disable daily goal tracking and related UI."
  :type '(choice (const :tag "Disable" nil) integer)
  :group 'mnemodeck-review)

(defcustom mnemodeck-review-hint-delay 1.5
  "Delay time in seconds for hint display."
  :type 'float
  :group 'mnemodeck-review)

(defcustom mnemodeck-review-hide-cursor t
  "Whether to hide the cursor in the review buffer."
  :type 'boolean
  :group 'mnemodeck-review)

(defcustom mnemodeck-review-enable-interval-labels t
  "Whether to show projected intervals next to rating options.
When non-nil, review mode shows compact labels such as (10m) for
Again/Hard/Good/Easy based on the current card state and FSRS prediction."
  :type 'boolean
  :group 'mnemodeck-review)

;; UI components

(defcustom mnemodeck-review-fixed-components
  '(mnemodeck-review-component-title
    mnemodeck-review-component-separator
    mnemodeck-review-component-counters
    mnemodeck-review-component-separator
    mnemodeck-review-component-daily-goal
    mnemodeck-review-component-separator
    mnemodeck-review-component-rates
    mnemodeck-review-component-separator
    mnemodeck-review-component-linebreak
    mnemodeck-review-component-word
    mnemodeck-review-component-linebreak
    mnemodeck-review-component-separator)
  "Components used for vertical centering and rendered before floating components."
  :type '(repeat function)
  :group 'mnemodeck-review)

(defcustom mnemodeck-review-floating-components
  '(mnemodeck-review-component-linebreak
    mnemodeck-review-component-hint)
  "Components rendered after the fixed block and excluded from centering."
  :type '(repeat function)
  :group 'mnemodeck-review)

(defcustom mnemodeck-review-daily-goal-progress-steps 50
  "Number of steps used in the daily goal progress bar."
  :type 'integer
  :group 'mnemodeck-review)

(defcustom mnemodeck-review-fill-column 50
  "Column beyond which automatic line-wrapping should happen."
  :type 'integer
  :group 'mnemodeck-review)

;; Faces

(defface mnemodeck-review-word-face
  `((((type graphic))
     :foreground ,(face-attribute 'ansi-color-red :foreground)
     :weight bold
     :height 1.5)
    (((type tty))
     :inherit default
     :foreground ,(face-attribute 'ansi-color-red :foreground)
     :weight bold
     :height 1.0))
  "Face for displaying the current word."
  :group 'mnemodeck-review)

(defface mnemodeck-review-status-new-face
  `((((type graphic))
     :foreground ,(face-attribute 'ansi-color-magenta :foreground)
     :weight bold
     :height 1.2)
    (((type tty))
     :inherit default
     :foreground ,(face-attribute 'ansi-color-magenta :foreground)
     :weight bold
     :height 1.0))
  "Face for displaying the `NEW WORD' status."
  :group 'mnemodeck-review)

(defface mnemodeck-review-status-review-face
  `((((type graphic))
     :foreground ,(face-attribute 'ansi-color-yellow :foreground)
     :weight bold
     :height 1.2)
    (((type tty))
     :inherit default
     :foreground ,(face-attribute 'ansi-color-yellow :foreground)
     :weight bold
     :height 1.0))
  "Face for displaying the `REVIEWING' status."
  :group 'mnemodeck-review)

(defface mnemodeck-review-counter-new-face
  `((t :foreground ,(face-attribute 'ansi-color-magenta :foreground)
       :weight bold :underline t))
  "Face for displaying the number of new words."
  :group 'mnemodeck-review)

(defface mnemodeck-review-counter-review-face
  `((t :foreground ,(face-attribute 'ansi-color-green :foreground)
       :weight bold :underline t))
  "Face for displaying the number of due words."
  :group 'mnemodeck-review)

(defface mnemodeck-review-counter-due-face
  `((t :foreground ,(face-attribute 'ansi-color-yellow :foreground)
       :weight bold :underline t))
  "Face for displaying the number of reviewed words today."
  :group 'mnemodeck-review)

(defface mnemodeck-review-status-goal-face
  `((t :foreground ,(face-attribute 'ansi-color-green :foreground)
       :weight bold))
  "Face for displaying the `DAILY GOAL REACHED' status."
  :group 'mnemodeck-review)

(defface mnemodeck-review-status-progress-face
  `((t :inherit default))
  "Face for displaying the daily goal progress bar."
  :group 'mnemodeck-review)

(defface mnemodeck-review-hint-placeholder-face
  `((t :inherit shadow :weight bold))
  "Face for displaying the hint placeholder."
  :group 'mnemodeck-review)

(defface mnemodeck-review-rating-interval-face
  `((t :inherit shadow))
  "Face for displaying rating interval hints."
  :group 'mnemodeck-review)

(defface mnemodeck-review-separator-face
  `((t :inherit shadow :weight bold))
  "Face for horizontal separators."
  :group 'mnemodeck-review)

;; Hooks

(defcustom mnemodeck-review-start-hook nil
  "Hook run when MnemoDeck review session starts."
  :type 'hook
  :group 'mnemodeck-review)

(defcustom mnemodeck-review-quit-hook nil
  "Hook run when MnemoDeck review quits/cleans up."
  :type 'hook
  :group 'mnemodeck-review)

(defcustom mnemodeck-review-next-card-hook nil
  "Hook run when MnemoDeck switches to the next word."
  :type 'hook
  :group 'mnemodeck-review)

(defcustom mnemodeck-review-daily-goal-reached-hook nil
  "Hook run once when the daily goal is reached."
  :type 'hook
  :group 'mnemodeck-review)

;; Internal

(defvar mnemodeck-review-buffer-name "*MnemoDeck Review*"
  "Name of the buffer used for reviews.")

(defvar mnemodeck-review--separator-width 0
  "Current separator width used by `mnemodeck-review-component-separator'.")

(defvar mnemodeck-review--state-display-hint nil
  "UI state: display hint.")

(defvar mnemodeck-review--hint-timer nil
  "Delay timer for hint display.")

;; Bring the definition up because it will be used by other functions down below.
(defvar mnemodeck-review-mode-map
  (define-keymap
    "1" #'mnemodeck-review-rate-again
    "2" #'mnemodeck-review-rate-hard
    "3" #'mnemodeck-review-rate-good
    "4" #'mnemodeck-review-rate-easy
    "q" #'mnemodeck-review-quit
    "n" #'mnemodeck-review-next-card
    "g" #'mnemodeck-review-refresh
    "D" #'mnemodeck-review-delete-card
    "e" #'mnemodeck-review-edit-word
    "t" #'mnemodeck-review-edit-hint)
  "Keymap for `mnemodeck-review-mode'.")

;; Format helpers

(defun mnemodeck--string-max-line-width (text)
  "Return the maximum line width for TEXT."
  (let ((max-width 0))
    (dolist (line (split-string text "\n") max-width)
      ;; Strip left padding so separators match the real content width.
      (let ((width (string-width (string-trim-left line))))
        (setq max-width (max max-width width))))))

(defun mnemodeck--string-pixel-size (text)
  "Return the pixel (WIDTH . HEIGHT) of TEXT in the current buffer context."
  (let ((fallback-height (* (length (split-string text "\n" t))
                            (frame-char-height))))
    (if (fboundp 'buffer-text-pixel-size)
        (let ((remap face-remapping-alist)
              (buffer-face (and (boundp 'buffer-face-mode-face)
                                buffer-face-mode-face)))
          (with-temp-buffer
            (let ((face-remapping-alist remap)
                  (inhibit-modification-hooks t))
              (when (boundp 'buffer-face-mode-face)
                (setq buffer-face-mode-face buffer-face))
              (insert text)
              (let ((size (buffer-text-pixel-size (current-buffer))))
                (cond
                 ((consp size) size)
                 ((numberp size) (cons size fallback-height))
                 (t (cons (string-pixel-width text) fallback-height)))))))
      (cons (string-pixel-width text) fallback-height))))

(defun mnemodeck--center-padding (lines)
  "Return a padding string to center LINES as a block in the window.
The lines in the block are left-aligned within the centered block."
  (let* ((graphic-p (display-graphic-p))
         ;; Get the available width (pixels for GUI, columns for terminal)
         (body-width (if graphic-p
                         (let ((edges (window-inside-pixel-edges)))
                           (- (nth 2 edges) (nth 0 edges)))
                       (window-body-width)))
         ;; Find the widest line (in pixels or columns)
         (max-line-width
          (apply #'max
                 (mapcar
                  (if graphic-p
                      (lambda (text)
                        (car (mnemodeck--string-pixel-size text)))
                    #'string-width)
                  lines)))
         ;; Calculate padding needed to center the block
         (padding (max 0 (/ (- body-width max-line-width) 2))))
    ;; Return appropriate padding format
    (if graphic-p
        ;; GUI: use pixel space specification
        (propertize " " 'display `(space :width (,padding)))
      ;; Terminal: use actual spaces
      (make-string padding ?\s))))

(defun mnemodeck-center-text (text)
  "Center TEXT in the current window width.
Multi-line text is centered as a block, not per-line."
  (let* ((lines (split-string text "\n"))
         (padding (mnemodeck--center-padding lines)))
    (mapconcat (lambda (line)
                 (concat padding line))
               lines
               "\n")))

(defun mnemodeck-fill-and-center-text (text width)
  "Wrap TEXT to WIDTH and center each wrapped line as a block."
  (let ((lines (string-split text "\n")))
    (string-join (mapcar (lambda (line)
                           (mnemodeck-center-text
                            (string-fill (string-trim line) width)))
                         lines)
                 "\n")))

(defun mnemodeck--format-interval (seconds)
  "Return a human readable string for SECONDS."
  (cond
   ((< seconds 60) "<1m")
   ((< seconds 3600) (format "%dm" (floor (/ seconds 60))))
   ((< seconds 86400) (format "%dh" (floor (/ seconds 3600))))
   ((< seconds 2592000) (format "%dd" (floor (/ seconds 86400))))
   ((< seconds 31536000) (format "%dM" (floor (/ seconds 2592000))))
   (t (format "%dy" (floor (/ seconds 31536000))))))

;; Buffer rendering

(defun mnemodeck-review--collect-component-items (components &optional last-was-sep)
  "Return (ITEMS . LAST-WAS-SEP) for COMPONENTS.
ITEMS contains (TEXT . SEPARATOR) pairs.  Leading or consecutive separators
are skipped so rendering won't stack separator lines."
  (let (items)
    (dolist (fn components)
      (let ((text (funcall fn))
            (separator (eq fn 'mnemodeck-review-component-separator)))
        (when text
          (unless (and separator (or last-was-sep (null items)))
            (push (cons text separator) items)
            (setq last-was-sep separator)))))
    (cons (nreverse items) last-was-sep)))

(defun mnemodeck-review--render-component-items (items)
  "Render ITEMS into a single string with trailing newlines."
  (concat (string-join (mapcar #'car items) "\n") "\n"))

(defun mnemodeck-review--render-with-vertical-center (fixed-items floating-items)
  "Render FIXED-ITEMS and FLOATING-ITEMS with vertical centering."
  (let* ((fixed-text (mnemodeck-review--render-component-items fixed-items))
         (floating-text (mnemodeck-review--render-component-items floating-items)))
    (if (display-graphic-p)
        ;; In GUI, center using pixel measurements for more accurate alignment.
        (let* ((measure-text (string-trim-right fixed-text "\n+"))
               (fixed-h (cdr (mnemodeck--string-pixel-size measure-text)))
               (half-body-h (/ (window-body-height nil t) 2))
               (padding-pixels (max 0 (- half-body-h fixed-h)))
               (padding (if (> padding-pixels 0)
                            (concat (propertize " " 'display `(space :height (,padding-pixels)))
                                    "\n")
                          "")))
          (concat padding fixed-text floating-text))
      ;; In TTY, fall back to line counts for centering.
      (let* ((line-count (length (split-string fixed-text "\n" t)))
             (half-body-h (floor (window-body-height) 2))
             (top-n-lines (max 0 (- half-body-h line-count 0))))
        (concat (make-string top-n-lines ?\n) fixed-text floating-text)))))

(defun mnemodeck-review--render-components ()
  "Render fixed and floating components with vertical centering for fixed ones."
  (let* ((content-components (seq-remove (lambda (fn)
                                           (eq fn 'mnemodeck-review-component-separator))
                                         (append mnemodeck-review-fixed-components
                                                 mnemodeck-review-floating-components)))
         (content-texts (delq nil (mapcar #'funcall content-components))))
    (if (null content-texts)
        ""
      ;; Update separator width.
      ;; Width is based on content only so separators match visible text.
      (setq mnemodeck-review--separator-width
            (seq-reduce
             #'max
             (mapcar #'mnemodeck--string-max-line-width content-texts)
             0))
      (let* ((fixed-result (mnemodeck-review--collect-component-items mnemodeck-review-fixed-components))
             (fixed-items (car fixed-result))
             ;; Track if the fixed block ended with a separator so we
             ;; do not stack separators between fixed and floating blocks.
             (last-was-sep (cdr fixed-result))
             (floating-result (mnemodeck-review--collect-component-items mnemodeck-review-floating-components last-was-sep))
             (floating-items (car floating-result)))
        (mnemodeck-review--render-with-vertical-center fixed-items floating-items)))))

(defun mnemodeck-review--setup-buffer ()
  "Set up the review buffer."
  (let ((buffer (get-buffer-create mnemodeck-review-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (unless (derived-mode-p 'mnemodeck-review-mode)
          (mnemodeck-review-mode))))
    buffer))

(defun mnemodeck-review--hint-delay-enabled-p ()
  "Return non-nil when hint delay is enabled.
Hint delay is enabled when `mnemodeck-review-hint-delay' is a positive number."
  (and (numberp mnemodeck-review-hint-delay) (> mnemodeck-review-hint-delay 0)))

(defun mnemodeck-review--reset-ui-state ()
  "Reset UI state."
  (mnemodeck-review--cancel-hint-timer)
  (setq mnemodeck-review--state-display-hint
        (not (mnemodeck-review--hint-delay-enabled-p))))

(defun mnemodeck-review--clean-up ()
  "Clear transient review session state."
  (setq mnemodeck-current-word nil)
  (setq mnemodeck-last-added-word nil)
  (setq mnemodeck-due-words nil)
  (mnemodeck-review--reset-ui-state))

(defun mnemodeck-review--refresh-visible (&rest _args)
  "Refresh the review buffer if it is visible in a window."
  (when-let ((window (get-buffer-window mnemodeck-review-buffer-name 'visible)))
    (with-current-buffer (window-buffer window)
      (when (eq major-mode 'mnemodeck-review-mode)
        (mnemodeck-review--render-buffer t)))))

(defun mnemodeck-review--enable-resize-refresh ()
  "Enable refresh hooks for resize and text scaling."
  (add-hook 'window-size-change-functions #'mnemodeck-review--refresh-visible)
  (unless (advice-member-p #'mnemodeck-review--refresh-visible 'text-scale-adjust)
    (advice-add 'text-scale-adjust :after #'mnemodeck-review--refresh-visible))
  (unless (advice-member-p #'mnemodeck-review--refresh-visible 'text-scale-set)
    (advice-add 'text-scale-set :after #'mnemodeck-review--refresh-visible)))

(defun mnemodeck-review--disable-resize-refresh ()
  "Disable refresh hooks for resize and text scaling."
  (remove-hook 'window-size-change-functions #'mnemodeck-review--refresh-visible)
  (advice-remove 'text-scale-adjust #'mnemodeck-review--refresh-visible)
  (advice-remove 'text-scale-set #'mnemodeck-review--refresh-visible))

(defun mnemodeck-review--daily-goal-reached-p ()
  "Check if the daily review goal has been reached."
  (when mnemodeck-review-daily-goal
    (or (>= (plist-get mnemodeck--counter :reviewed)
            mnemodeck-review-daily-goal)
        (and (<= (+ (plist-get mnemodeck--counter :due-review)
                    (plist-get mnemodeck--counter :due-learning))
                 0)
             (<= (plist-get mnemodeck--counter :new) 0)))))

(defun mnemodeck-review--cancel-hint-timer ()
  "Cancel `mnemodeck-review--hint-timer'."
  (when mnemodeck-review--hint-timer
    (cancel-timer mnemodeck-review--hint-timer)
    (setq mnemodeck-review--hint-timer nil)))

(defun mnemodeck-review--start-hint-timer ()
  "Start `mnemodeck-review--hint-timer'."
  (when (and (not mnemodeck-review--hint-timer)
             (mnemodeck-review--hint-delay-enabled-p))
    (setq mnemodeck-review--hint-timer
          (run-at-time mnemodeck-review-hint-delay
                       nil
                       (lambda ()
                         (setq mnemodeck-review--state-display-hint t)
                         (mnemodeck-review--render-buffer t))))))

(defun mnemodeck-review--instruction-key-label (command)
  "Return a key label for COMMAND from `mnemodeck-review-mode-map'."
  (if (where-is-internal command (list mnemodeck-review-mode-map))
      (substitute-command-keys
       (format "\\<mnemodeck-review-mode-map>\\[%s]" command))
    ""))

(defun mnemodeck-review--instruction-interval-label (word meta grade)
  "Return a propertized interval label for WORD with META at GRADE.
When `mnemodeck-review-enable-interval-labels' is nil, return an empty string.
Otherwise, simulate one FSRS review with GRADE and format the predicted
interval as a compact label, for example (10m)."
  (if (not mnemodeck-review-enable-interval-labels)
      ""
    (let* ((scheduler (mnemodeck--get-fsrs-scheduler))
           (rating (mnemodeck--fsrs-rating-from-grade grade))
           (review-time (fsrs-now))
           (card (mnemodeck--card-meta->fsrs-card word meta))
           ;; Simulate the rating to show the interval before the user commits.
           (new-card (cl-nth-value 0
                                   (fsrs-scheduler-review-card
                                    scheduler card rating review-time))))
      (propertize
       (format " (%s)"
               (mnemodeck--format-interval
                (fsrs-timestamp-difference (fsrs-card-due new-card) review-time)))
       'face 'mnemodeck-review-rating-interval-face))))

(defun mnemodeck-review--separator (&optional length)
  "Return the styled separator line for instruction blocks.
When LENGTH is non-nil, use it as the separator width."
  (propertize (make-string (or length 49) ?─)
              'face 'mnemodeck-review-separator-face))

(defun mnemodeck-review-component-title ()
  "Return the centered title line for the review header."
  (mnemodeck-center-text
   (let ((meta (mnemodeck--load-card-meta mnemodeck-current-word)))
     (cond
      ((mnemodeck-card-meta-is-new meta)
       (propertize "NEW WORD" 'face 'mnemodeck-review-status-new-face))
      ((memq (mnemodeck-card-meta-state meta) '(:learning :relearning))
       (propertize "LEARNING" 'face 'mnemodeck-review-status-review-face))
      (t
       (propertize "REVIEWING" 'face 'mnemodeck-review-status-review-face))))))

(defun mnemodeck-review-component-counters ()
  "Return the counter block for the instructions."
  (let* ((n-reviewed (plist-get mnemodeck--counter :reviewed))
         (n-due-review (plist-get mnemodeck--counter :due-review))
         (n-due-learning (plist-get mnemodeck--counter :due-learning))
         (n-new (plist-get mnemodeck--counter :new)))
    (mnemodeck-center-text
     (format "%s reviewed / %s review due / %s learning due / %s new"
             (propertize (number-to-string n-reviewed) 'face 'mnemodeck-review-counter-review-face)
             (propertize (number-to-string n-due-review) 'face 'mnemodeck-review-counter-due-face)
             (propertize (number-to-string n-due-learning) 'face 'mnemodeck-review-counter-due-face)
             (propertize (number-to-string n-new) 'face 'mnemodeck-review-counter-new-face)))))

(defun mnemodeck-review-component-daily-goal ()
  "Insert the daily goal banner in the review buffer."
  (when (and mnemodeck-review-daily-goal (> mnemodeck-review-daily-goal 0))
    (if (mnemodeck-review--daily-goal-reached-p)
        (mnemodeck-center-text (propertize "DAILY GOAL REACHED" 'face 'mnemodeck-review-status-goal-face))
      (let* ((n-reviewed (plist-get mnemodeck--counter :reviewed))
             (goal-progress (/ (float n-reviewed) mnemodeck-review-daily-goal))
             (steps (max 0 mnemodeck-review-daily-goal-progress-steps))
             (n-clicks-completed (mnemodeck--clamp (round (* goal-progress steps)) 0 steps))
             (percentage (format "%.2f%%" (* 100 goal-progress)))
             (progress-bar (concat
                            (make-string n-clicks-completed ?█)
                            (make-string (- steps n-clicks-completed) ?░))))
        (concat
         (mnemodeck-center-text (propertize percentage 'face 'mnemodeck-review-status-progress-face))
         "\n"
         (mnemodeck-center-text (propertize progress-bar 'face 'mnemodeck-review-status-progress-face)))))))

(defun mnemodeck-review-component-rates ()
  "Return the options block for review ratings and commands.
Interval labels are included when
`mnemodeck-review-enable-interval-labels' is non-nil."
  (let* ((meta (mnemodeck--load-card-meta mnemodeck-current-word))
         (option-lines
          (list
           (concat (mnemodeck-review--instruction-key-label 'mnemodeck-review-rate-again)
                   " Again"
                   (mnemodeck-review--instruction-interval-label mnemodeck-current-word meta 1))
           ;; Add one extra space after "Hard", "Good", and "Easy" to make interval labels vertically aligned
           (concat (mnemodeck-review--instruction-key-label 'mnemodeck-review-rate-hard)
                   " Hard "
                   (mnemodeck-review--instruction-interval-label mnemodeck-current-word meta 2))
           (concat (mnemodeck-review--instruction-key-label 'mnemodeck-review-rate-good)
                   " Good "
                   (mnemodeck-review--instruction-interval-label mnemodeck-current-word meta 3))
           (concat (mnemodeck-review--instruction-key-label 'mnemodeck-review-rate-easy)
                   " Easy "
                   (mnemodeck-review--instruction-interval-label mnemodeck-current-word meta 4)))))
    (mnemodeck-center-text (string-join option-lines "\n"))))

(defun mnemodeck-review-component-separator ()
  "Return the separator line for the review components."
  (when (> mnemodeck-review--separator-width 0)
    (mnemodeck-center-text
     (mnemodeck-review--separator mnemodeck-review--separator-width))))

(defun mnemodeck-review-component-linebreak ()
  "Return an empty component that is kept to force a blank line."
  "")

(defun mnemodeck-review-component-word ()
  "Insert the current word in the review buffer."
  (let ((word (propertize mnemodeck-current-word 'face 'mnemodeck-review-word-face)))
    (mnemodeck-center-text word)))

(defun mnemodeck-review-component-hint ()
  "Insert the current word's hint in the review buffer."
  (let ((hint (mnemodeck--current-card-hint)))
    (when hint
      (if mnemodeck-review--state-display-hint
          (mnemodeck-fill-and-center-text hint mnemodeck-review-fill-column)
        (let ((hint-placeholder (propertize "[HINT]" 'face 'mnemodeck-review-hint-placeholder-face)))
          (mnemodeck-center-text hint-placeholder))))))

(defun mnemodeck-review--hide-cursor ()
  "Hide the cursor and hl-line in the review buffer window."
  (when (eq major-mode 'mnemodeck-review-mode)
    ;; Hide cursor
    (setq-local cursor-type nil)
    ;; Disable hl-line-mode for a cleaner UI
    (when (boundp 'hl-line-mode)
      (hl-line-mode -1))
    (when (boundp 'global-hl-line-mode)
      (setq-local global-hl-line-mode nil))))

(defun mnemodeck-review--render-buffer (&optional keep-position)
  "Render the review buffer.
When KEEP-POSITION is non-nil, preserve the window scroll and point."
  (with-current-buffer (get-buffer mnemodeck-review-buffer-name)
    (when mnemodeck-review-hide-cursor
      (mnemodeck-review--hide-cursor))
    (let* ((window (get-buffer-window (current-buffer) 0))
           (saved-point (when (and keep-position window) (window-point window)))
           (saved-start (when (and keep-position window) (window-start window)))
           (render (lambda ()
                     (let ((inhibit-read-only t))
                       (erase-buffer)
                       (insert (mnemodeck-review--render-components))))))
      ;; Render using the review window so centering uses its dimensions.
      (if window
          (with-selected-window window (funcall render))
        (funcall render))
      (if (mnemodeck--current-card-hint)
          (mnemodeck-review--start-hint-timer)
        (mnemodeck-review--cancel-hint-timer))
      (if (and keep-position window)
          (progn
            (set-window-point window (min (or saved-point (point-min)) (point-max)))
            (when saved-start
              (set-window-start window saved-start t)))
        (goto-char (point-min))))))

;; Review flow and rating commands

(defun mnemodeck-review-next-card ()
  "Review the next due card.
When current list is empty, re-check for due cards and continue if any exist."
  (interactive)
  (if (or mnemodeck-due-words (mnemodeck--refresh-due-words))
      (let ((word (pop mnemodeck-due-words)))
        (setq mnemodeck-current-word word)
        (mnemodeck-review--reset-ui-state)
        (run-hooks 'mnemodeck-review-next-card-hook)
        (mnemodeck-review--render-buffer))
    (mnemodeck-review-quit)))

(defun mnemodeck-review-quit ()
  "Quit MnemoDeck review."
  (interactive)
  (mnemodeck-review--clean-up)
  (when-let ((buffer (get-buffer mnemodeck-review-buffer-name)))
    (kill-buffer buffer))
  (run-hooks 'mnemodeck-review-quit-hook)
  (mnemodeck-db--disconnect-if-idle)
  (message "Review session finished"))

(defun mnemodeck-review--handle-grade (grade)
  "Handle a GRADE input and move on to the next word."
  (let ((word (mnemodeck--require-current-word "rate")))
    (let ((goal-was-reached (mnemodeck-review--daily-goal-reached-p)))
      (mnemodeck-rate-card word grade)
      (when (and (not goal-was-reached)
                 (mnemodeck-review--daily-goal-reached-p))
        (run-hooks 'mnemodeck-review-daily-goal-reached-hook)))
    (let ((rating-text (pcase grade
                         (1 "Again")
                         (2 "Hard")
                         (3 "Good")
                         (4 "Easy")
                         (_ "Unknown"))))
      (message "Rated \"%s\" as (%s)" word rating-text))
    (mnemodeck-review-next-card)))

(defun mnemodeck-review-rate-again ()
  "Rate the current word as `again'."
  (interactive)
  (mnemodeck-review--handle-grade 1))

(defun mnemodeck-review-rate-hard ()
  "Rate the current word as `hard'."
  (interactive)
  (mnemodeck-review--handle-grade 2))

(defun mnemodeck-review-rate-good ()
  "Rate the current word as `good'."
  (interactive)
  (mnemodeck-review--handle-grade 3))

(defun mnemodeck-review-rate-easy ()
  "Rate the current word as `easy'."
  (interactive)
  (mnemodeck-review--handle-grade 4))

(defun mnemodeck-review-refresh ()
  "Refresh the review window."
  (interactive)
  (when mnemodeck-current-word
    (let ((buffer (mnemodeck-review--setup-buffer)))
      (switch-to-buffer buffer)
      (mnemodeck-review--render-buffer))))

(defun mnemodeck-review--edit-card-fields (edit-word edit-hint)
  "Edit the current card using EDIT-WORD and EDIT-HINT flags."
  (unless mnemodeck-current-word
    (user-error "No current word to edit"))
  (setq mnemodeck-current-word
        (mnemodeck-prompt-edit-card-fields mnemodeck-current-word edit-word edit-hint))
  (when (eq major-mode 'mnemodeck-review-mode)
    (mnemodeck-review--render-buffer))
  (message "Updated \"%s\"" mnemodeck-current-word))

(defun mnemodeck-review-edit-word ()
  "Edit the current word."
  (interactive)
  (mnemodeck-review--edit-card-fields t nil))

(defun mnemodeck-review-edit-hint ()
  "Edit the current hint."
  (interactive)
  (mnemodeck-review--edit-card-fields nil t))

(defun mnemodeck-review-delete-card ()
  "Delete the current card from the deck."
  (interactive)
  (if (null mnemodeck-current-word)
      (message "No current word to delete")
    (when (yes-or-no-p (format "Are you sure you want to delete \"%s\" from the deck? " mnemodeck-current-word))
      (mnemodeck-delete-card mnemodeck-current-word)
      (message "Deleted \"%s\" from the deck." mnemodeck-current-word)
      (setq mnemodeck-current-word nil)
      (when (eq major-mode 'mnemodeck-review-mode)
        (mnemodeck-review-next-card)))))

;; Review mode setup

;;;###autoload
(defun mnemodeck-review ()
  "Start a review session."
  (interactive)
  (run-hooks 'mnemodeck-review-start-hook)
  (mnemodeck--refresh-due-words)
  (if (null mnemodeck-due-words)
      (message "No words to review")
    (let ((buffer (mnemodeck-review--setup-buffer)))
      (switch-to-buffer buffer)
      (mnemodeck--refresh-counter)
      (mnemodeck-review-next-card))))

;; Keep review UI in sync when a hint is added via the quick add flow.
;; This runs only in review buffers and only when the last-added word is
;; the current review word, so other contexts stay unaffected.
(defun mnemodeck-review--maybe-refresh-after-add-hint (&rest _)
  "Refresh the review buffer after adding a hint for the current word."
  (when (and (eq major-mode 'mnemodeck-review-mode)
             mnemodeck-current-word
             mnemodeck-last-added-word
             (string-equal mnemodeck-current-word mnemodeck-last-added-word))
    (mnemodeck-review--render-buffer t)))

;; Use advice to make the logic less entangled.
;; This refresh logic is solely for the review mode.
(advice-add 'mnemodeck-add-hint :after #'mnemodeck-review--maybe-refresh-after-add-hint)

;; Backup
(add-hook 'mnemodeck-review-start-hook #'mnemodeck-db-backup)
(add-hook 'mnemodeck-review-quit-hook #'mnemodeck-db-backup)
;; Auto-refresh
(add-hook 'mnemodeck-review-start-hook #'mnemodeck-review--enable-resize-refresh)
(add-hook 'mnemodeck-review-quit-hook #'mnemodeck-review--disable-resize-refresh)

(define-derived-mode mnemodeck-review-mode special-mode "MnemoDeck-Review"
  "Major mode for reviewing vocabulary with FSRS algorithm."
  (setq buffer-read-only t)
  (buffer-disable-undo))

;; -----------------------------------------------------
;;; Dictionary
;; -----------------------------------------------------

(defgroup mnemodeck-dictionary nil
  "Dictionary integration for MnemoDeck."
  :group 'mnemodeck)

(defcustom mnemodeck-dictionary-audio-cache-dir
  (expand-file-name "audio-cache" mnemodeck-directory)
  "Audio cache directory."
  :type 'file
  :group 'mnemodeck-dictionary)

(defcustom mnemodeck-lookup-providers
  '(("Google" . "https://www.google.com/search?q=define:%s")
    ("Merriam-Webster" . "https://www.merriam-webster.com/dictionary/%s")
    ("Oxford Learner's" . "https://www.oxfordlearnersdictionaries.com/definition/english/%s")
    ("Cambridge" . "https://dictionary.cambridge.org/dictionary/english/%s")
    ("Wiktionary" . "https://en.wiktionary.org/wiki/%s"))
  "Alist of word lookup providers.
Each entry is (NAME . URL), where URL expects a single %s for the word."
  :type '(alist :key-type string :value-type string)
  :group 'mnemodeck-dictionary)

(defcustom mnemodeck-lookup-default-provider "Google"
  "Default provider name used for word lookups."
  :type 'string
  :group 'mnemodeck-dictionary)

(defcustom mnemodeck-dictionary-define-function
  #'mnemodeck-dictionary-default-define-function
  "Function to define WORD and return a formatted string.
The function takes a WORD string and returns a propertized string for display.
Caching is handled by `mnemodeck-define'."
  :type 'function
  :group 'mnemodeck-dictionary)

(defcustom mnemodeck-dictionary-audio-function
  #'mnemodeck-dictionary-default-audio-function
  "Function to return a local audio file path for WORD.
The function should take a WORD string and return a local file path or nil.
Caching is handled by `mnemodeck-speak'."
  :type 'function
  :group 'mnemodeck-dictionary)

(defcustom mnemodeck-lookup-hook nil
  "Hook run after browser lookup is opened."
  :type 'hook
  :group 'mnemodeck-dictionary)

(defcustom mnemodeck-define-hook nil
  "Hook run after `mnemodeck-define' prepares definition content."
  :type 'hook
  :group 'mnemodeck-dictionary)

(defface mnemodeck-dictionary-word-face
  `((t :foreground ,(face-attribute 'ansi-color-red :foreground)
       :weight bold))
  "Face for displaying the word in dictionary buffer."
  :group 'mnemodeck-dictionary)

(defvar mnemodeck-dictionary-buffer-name "*MnemoDeck Definition*"
  "Name of the buffer used to display dictionary definitions.")

(defvar mnemodeck-dictionary-cache-definition (make-hash-table :test 'equal)
  "Cache for `mnemodeck-dictionary-define-function'.")

(defvar mnemodeck-dictionary-cache-audio (make-hash-table :test 'equal)
  "Cache for `mnemodeck-dictionary-audio-function'.")

;;;###autoload
(defun mnemodeck-lookup (word &optional provider)
  "Lookup WORD in the browser using PROVIDER.
PROVIDER should be a key in `mnemodeck-lookup-providers`.
When PROVIDER is nil, use `mnemodeck-lookup-default-provider`."
  (interactive (list nil nil))
  (let* ((word (mnemodeck--resolve-word word "Lookup word: "))
         (provider (or provider mnemodeck-lookup-default-provider))
         (url-template (cdr (assoc provider mnemodeck-lookup-providers))))
    (unless url-template
      (user-error "Unknown provider: %s" provider))
    (browse-url (format url-template (url-hexify-string word)))
    (run-hooks 'mnemodeck-lookup-hook)))

;;;###autoload
(defun mnemodeck-lookup-with-provider (word &optional provider)
  "Lookup WORD using a selected PROVIDER.
When WORD or PROVIDER is nil, prompt for them."
  (interactive (list nil nil))
  (let ((word (mnemodeck--resolve-word word "Lookup word: "))
        (provider (or provider
                      (completing-read "Lookup provider: "
                                       (mapcar #'car mnemodeck-lookup-providers)
                                       nil t))))
    (mnemodeck-lookup word provider)))

;; Dictionary API helpers

(define-derived-mode mnemodeck-dictionary-mode special-mode "MnemoDeck-Dictionary"
  "Major mode for MnemoDeck dictionary popups.")

(defun mnemodeck-dictionary--close-buffer ()
  "Close the dictionary buffer and window if present."
  (let ((buffer (get-buffer mnemodeck-dictionary-buffer-name)))
    (when buffer
      (when-let ((window (get-buffer-window buffer)))
        (quit-window t window))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defun mnemodeck-dictionary--display (content &optional focus-popup)
  "Display dictionary CONTENT in a dedicated buffer.
When FOCUS-POPUP is non-nil, focus the popup window; otherwise keep focus."
  (let ((buffer (get-buffer-create mnemodeck-dictionary-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert content)
        (goto-char (point-min))
        (mnemodeck-dictionary-mode)))
    (if focus-popup
        (pop-to-buffer buffer '(display-buffer-pop-up-window))
      (save-selected-window
        (display-buffer buffer '(display-buffer-pop-up-window))))
    buffer))

(defun mnemodeck-dictionary--fetch-definition (url &optional method headers body)
  "Fetch definition data from URL.
METHOD, HEADERS, and BODY override the request defaults.
Returns parsed JSON data or nil if request fails or JSON is invalid."
  (let* ((url-request-method (or method "GET"))
         (url-request-extra-headers headers)
         (url-request-data body)
         (buffer (condition-case err
                     (url-retrieve-synchronously url t nil 10)
                   (error
                    (message "Error fetching definition: %s" (error-message-string err))
                    nil))))
    (when buffer
      (with-current-buffer buffer
        (goto-char (point-min))
        ;; Skip HTTP headers before parsing the JSON body.
        (when (re-search-forward "^$" nil t)
          (let* ((json-string (buffer-substring-no-properties (point) (point-max)))
                 (parsed-data (mnemodeck--json-parse-safe json-string "Error parsing JSON")))
            (kill-buffer buffer)
            parsed-data))))))

(defun mnemodeck-dictionary--format-phonetics (phonetics)
  "Format PHONETICS data into a readable string."
  (when phonetics
    (let ((phonetic-texts (delq nil (mapcar (lambda (p) (alist-get 'text p)) phonetics))))
      (when phonetic-texts
        (format "Pronunciation: %s\n" (string-join phonetic-texts ", "))))))

(defun mnemodeck-dictionary--format-meanings (meanings)
  "Format MEANINGS data into a readable string."
  (mapconcat
   (lambda (meaning)
     (let ((part-of-speech (alist-get 'partOfSpeech meaning))
           (definitions (alist-get 'definitions meaning)))
       (concat
        (format "\n%s:\n" (capitalize part-of-speech))
        (mapconcat
         (lambda (def)
           (let ((definition (alist-get 'definition def))
                 (example (alist-get 'example def)))
             (concat
              (format "  • %s" definition)
              (when example
                (format "\n    Example: \"%s\"" example)))))
         definitions
         "\n"))))
   meanings
   "\n"))

(defun mnemodeck-dictionary-default-define-function (word)
  "Return a formatted definition string for WORD using Dictionary API."
  (let* ((url (format "https://api.dictionaryapi.dev/api/v2/entries/en/%s"
                      (url-hexify-string word)))
         (result (mnemodeck-dictionary--fetch-definition url)))
    (cond
     ((not result)
      (format "No definition found for \"%s\"." word))
     ((and (listp result) (alist-get 'title result))
      ;; Some error payloads come back as JSON objects instead of vectors.
      (format "No definition found for \"%s\". Try browser lookup instead." word))
     ((not (vectorp result))
      (format "No definition found for \"%s\"." word))
     (t
      (let* ((entry (aref result 0))
             (word-text (alist-get 'word entry))
             (phonetics (alist-get 'phonetics entry))
             (meanings (alist-get 'meanings entry)))
        (concat
         (format "Word: %s\n"
                 (propertize word-text 'face 'mnemodeck-dictionary-word-face))
         (or (mnemodeck-dictionary--format-phonetics phonetics) "")
         (mnemodeck-dictionary--format-meanings meanings)))))))

(defun mnemodeck-dictionary-default-audio-function (word)
  "Return a local audio file path for WORD using Dictionary API."
  (when-let* ((url (format "https://api.dictionaryapi.dev/api/v2/entries/en/%s"
                           (url-hexify-string word)))
              (result (mnemodeck-dictionary--fetch-definition url))
              (entry (and (vectorp result) (> (length result) 0) (aref result 0)))
              (phonetics (alist-get 'phonetics entry))
              (audio-url (seq-find (lambda (phonetic)
                                     (let ((audio (alist-get 'audio phonetic)))
                                       (and audio (not (string-empty-p audio)))))
                                   phonetics))
              (audio-url (alist-get 'audio audio-url)))
    (make-directory mnemodeck-dictionary-audio-cache-dir t)
    (let* ((file-extension (or (file-name-extension audio-url) "mp3"))
           (hash (secure-hash 'sha1 audio-url))
           (target (expand-file-name (format "%s.%s" hash file-extension)
                                     mnemodeck-dictionary-audio-cache-dir)))
      (unless (file-exists-p target)
        (condition-case err
            (url-copy-file audio-url target t)
          (error
           (message "Failed to download audio: %s" (error-message-string err))
           (setq target nil))))
      (and (file-exists-p target) target))))

;;;###autoload
(defun mnemodeck-define (word &optional popup focus-popup)
  "Define WORD and return formatted result.
Ignores audio links and focuses on definitions and phonetics.
When POPUP is non-nil, show the definition in a popup buffer.
When FOCUS-POPUP is non-nil, focus the popup window; otherwise keep focus."
  (interactive (list nil nil nil))
  (let* ((word (mnemodeck--resolve-word word "Define word: "))
         (popup (or popup (called-interactively-p 'any)))
         ;; Always focus popup if its called interactively.
         ;; The popup window can be easily closed by pressing "q",
         (focus-popup (or (called-interactively-p 'any)
                          focus-popup))
         (cached (gethash word mnemodeck-dictionary-cache-definition))
         (result (or cached
                     (when (functionp mnemodeck-dictionary-define-function)
                       (funcall mnemodeck-dictionary-define-function word))
                     (format "Dictionary define function is not set for \"%s\"." word))))
    ;; Ensure result is a string
    (unless (stringp result)
      (setq result (format "Dictionary define function returned invalid data for \"%s\"." word)))
    ;; Cache new results
    (unless cached
      (puthash word result mnemodeck-dictionary-cache-definition))
    ;; Run hooks
    (run-hooks 'mnemodeck-define-hook)
    ;; Display in popup if requested
    (when popup
      (mnemodeck-dictionary--display result focus-popup))
    result))

;;;###autoload
(defun mnemodeck-speak (word &optional audio-player)
  "Play pronunciation audio for WORD using `mnemodeck-dictionary-audio-function'.
Downloads and caches audio files locally for better compatibility.
AUDIO-PLAYER specifies the command to play audio (defaults to system default)."
  (interactive (list nil nil))
  (let* ((word (mnemodeck--resolve-word word "Pronounce word: "))
         (cached (gethash word mnemodeck-dictionary-cache-audio))
         (audio-path (if (and (stringp cached) (file-exists-p cached))
                         cached
                       (when (functionp mnemodeck-dictionary-audio-function)
                         (funcall mnemodeck-dictionary-audio-function word))))
         (player (or audio-player
                     (executable-find "afplay")
                     (executable-find "mpv")
                     (if (eq system-type 'windows-nt) "start" "mpv"))))
    (cond
     ((and audio-path (file-exists-p audio-path))
      (puthash word audio-path mnemodeck-dictionary-cache-audio)
      (message "Playing pronunciation of \"%s\"..." word)
      (start-process "mnemodeck-audio" nil player audio-path))
     ((not (functionp mnemodeck-dictionary-audio-function))
      (message "Dictionary audio function is not set for \"%s\"" word))
     (t
      (message "No audio pronunciation available for \"%s\"" word)))))

;;;###autoload
(defun mnemodeck-clear-audio-cache ()
  "Clear the audio cache directory."
  (interactive)
  (when (file-directory-p mnemodeck-dictionary-audio-cache-dir)
    (delete-directory mnemodeck-dictionary-audio-cache-dir t)
    (message "Audio cache cleared"))
  (unless (file-directory-p mnemodeck-dictionary-audio-cache-dir)
    (message "Audio cache was already empty")))

;;;###autoload
(defun mnemodeck-clear-api-cache ()
  "Clear the in-memory dictionary cache.
This is a no-op unless your custom functions use their own caches."
  (interactive)
  (clrhash mnemodeck-dictionary-cache-definition)
  (clrhash mnemodeck-dictionary-cache-audio)
  (when (called-interactively-p 'any)
    (message "Dictionary cache cleared")))

;; Automatically close the dictionary buffer for a better review flow
(add-hook 'mnemodeck-review-quit-hook #'mnemodeck-dictionary--close-buffer)
(add-hook 'mnemodeck-review-next-card-hook #'mnemodeck-dictionary--close-buffer)
(add-hook 'mnemodeck-edit-quit-hook #'mnemodeck-dictionary--close-buffer)
;; Clear API cache but still keep audio cache
(add-hook 'mnemodeck-review-quit-hook #'mnemodeck-clear-api-cache)
(add-hook 'mnemodeck-edit-quit-hook #'mnemodeck-clear-api-cache)

;; Default keybindings

(mnemodeck--bind-keys
 mnemodeck-review-mode-map
 '(("l" . mnemodeck-lookup)
   ("L" . mnemodeck-lookup-with-provider)
   ("f" . mnemodeck-define)
   ("s" . mnemodeck-speak)))

(mnemodeck--bind-keys
 mnemodeck-edit-mode-map
 '(("l" . mnemodeck-lookup)
   ("L" . mnemodeck-lookup-with-provider)
   ("f" . mnemodeck-define)
   ("s" . mnemodeck-speak)))

;; -----------------------------------------------------
;;; Calendar
;; -----------------------------------------------------

(defgroup mnemodeck-calendar nil
  "Calendar integration for MnemoDeck."
  :group 'mnemodeck)

(defcustom mnemodeck-calendar-days-ahead 90
  "Number of days ahead to calculate due cards for calendar display."
  :type 'integer
  :group 'mnemodeck-calendar)

(defcustom mnemodeck-calendar-thresholds
  '(25 50 75)
  "List of 3 thresholds for highlighting calendar dates with due cards.
Each value represents the maximum number of cards for a new color level."
  :type '(repeat integer)
  :group 'mnemodeck-calendar)

(defface mnemodeck-calendar-level-1-face
  `((t :background ,(face-attribute 'ansi-color-green :foreground)
       :foreground ,(face-attribute 'ansi-color-black :foreground)
       :weight bold))
  "Face for dates with few due cards (level 1)."
  :group 'mnemodeck-calendar)

(defface mnemodeck-calendar-level-2-face
  `((t :background ,(face-attribute 'ansi-color-yellow :foreground)
       :foreground ,(face-attribute 'ansi-color-black :foreground)
       :weight bold))
  "Face for dates with some due cards (level 2)."
  :group 'mnemodeck-calendar)

(defface mnemodeck-calendar-level-3-face
  `((t :background ,(face-attribute 'ansi-color-red :foreground)
       :foreground ,(face-attribute 'ansi-color-black :foreground)
       :weight bold))
  "Face for dates with many due cards (level 3)."
  :group 'mnemodeck-calendar)

(defface mnemodeck-calendar-level-4-face
  `((t :background ,(face-attribute 'ansi-color-magenta :foreground)
       :foreground ,(face-attribute 'ansi-color-black :foreground)
       :weight bold))
  "Face for dates with very many due cards (level 4)."
  :group 'mnemodeck-calendar)

(defvar mnemodeck-calendar--due-counts (make-hash-table :test 'equal)
  "Cache of due-card counts keyed by calendar date.")

;; Internal functions

(defun mnemodeck-calendar--get-face-for-count (count)
  "Return the appropriate face for COUNT due cards."
  (let ((thresholds mnemodeck-calendar-thresholds))
    (cond
     ((< count (nth 0 thresholds)) 'mnemodeck-calendar-level-1-face)
     ((< count (nth 1 thresholds)) 'mnemodeck-calendar-level-2-face)
     ((< count (nth 2 thresholds)) 'mnemodeck-calendar-level-3-face)
     (t 'mnemodeck-calendar-level-4-face))))

(defun mnemodeck-calendar--date-string-to-date (date-string)
  "Convert DATE-STRING (YYYY-MM-DD) into (month day year) calendar date."
  (when (and date-string (string-match "\\`\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)\\'" date-string))
    (list (string-to-number (match-string 2 date-string))
          (string-to-number (match-string 3 date-string))
          (string-to-number (match-string 1 date-string)))))

(defun mnemodeck-calendar--time->calendar-date (time)
  "Convert TIME to a calendar date list (month day year)."
  (let ((decoded (decode-time time)))
    (list (nth 4 decoded) (nth 3 decoded) (nth 5 decoded))))

(defun mnemodeck-calendar--hash-inc (table key delta)
  "Increment TABLE at KEY by DELTA."
  (puthash key (+ delta (gethash key table 0)) table))

(defun mnemodeck-calendar--get-due-cards-by-date ()
  "Get a hash table mapping due dates to card counts.
Dates follow the review day defined by `mnemodeck-day-rollover-hour'."
  (let* ((due-counts (make-hash-table :test 'equal))
         (day-start (mnemodeck--day-start-time))
         (cutoff (time-add day-start (days-to-time mnemodeck-calendar-days-ahead)))
         (result (mnemodeck-db--due-counts-by-date day-start cutoff))
         (rows (plist-get result :rows))
         (overdue-count (plist-get result :overdue)))
    ;; Rows are grouped by local date; add each to the calendar hash.
    (dolist (row rows)
      (pcase-let ((`(,date-string ,count) row))
        (when-let ((date (mnemodeck-calendar--date-string-to-date date-string)))
          (mnemodeck-calendar--hash-inc due-counts date count))))
    ;; Overdue cards are shown on today so they remain visible.
    (when (> overdue-count 0)
      (let ((today-date (mnemodeck-calendar--time->calendar-date day-start)))
        (mnemodeck-calendar--hash-inc due-counts today-date overdue-count)))
    due-counts))

(defun mnemodeck-calendar--refresh-due-counts ()
  "Refresh cached due-counts for calendar display."
  (setq mnemodeck-calendar--due-counts (mnemodeck-calendar--get-due-cards-by-date)))

(defun mnemodeck-calendar--mark-dates-with-due-cards ()
  "Mark calendar dates with due cards using appropriate faces."
  (let* ((displayed-month (and (boundp 'displayed-month) displayed-month))
         (displayed-year (and (boundp 'displayed-year) displayed-year)))
    (when (and displayed-month displayed-year)
      (maphash (lambda (date count)
                 (let* ((due-month (nth 0 date))
                        (due-year (nth 2 date))
                        (due-n-month (+ due-month (* 12 due-year)))
                        (max-n-month (+ (1+ displayed-month) (* 12 displayed-year)))
                        (min-n-month (+ (1- displayed-month) (* 12 displayed-year))))
                   ;; Filter out dates that are not currently displayed.
                   (when (and (<= due-n-month max-n-month)
                              (<= min-n-month due-n-month))
                     (let ((face (mnemodeck-calendar--get-face-for-count count)))
                       (calendar-mark-visible-date date face)))))
               mnemodeck-calendar--due-counts))))

;;;###autoload
(defun mnemodeck-calendar-mark-due-dates ()
  "Mark dates with due cards on the calendar."
  (interactive)
  (mnemodeck-calendar--refresh-due-counts)
  ;; First clear any existing marks
  (calendar-unmark)
  ;; Then mark dates with due cards
  (mnemodeck-calendar--mark-dates-with-due-cards)
  (message "Marked dates with due cards"))

;;;###autoload
(defun mnemodeck-calendar-show-due-count-at-date ()
  "Show the number of cards due on the selected date."
  (interactive)
  (let* ((date (calendar-cursor-to-date))
         (count (gethash date mnemodeck-calendar--due-counts 0)))
    (if (> count 0)
        (message "%d card%s due on %s"
                 count
                 (if (= count 1) "" "s")
                 (calendar-date-string date)))))

;; Define a minor mode for the calendar integration
;;;###autoload
(define-minor-mode mnemodeck-calendar-mode
  "Toggle MnemoDeck calendar integration.
When enabled, dates with due cards are highlighted in the calendar."
  :global t
  :lighter " MnemoDeckCal"
  :group 'mnemodeck-calendar
  (if mnemodeck-calendar-mode
      (progn
        (add-hook 'calendar-mode-hook 'mnemodeck-calendar--refresh-due-counts)
        (add-hook 'calendar-today-visible-hook 'mnemodeck-calendar-mark-due-dates)
        (add-hook 'calendar-today-invisible-hook 'mnemodeck-calendar-mark-due-dates)
        (add-hook 'calendar-move-hook 'mnemodeck-calendar-show-due-count-at-date))
    (remove-hook 'calendar-mode-hook 'mnemodeck-calendar--refresh-due-counts)
    (remove-hook 'calendar-today-visible-hook 'mnemodeck-calendar-mark-due-dates)
    (remove-hook 'calendar-today-invisible-hook 'mnemodeck-calendar-mark-due-dates)
    (remove-hook 'calendar-move-hook 'mnemodeck-calendar-show-due-count-at-date)))

;; -----------------------------------------------------
;;; Import
;; -----------------------------------------------------

(defgroup mnemodeck-import nil
  "E-reader vocabulary import for MnemoDeck."
  :group 'mnemodeck)

(defcustom mnemodeck-import-sqlite-command "sqlite3"
  "SQLite command used for e-reader vocab extraction."
  :type 'string
  :group 'mnemodeck-import)

(defvar mnemodeck-import-kindle-buffer-name "*MnemoDeck Import Kindle*"
  "Buffer name for Kindle vocab import.")

(defvar mnemodeck-import-kobo-buffer-name "*MnemoDeck Import Kobo*"
  "Buffer name for Kobo vocab import.")

(defun mnemodeck-import--ensure-sqlite ()
  "Ensure sqlite command is available."
  (unless (executable-find mnemodeck-import-sqlite-command)
    (error "%s command not found.  Please install SQLite3" mnemodeck-import-sqlite-command)))

(defun mnemodeck-import--sqlite-call (db-file sql context)
  "Run SQL against DB-FILE and return command output.
CONTEXT is used as the error message prefix when command execution fails."
  (with-temp-buffer
    (unless (zerop (call-process mnemodeck-import-sqlite-command nil t nil db-file sql))
      (error "%s: %s" context (string-trim (buffer-string))))
    (buffer-string)))

(defun mnemodeck-import-kindle--read-words (db-file)
  "Return a list of unique words from Kindle DB-FILE."
  ;; Use stems instead of words in their original forms.
  (split-string (mnemodeck-import--sqlite-call
                 db-file
                 "SELECT stem FROM WORDS ORDER BY rowid;"
                 "Failed to query database")
                "\n" t))

(defun mnemodeck-import-kindle--maybe-clear-db (db-file)
  "Prompt to clear DB-FILE after a successful import."
  (when (yes-or-no-p (format "Clear all data from %s? "
                             (file-name-nondirectory db-file)))
    (mnemodeck-import--sqlite-call
     db-file
     "DELETE FROM LOOKUPS; DELETE FROM WORDS; DELETE FROM BOOK_INFO; VACUUM;"
     "Failed to clear database")
    (message "Database cleared successfully")))

(defun mnemodeck-import-kobo--read-words (db-file)
  "Return a list of unique words from Kobo DB-FILE."
  (split-string (mnemodeck-import--sqlite-call
                 db-file
                 "SELECT Text FROM WordList ORDER BY rowid;"
                 "Failed to query database")
                "\n" t))

(defun mnemodeck-import-kobo--maybe-clear-db (db-file)
  "Prompt to clear imported word rows from Kobo DB-FILE."
  (when (yes-or-no-p (format "Clear all words from %s? "
                             (file-name-nondirectory db-file)))
    (mnemodeck-import--sqlite-call
     db-file
     "DELETE FROM WordList; VACUUM;"
     "Failed to clear database")
    (message "WordList cleared successfully")))

;;;###autoload
(defun mnemodeck-import-kindle (db-file)
  "Extract words from Kindle vocab DB-FILE and open a batch add buffer."
  (interactive "fKindle vocab.db file: ")
  (setq db-file (expand-file-name db-file))
  (unless (file-exists-p db-file)
    (error "Database file does not exist: %s" db-file))
  (mnemodeck-import--ensure-sqlite)
  (let* ((words (mnemodeck-import-kindle--read-words db-file))
         (message-prefix (format "Extracted %d unique words." (length words))))
    (mnemodeck-add-card-batch
     words
     :buffer-name mnemodeck-import-kindle-buffer-name
     :title "MnemoDeck Kindle Vocab Import"
     :message-prefix message-prefix
     :on-confirm (lambda (_words)
                   (mnemodeck-import-kindle--maybe-clear-db db-file)))))

;;;###autoload
(defun mnemodeck-import-kobo (db-file)
  "Extract words from KoboReader DB-FILE and open a batch add buffer."
  (interactive "fKoboReader.sqlite file: ")
  (setq db-file (expand-file-name db-file))
  (unless (file-exists-p db-file)
    (error "Database file does not exist: %s" db-file))
  (mnemodeck-import--ensure-sqlite)
  (let* ((words (mnemodeck-import-kobo--read-words db-file))
         (message-prefix (format "Extracted %d unique words." (length words))))
    (mnemodeck-add-card-batch
     words
     :buffer-name mnemodeck-import-kobo-buffer-name
     :title "MnemoDeck Kobo Vocab Import"
     :message-prefix message-prefix
     :on-confirm (lambda (_words)
                   (mnemodeck-import-kobo--maybe-clear-db db-file)))))

;; -----------------------------------------------------
;;; Keybinding Helper
;; -----------------------------------------------------

(defun mnemodeck--collect-mode-bindings (keymap &optional prefix)
  "Collect MnemoDeck bindings from KEYMAP as (KEY . COMMAND) pairs.
PREFIX is the key sequence vector accumulated during recursion."
  (let ((bindings nil)
        (prefix (or prefix [])))
    (map-keymap
     (lambda (event raw-binding)
       ;; Rebuild full key sequence while recursively walking prefix maps.
       (let* ((key (vconcat prefix (vector event)))
              ;; Normalize menu-item wrappers to their real command/keymap target.
              (binding (if (and (consp raw-binding)
                                (eq (car raw-binding) 'menu-item))
                           (nth 2 raw-binding)
                         raw-binding)))
         (cond
          ((keymapp binding)
           ;; Descend into nested keymaps such as multi-key prefixes.
           (setq bindings (nconc bindings (mnemodeck--collect-mode-bindings binding key))))
          ((and (symbolp binding)
                (commandp binding)
                (member "mnemodeck"
                        (split-string (symbol-name binding) "[^[:alnum:]_]+" t)))
           (push (cons (key-description key) binding) bindings)))))
     keymap)
    bindings))

(defun mnemodeck--describe-command (target)
  "Show help for command TARGET.
TARGET can be a command symbol or a button carrying `mnemodeck-symbol'.
Prefer `helpful-callable' when available."
  (let ((sym (if (symbolp target)
                 target
               (button-get target 'mnemodeck-symbol))))
    (if (fboundp 'helpful-callable)
        (helpful-callable sym)
      (describe-function sym))))

(defun mnemodeck--render-mode-bindings (title keymap)
  "Render TITLE and MnemoDeck bindings from KEYMAP into current buffer."
  (insert (format "%s\n\n" title))
  (let* ((bindings (sort (mnemodeck--collect-mode-bindings keymap)
                         (lambda (a b) (string< (car a) (car b)))))
         (rows (mapcar
                (lambda (binding)
                  (let* ((fallback (car binding))
                         (command (cdr binding))
                         ;; Resolve display label via substitute-command-keys so aliases/remaps
                         ;; and mode-local key syntax render like built-in help buffers.
                         (label (with-temp-buffer
                                  (use-local-map keymap)
                                  (substitute-command-keys (format "\\[%s]" command))))
                         ;; If resolution fails or degrades to M-x, keep collected key text.
                         (resolved (if (or (string-empty-p label)
                                           (string-prefix-p "M-x " label))
                                       fallback
                                     label)))
                    (cons (propertize resolved 'face 'help-key-binding) command)))
                bindings)))
    (if (null rows)
        (insert "No MnemoDeck bindings found in this keymap.\n")
      ;; Align command names by the visible width of key labels.
      (let ((width (apply #'max (mapcar (lambda (row)
                                          (string-width (substring-no-properties (car row))))
                                        rows))))
        (dolist (row rows)
          (let* ((label (car row))
                 (command (cdr row))
                 (padding (- width (string-width (substring-no-properties label)))))
            (insert label (make-string (+ 2 padding) ?\s))
            (insert-text-button
             (symbol-name command)
             'mnemodeck-symbol command
             'action #'mnemodeck--describe-command
             'follow-link t
             'help-echo "mouse-1, mouse-2 or RET: show function help"
             'face 'link)
            (insert "\n")))))))

(defun mnemodeck-show-mode-keys ()
  "Show all MnemoDeck keybindings for the current mode."
  (interactive)
  (let* ((context (cond
                    ((derived-mode-p 'mnemodeck-review-mode)
                     (cons "MnemoDeck Review Mode Keys" mnemodeck-review-mode-map))
                    ((derived-mode-p 'mnemodeck-edit-mode)
                     (cons "MnemoDeck Edit Mode Keys" mnemodeck-edit-mode-map))
                    (t nil)))
         ;; Capture mode context before entering help buffer.
         (title (car context))
         (keymap (cdr context)))
    (with-help-window "*MnemoDeck Keys*"
      (if context
          (mnemodeck--render-mode-bindings title keymap)
        (insert "No MnemoDeck key summary for this mode.\n")))))

(mnemodeck--bind-keys
 mnemodeck-review-mode-map
 '(("h" . mnemodeck-show-mode-keys)))

(mnemodeck--bind-keys
 mnemodeck-edit-mode-map
 '(("h" . mnemodeck-show-mode-keys)))

(provide 'mnemodeck)
;;; mnemodeck.el ends here
