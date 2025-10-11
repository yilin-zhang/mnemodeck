;;; mnemodeck-test.el --- Tests for MnemoDeck -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'mnemodeck)

(defmacro mnemodeck-test--with-temp-db (&rest body)
  "Run BODY with an isolated temporary MnemoDeck database."
  (declare (indent 0) (debug t))
  `(let* ((tmp-dir (make-temp-file "mnemodeck-test-" t))
          (mnemodeck-directory (file-name-as-directory tmp-dir))
          (mnemodeck-db-file (expand-file-name "mnemodeck.sqlite" tmp-dir))
          (mnemodeck-backup-directory (expand-file-name "backups" tmp-dir))
          (mnemodeck-db--conn nil)
          (mnemodeck--fsrs-scheduler nil)
          (mnemodeck-review-order '((:sort :due :asc :learning)
                                    (:sort :added :desc :new)
                                    (:sort :due :asc :review))))
     (unwind-protect
         (progn
           ,@body)
       (when (and (boundp 'mnemodeck-db--conn) mnemodeck-db--conn)
         (sqlite-close mnemodeck-db--conn)
         (setq mnemodeck-db--conn nil))
       (delete-directory tmp-dir t))))

(defun mnemodeck-test--ts (time)
  "Convert TIME to MnemoDeck timestamp string."
  (mnemodeck--time->fsrs-timestamp time))

(defun mnemodeck-test--make-meta (&rest plist)
  "Create a card meta object from PLIST."
  (apply #'make-mnemodeck-card-meta plist))

;; ---------------------------------------------------------------------------
;; Normalization helpers
;; ---------------------------------------------------------------------------
;; These tests protect input sanitation rules.  Empty/blank words must fail,
;; while hint normalization should map blank strings to nil.

(ert-deftest mnemodeck-test-normalize-word ()
  (should (string= (mnemodeck-db--normalize-word "  lucid  ") "lucid"))
  (should-error (mnemodeck-db--normalize-word "  "))
  (should-error (mnemodeck-db--normalize-word "")))

(ert-deftest mnemodeck-test-normalize-hint ()
  (should (equal (mnemodeck-db--normalize-hint nil) nil))
  (should (equal (mnemodeck-db--normalize-hint "  ") nil))
  (should (string= (mnemodeck-db--normalize-hint "  foo bar  ") "foo bar")))

;; ---------------------------------------------------------------------------
;; Basic DB lifecycle
;; ---------------------------------------------------------------------------
;; Verifies that DB initialization + upsert + select round-trip works for the
;; minimal card payload.

(ert-deftest mnemodeck-test-db-ensure-and-upsert-select ()
  (mnemodeck-test--with-temp-db
    (mnemodeck-db--ensure)
    (let* ((now (current-time))
           (meta (mnemodeck-test--make-meta
                  :added-date (mnemodeck-test--ts now)
                  :last-review (mnemodeck-test--ts now)
                  :due (mnemodeck-test--ts now)
                  :state :review
                  :hint "sample")))
      (mnemodeck-db--upsert-card "lucid" meta)
      (let ((row (mnemodeck-db--select-card "lucid")))
        (should row)
        (should (string= (car row) "lucid"))))))

;; ---------------------------------------------------------------------------
;; Archive/unarchive flow
;; ---------------------------------------------------------------------------
;; Ensures archive filters behave correctly and that unarchive returns the card
;; to normal (non-archived) selection results.

(ert-deftest mnemodeck-test-archive-filter-flow ()
  (mnemodeck-test--with-temp-db
    (let* ((now (current-time))
           (ts (mnemodeck-test--ts now))
           (meta (mnemodeck-test--make-meta
                  :added-date ts :last-review ts :due ts :state :review)))
      (mnemodeck-db--upsert-card "archive-me" meta)
      (should (= 1 (length (mnemodeck-db--select-cards 'all nil))))
      (mnemodeck-db--archive-card "archive-me" ts)
      (should (= 0 (length (mnemodeck-db--select-cards 'all nil))))
      (should (= 1 (length (mnemodeck-db--select-cards 'archived nil))))
      (mnemodeck-db--unarchive-card "archive-me")
      (should (= 1 (length (mnemodeck-db--select-cards 'all nil)))))))

;; ---------------------------------------------------------------------------
;; Due-word selection with review-order
;; ---------------------------------------------------------------------------
;; Exercises the DB-level queue assembly:
;; - learning due "now" by exact timestamp,
;; - new/review due by review-day cutoff,
;; - final sequence respects configured review-order steps.

(ert-deftest mnemodeck-test-select-due-words-review-order ()
  (mnemodeck-test--with-temp-db
    (let* ((now (current-time))
           (past-2h (time-subtract now (seconds-to-time (* 2 3600))))
           (past-1h (time-subtract now (seconds-to-time 3600)))
           (past-10m (time-subtract now (seconds-to-time 600)))
           (future-30m (time-add now (seconds-to-time 1800))))
      ;; Learning due now.
      (mnemodeck-db--upsert-card
       "learn-a"
       (mnemodeck-test--make-meta
        :added-date (mnemodeck-test--ts past-2h)
        :last-review (mnemodeck-test--ts past-1h)
        :due (mnemodeck-test--ts past-10m)
        :state :learning))
      ;; New card (last_review nil), due by today's review cutoff.
      (mnemodeck-db--upsert-card
       "new-a"
       (mnemodeck-test--make-meta
        :added-date (mnemodeck-test--ts now)
        :last-review nil
        :due (mnemodeck-test--ts now)
        :state :learning))
      ;; Review card due later today (still included in review target).
      (mnemodeck-db--upsert-card
       "review-a"
       (mnemodeck-test--make-meta
        :added-date (mnemodeck-test--ts past-2h)
        :last-review (mnemodeck-test--ts past-1h)
        :due (mnemodeck-test--ts future-30m)
        :state :review))

      (should (equal (mnemodeck-db--select-due-words)
                     '("learn-a" "new-a" "review-a"))))))

;; ---------------------------------------------------------------------------
;; Review-order validation
;; ---------------------------------------------------------------------------
;; These tests guard schema rules for `mnemodeck-review-order`.
;; Duplicate targets should fail early, and learning/new sort fields are
;; intentionally restricted.

(ert-deftest mnemodeck-test-review-order-validate-rejects-duplicates ()
  (should-error
   (mnemodeck-db--review-validate-order
    '((:shuffle :review)
      (:sort :due :asc :review)))))

(ert-deftest mnemodeck-test-review-order-validate-rejects-invalid-learning-sort-field ()
  (should-error
   (mnemodeck-db--review-validate-order
    '((:sort :stability :desc :learning)))))

;; ---------------------------------------------------------------------------
;; Edit sorting SQL generation
;; ---------------------------------------------------------------------------
;; Critical for table behavior: numeric columns use numeric coalesce (0),
;; while text/time columns use string coalesce ('').

(ert-deftest mnemodeck-test-edit-order-sql-numeric-vs-text-columns ()
  (should (string-match-p
           "ORDER BY COALESCE(stability, 0) DESC, rowid DESC"
           (mnemodeck-db--edit-order-sql '("Stability" . t))))
  (should (string-match-p
           "ORDER BY COALESCE(due, '') ASC, rowid ASC"
           (mnemodeck-db--edit-order-sql '("Due" . nil)))))

;; ---------------------------------------------------------------------------
;; Backup naming + list ordering
;; ---------------------------------------------------------------------------
;; These tests target backup file conventions:
;; - suffix collision resolution,
;; - newest-first ordering by embedded UTC timestamp,
;; - timestamp extraction from suffixed filenames.

(ert-deftest mnemodeck-test-backup-target-adds-numeric-suffix ()
  (mnemodeck-test--with-temp-db
    (let* ((backup-dir (file-name-as-directory mnemodeck-backup-directory))
           (base (file-name-base mnemodeck-db-file))
           (timestamp "20260206T120000Z")
           (existing (expand-file-name
                      (format "%s-%s.sqlite" base timestamp)
                      backup-dir)))
      (make-directory backup-dir t)
      (with-temp-file existing (insert "dummy"))
      (should (string= (mnemodeck-db--backup-target backup-dir base timestamp)
                       (expand-file-name
                        (format "%s-%s-1.sqlite" base timestamp)
                        backup-dir))))))

(ert-deftest mnemodeck-test-backup-files-sorted-by-timestamp-desc ()
  (mnemodeck-test--with-temp-db
    (let* ((backup-dir (file-name-as-directory mnemodeck-backup-directory))
           (base (file-name-base mnemodeck-db-file))
           (older (expand-file-name
                   (format "%s-20250101T010101Z.sqlite" base)
                   backup-dir))
           (middle (expand-file-name
                    (format "%s-20250102T010101Z.sqlite" base)
                    backup-dir))
           (newest (expand-file-name
                    (format "%s-20250103T010101Z.sqlite" base)
                    backup-dir)))
      (make-directory backup-dir t)
      (dolist (f (list middle newest older))
        (with-temp-file f (insert "dummy")))
      (should (equal (mnemodeck-db--backup-files)
                     (list newest middle older))))))

(ert-deftest mnemodeck-test-backup-timestamp-parses-suffixed-filenames ()
  (mnemodeck-test--with-temp-db
    (let* ((base (file-name-base mnemodeck-db-file))
           (file (expand-file-name
                  (format "%s-20260101T123456Z-2.sqlite" base)
                  temporary-file-directory))
           (parsed (mnemodeck-db--backup-timestamp file)))
      (should parsed)
      (should (string= (format-time-string "%Y%m%dT%H%M%SZ" parsed "UTC0")
                       "20260101T123456Z")))))

;; Ensure exported UTC timestamp helper always matches the filename-friendly
;; compact format used by backups and JSON exports.
(ert-deftest mnemodeck-test-db-timestamp-utc-format ()
  (should (string-match-p
           "\\`[0-9]\\{8\\}T[0-9]\\{6\\}Z\\'"
           (mnemodeck-db--timestamp-utc))))

;; ---------------------------------------------------------------------------
;; JSON import
;; ---------------------------------------------------------------------------
;; Covers:
;; - importing exported-format JSON rows,
;; - archived flag preservation,
;; - duplicate conflict handling (skip/overwrite + global choice).

(ert-deftest mnemodeck-test-db-import-json-adds-and-preserves-archive ()
  (mnemodeck-test--with-temp-db
    (let* ((file (expand-file-name "import.json" tmp-dir))
           (rows '(((word . "alpha")
                    (added_date . "20250101T010101Z")
                    (last_review . nil)
                    (due . "20250101T010101Z")
                    (archived_at . nil)
                    (state . "learning")
                    (step . 0)
                    (stability . nil)
                    (difficulty . nil)
                    (hint . "first"))
                   ((word . "beta")
                    (added_date . "20250102T010101Z")
                    (last_review . "20250102T010101Z")
                    (due . "20250103T010101Z")
                    (archived_at . "20250104T010101Z")
                    (state . "review")
                    (step . 0)
                    (stability . 10.0)
                    (difficulty . 3.0)
                    (hint . "second"))))
           (json-encoding-pretty-print t))
      (with-temp-file file
        (insert (json-encode rows)))
      (let ((stats (mnemodeck-db-import-json file)))
        (should (= 2 (plist-get stats :added)))
        (should (= 0 (plist-get stats :overwritten)))
        (should (= 0 (plist-get stats :skipped))))
      (should (= 1 (length (mnemodeck-db--select-cards 'all nil))))
      (should (= 1 (length (mnemodeck-db--select-cards 'archived nil)))))))

(ert-deftest mnemodeck-test-db-import-json-conflict-skip-and-overwrite ()
  (mnemodeck-test--with-temp-db
    (let* ((file (expand-file-name "import-conflict.json" tmp-dir))
           (base-meta (mnemodeck-test--make-meta
                       :added-date "20250101T000000Z"
                       :last-review "20250101T000000Z"
                       :due "20250102T000000Z"
                       :state :review
                       :hint "old"))
           (rows '(((word . "alpha")
                    (added_date . "20250110T000000Z")
                    (last_review . "20250110T000000Z")
                    (due . "20250111T000000Z")
                    (archived_at . nil)
                    (state . "review")
                    (step . 0)
                    (stability . 1.0)
                    (difficulty . 1.0)
                    (hint . "new"))))
           (json-encoding-pretty-print t))
      (mnemodeck-db--upsert-card "alpha" base-meta)
      (with-temp-file file
        (insert (json-encode rows)))
      ;; Conflict => skip
      (cl-letf (((symbol-function 'mnemodeck-db--import-read-conflict-choice)
                 (lambda (_word) (cons :skip nil))))
        (let ((stats (mnemodeck-db-import-json file)))
          (should (= 0 (plist-get stats :added)))
          (should (= 0 (plist-get stats :overwritten)))
          (should (= 1 (plist-get stats :skipped)))))
      (should (string= "old" (nth 8 (mnemodeck-db--select-card "alpha"))))
      ;; Conflict => overwrite
      (cl-letf (((symbol-function 'mnemodeck-db--import-read-conflict-choice)
                 (lambda (_word) (cons :overwrite nil))))
        (let ((stats (mnemodeck-db-import-json file)))
          (should (= 0 (plist-get stats :added)))
          (should (= 1 (plist-get stats :overwritten)))
          (should (= 0 (plist-get stats :skipped)))))
      (should (string= "new" (nth 8 (mnemodeck-db--select-card "alpha")))))))

(ert-deftest mnemodeck-test-db-import-read-conflict-choice-global-confirm ()
  ;; Choosing all-overwrite and confirming should return current overwrite action
  ;; and persist global overwrite for remaining conflicts.
  (cl-letf (((symbol-function 'read-char-choice)
             (lambda (_prompt _chars) ?A))
            ((symbol-function 'yes-or-no-p)
             (lambda (_prompt) t)))
    (should (equal (mnemodeck-db--import-read-conflict-choice "alpha")
                   '(:overwrite . :overwrite))))
  ;; If user cancels a global choice, function should ask again and still
  ;; resolve the current word action.
  (let ((calls 0))
    (cl-letf (((symbol-function 'read-char-choice)
               (lambda (_prompt _chars)
                 (setq calls (1+ calls))
                 (if (= calls 1) ?a ?o)))
              ((symbol-function 'yes-or-no-p)
               (lambda (_prompt) nil)))
      (should (equal (mnemodeck-db--import-read-conflict-choice "beta")
                     '(:overwrite . nil))))))

(ert-deftest mnemodeck-test-db-import-record-step-default-by-state ()
  ;; Missing step defaults to nil for review, 0 for learning-like states.
  (pcase-let ((`(,_word ,review-meta ,_archived)
               (mnemodeck-db--import-record->card
                '((word . "review-word")
                  (state . "review"))))
              (`(,_word2 ,learning-meta ,_archived2)
               (mnemodeck-db--import-record->card
                '((word . "learning-word")
                  (state . "learning")))))
    (should (null (mnemodeck-card-meta-step review-meta)))
    (should (= 0 (mnemodeck-card-meta-step learning-meta)))))

;; ---------------------------------------------------------------------------
;; Edit filter SQL mapping
;; ---------------------------------------------------------------------------
;; Keeps tabulated-list filter commands stable by asserting exact SQL snippets.

(ert-deftest mnemodeck-test-edit-filter-sql-clauses ()
  (pcase-let ((`(,review-sql . ,review-params) (mnemodeck-db--edit-filter-sql 'review))
              (`(,learning-sql . ,learning-params) (mnemodeck-db--edit-filter-sql 'learning))
              (`(,archived-sql . ,archived-params) (mnemodeck-db--edit-filter-sql 'archived))
              (`(,all-sql . ,all-params) (mnemodeck-db--edit-filter-sql 'all)))
    ;; Keep checks resilient to SQL formatting changes while asserting semantics.
    (should (string-match-p "archived_at IS NULL" review-sql))
    (should (string-match-p "state = 'review'" review-sql))
    (should (equal review-params nil))

    (should (string-match-p "archived_at IS NULL" learning-sql))
    (should (string-match-p "state IN ('learning', 'relearning')" learning-sql))
    (should (equal learning-params nil))

    (should (string-match-p "archived_at IS NOT NULL" archived-sql))
    (should (equal archived-params nil))

    (should (string-match-p "archived_at IS NULL" all-sql))
    (should (equal all-params nil))))

;; ---------------------------------------------------------------------------
;; Review target clause generation
;; ---------------------------------------------------------------------------
;; Validates that each target kind maps to expected WHERE conditions and
;; generates exactly one cutoff parameter.

(ert-deftest mnemodeck-test-review-target-clause-shapes ()
  (let* ((now (current-time))
         (learning (mnemodeck-db--review-target-clause :learning now))
         (review (mnemodeck-db--review-target-clause :review now))
         (new (mnemodeck-db--review-target-clause :new now)))
    (should (string-match-p "state IN ('learning', 'relearning')" (car learning)))
    (should (string-match-p "state = 'review'" (car review)))
    (should (string-match-p "last_review IS NULL" (car new)))
    (should (= 1 (length (cdr learning))))
    (should (= 1 (length (cdr review))))
    (should (= 1 (length (cdr new))))))

;; ---------------------------------------------------------------------------
;; Backup prune behavior
;; ---------------------------------------------------------------------------
;; Here we test both non-interactive prune by max-count, and interactive
;; confirmation refusal.

(ert-deftest mnemodeck-test-backup-prune-max-count-without-confirm ()
  (mnemodeck-test--with-temp-db
    (let* ((backup-dir (file-name-as-directory mnemodeck-backup-directory))
           (base (file-name-base mnemodeck-db-file))
           (mnemodeck-backup-retain-days 365)
           (mnemodeck-backup-prune-min-count 999)
           (mnemodeck-backup-prune-max-count 2)
           (mnemodeck-backup-prune-confirm nil)
           (files (list
                   (expand-file-name (format "%s-20250101T010101Z.sqlite" base) backup-dir)
                   (expand-file-name (format "%s-20250102T010101Z.sqlite" base) backup-dir)
                   (expand-file-name (format "%s-20250103T010101Z.sqlite" base) backup-dir)
                   (expand-file-name (format "%s-20250104T010101Z.sqlite" base) backup-dir))))
      (make-directory backup-dir t)
      (cl-loop for f in files
               for idx from 0
               do (with-temp-file f (insert "dummy"))
               do (set-file-times f (time-subtract (current-time)
                                                   (days-to-time (+ 10 idx)))))
      (mnemodeck-db--backup-prune backup-dir base)
      (let ((remaining (sort (directory-files backup-dir t "\\.sqlite\\'") #'string<)))
        (should (= 2 (length remaining)))
        ;; Should keep the two newest by file mtime.
        ;; In this fixture, 20250101/20250102 were given newer mtimes.
        (should (equal (mapcar #'file-name-nondirectory remaining)
                       (list (format "%s-20250101T010101Z.sqlite" base)
                             (format "%s-20250102T010101Z.sqlite" base))))))))

(ert-deftest mnemodeck-test-backup-prune-respects-confirm-no ()
  (mnemodeck-test--with-temp-db
    (let* ((backup-dir (file-name-as-directory mnemodeck-backup-directory))
           (base (file-name-base mnemodeck-db-file))
           (mnemodeck-backup-retain-days 365)
           (mnemodeck-backup-prune-min-count 1)
           (mnemodeck-backup-prune-max-count 1)
           (mnemodeck-backup-prune-confirm t)
           (files (list
                   (expand-file-name (format "%s-20250101T010101Z.sqlite" base) backup-dir)
                   (expand-file-name (format "%s-20250102T010101Z.sqlite" base) backup-dir))))
      (make-directory backup-dir t)
      (dolist (f files)
        (with-temp-file f (insert "dummy")))
      ;; Mock confirmation prompt: user declines, so nothing should be deleted.
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) nil)))
        (mnemodeck-db--backup-prune backup-dir base))
      (should (= 2 (length (directory-files backup-dir t "\\.sqlite\\'")))))))

;; ---------------------------------------------------------------------------
;; Backup idempotency
;; ---------------------------------------------------------------------------
;; Calling interactive backup twice without DB changes should create exactly one
;; backup file.

(ert-deftest mnemodeck-test-db-backup-skip-when-unchanged ()
  (mnemodeck-test--with-temp-db
    (mnemodeck-db--ensure)
    ;; Create a real DB change so first backup has content.
    (let* ((now (mnemodeck-test--ts (current-time)))
           (meta (mnemodeck-test--make-meta
                  :added-date now :last-review now :due now :state :review)))
      (mnemodeck-db--upsert-card "backup-word" meta))
    (mnemodeck-db-backup)
    (let ((count-1 (length (mnemodeck-db--backup-files))))
      (should (= 1 count-1))
      ;; No DB writes between backups; second call should not add files.
      (mnemodeck-db-backup)
      (should (= count-1 (length (mnemodeck-db--backup-files)))))))

;; ---------------------------------------------------------------------------
;; Completion binding hook behavior
;; ---------------------------------------------------------------------------
;; The restore completion wrapper supports dynamic var bindings for completion
;; UIs (e.g., vertico sorting override).  We mock `completing-read` to prove
;; those bindings are in effect.

(ert-deftest mnemodeck-test-read-backup-choice-binds-completion-vars ()
  (let ((mnemodeck-backup-restore-completion-setup
         (lambda () '((mnemodeck-test--temp-binding . 42))))
        (captured nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (prompt choices &optional predicate require-match initial-input hist def)
                 (setq captured (list :prompt prompt
                                      :choices choices
                                      :predicate predicate
                                      :require-match require-match
                                      :initial-input initial-input
                                      :hist hist
                                      :default def))
                 ;; Read from dynamic binding explicitly.
                 (number-to-string (symbol-value 'mnemodeck-test--temp-binding)))))
      (should (string= (mnemodeck-db--read-backup-choice '("a" "b") "a") "42"))
      ;; Assert important completion contract was preserved.
      (should (equal (plist-get captured :choices) '("a" "b")))
      (should (equal (plist-get captured :default) "a"))
      (should (eq (plist-get captured :require-match) t)))))

;; ---------------------------------------------------------------------------
;; Review flow: grade handling and hook transitions
;; ---------------------------------------------------------------------------
;; These tests focus on the central grade handler used by 1/2/3/4 commands.
;; We explicitly mock side-effect functions (`mnemodeck-rate-card`,
;; `mnemodeck-review-next-card`) so we can verify behavior without UI coupling.

(ert-deftest mnemodeck-test-review-handle-grade-triggers-daily-goal-hook-on-transition ()
  (let ((mnemodeck-current-word "goal-word")
        (hook-count 0)
        (rated nil)
        (next-count 0)
        ;; First check: before rating -> not reached.
        ;; Second check: after rating -> reached.
        (goal-states '(nil t)))
    (let ((mnemodeck-review-daily-goal-reached-hook
           (list (lambda () (setq hook-count (1+ hook-count))))))
      (cl-letf (((symbol-function 'mnemodeck-review--daily-goal-reached-p)
                 (lambda ()
                   (prog1 (car goal-states)
                     (setq goal-states (cdr goal-states)))))
                ((symbol-function 'mnemodeck-rate-card)
                 (lambda (word grade)
                   (setq rated (list word grade))))
                ((symbol-function 'mnemodeck-review-next-card)
                 (lambda ()
                   (setq next-count (1+ next-count)))))
        (mnemodeck-review--handle-grade 3)))
    (should (equal rated '("goal-word" 3)))
    (should (= 1 hook-count))
    (should (= 1 next-count))))

(ert-deftest mnemodeck-test-review-handle-grade-does-not-trigger-hook-without-transition ()
  (let ((mnemodeck-current-word "steady-word")
        (hook-count 0)
        ;; reached before and after rating -> no transition
        (goal-states '(t t)))
    (let ((mnemodeck-review-daily-goal-reached-hook
           (list (lambda () (setq hook-count (1+ hook-count))))))
      (cl-letf (((symbol-function 'mnemodeck-review--daily-goal-reached-p)
                 (lambda ()
                   (prog1 (car goal-states)
                     (setq goal-states (cdr goal-states)))))
                ((symbol-function 'mnemodeck-rate-card) (lambda (&rest _) nil))
                ((symbol-function 'mnemodeck-review-next-card) (lambda () nil)))
        (mnemodeck-review--handle-grade 1)))
    (should (= 0 hook-count))))

;; ---------------------------------------------------------------------------
;; Review flow: next-card sequencing and hook execution
;; ---------------------------------------------------------------------------
;; Ensures `mnemodeck-review-next-card` consumes queue state in order and runs
;; next-card hook once per transition.

(ert-deftest mnemodeck-test-review-next-card-consumes-due-queue-and-runs-hook ()
  (let ((mnemodeck-due-words '("w1" "w2"))
        (mnemodeck-current-word nil)
        (hook-count 0))
    (let ((mnemodeck-review-next-card-hook
           (list (lambda () (setq hook-count (1+ hook-count))))))
      (cl-letf (((symbol-function 'mnemodeck-review--reset-ui-state) (lambda () nil))
                ((symbol-function 'mnemodeck-review--render-buffer) (lambda (&rest _) nil))
                ((symbol-function 'mnemodeck-review-quit) (lambda () nil)))
        (mnemodeck-review-next-card)))
    (should (equal mnemodeck-current-word "w1"))
    (should (equal mnemodeck-due-words '("w2")))
    (should (= 1 hook-count))))

;; ---------------------------------------------------------------------------
;; Edit flow: batch operations and delegation branches
;; ---------------------------------------------------------------------------
;; These tests verify branch behavior in `mnemodeck-edit-delete` and
;; `mnemodeck-edit-archive` without requiring tabulated-list UI setup.

(ert-deftest mnemodeck-test-edit-delete-marked-branch-runs-batch-delete ()
  (let ((deleted '())
        (ensured nil)
        (cleared nil)
        (refreshed nil))
    (cl-letf (((symbol-function 'mnemodeck-edit--marked-words)
               (lambda () '("a" "b")))
              ((symbol-function 'mnemodeck-edit--ensure-not-current)
               (lambda (words) (setq ensured words)))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
              ((symbol-function 'mnemodeck-delete-card)
               (lambda (word) (push word deleted)))
              ((symbol-function 'mnemodeck-edit--clear-marks)
               (lambda () (setq cleared t)))
              ((symbol-function 'mnemodeck-edit-refresh)
               (lambda () (setq refreshed t)))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      (mnemodeck-edit-delete))
    (should (equal ensured '("a" "b")))
    (should (equal (sort deleted #'string<) '("a" "b")))
    (should cleared)
    (should refreshed)))

(ert-deftest mnemodeck-test-edit-delete-without-marks-delegates-to-single-delete ()
  (let ((delegated nil))
    (cl-letf (((symbol-function 'mnemodeck-edit--marked-words) (lambda () nil))
              ((symbol-function 'mnemodeck-edit-delete-card)
               (lambda () (setq delegated t))))
      (mnemodeck-edit-delete))
    (should delegated)))

(ert-deftest mnemodeck-test-edit-archive-uses-unarchive-in-archived-filter ()
  (let ((mnemodeck-edit--filter 'archived)
        (unarchived nil)
        (archived nil))
    (cl-letf (((symbol-function 'mnemodeck-edit--marked-words) (lambda () nil))
              ((symbol-function 'mnemodeck-edit-unarchive-card)
               (lambda () (setq unarchived t)))
              ((symbol-function 'mnemodeck-edit-archive-card)
               (lambda () (setq archived t))))
      (mnemodeck-edit-archive))
    (should unarchived)
    (should-not archived)))

;; ---------------------------------------------------------------------------
;; Review rendering internals (lightweight, no window coupling)
;; ---------------------------------------------------------------------------
;; We only test the pure list-assembly behavior here.  This protects the
;; separator-collapse rule without requiring full buffer rendering.

(ert-deftest mnemodeck-test-review-collect-component-items-collapses-separators ()
  (cl-letf (((symbol-function 'mnemodeck-review-component-separator)
             (lambda () "SEP"))
            ((symbol-function 'mnemodeck-test--component-a)
             (lambda () "A"))
            ((symbol-function 'mnemodeck-test--component-b)
             (lambda () "B")))
    (let* ((result (mnemodeck-review--collect-component-items
                    '(mnemodeck-review-component-separator
                      mnemodeck-test--component-a
                      mnemodeck-review-component-separator
                      mnemodeck-review-component-separator
                      mnemodeck-test--component-b)))
           (items (car result)))
      ;; Leading separator should be skipped.
      ;; Consecutive separators should collapse into one.
      (should (equal (mapcar #'car items) '("A" "SEP" "B")))
      (should (equal (mapcar #'cdr items) '(nil t nil))))))

;; ---------------------------------------------------------------------------
;; Hint timer state machine
;; ---------------------------------------------------------------------------
;; `mnemodeck-review--start-hint-timer' should not schedule multiple timers.

(ert-deftest mnemodeck-test-review-start-hint-timer-is-idempotent ()
  (let ((mnemodeck-review--hint-timer nil)
        (calls 0)
        (mnemodeck-review-hint-delay 0.1))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (&rest _)
                 (setq calls (1+ calls))
                 'fake-timer)))
      (mnemodeck-review--start-hint-timer)
      (mnemodeck-review--start-hint-timer))
    (should (= calls 1))
    (should (eq mnemodeck-review--hint-timer 'fake-timer))))

;; ---------------------------------------------------------------------------
;; Calendar-facing DB aggregation
;; ---------------------------------------------------------------------------
;; Keep this at DB layer: verify overdue and in-range due counts are split
;; correctly for `mnemodeck-db--due-counts-by-date'.

(ert-deftest mnemodeck-test-db-due-counts-by-date-splits-overdue-and-range ()
  (mnemodeck-test--with-temp-db
    (let* ((now (current-time))
           (day-start (mnemodeck--day-start-time now))
           (cutoff (mnemodeck--next-day-start-time now))
           (overdue-time (time-subtract day-start (seconds-to-time 60)))
           (in-range-time (time-add day-start (seconds-to-time 3600)))
           (ts-added (mnemodeck-test--ts (time-subtract now (seconds-to-time 7200))))
           (ts-last (mnemodeck-test--ts (time-subtract now (seconds-to-time 1800)))))
      ;; Overdue reviewed card.
      (mnemodeck-db--upsert-card
       "overdue-card"
       (mnemodeck-test--make-meta
        :added-date ts-added
        :last-review ts-last
        :due (mnemodeck-test--ts overdue-time)
        :state :review))
      ;; Due in current [day-start, cutoff) window.
      (mnemodeck-db--upsert-card
       "range-card"
       (mnemodeck-test--make-meta
        :added-date ts-added
        :last-review ts-last
        :due (mnemodeck-test--ts in-range-time)
        :state :review))
      (let* ((result (mnemodeck-db--due-counts-by-date day-start cutoff))
             (rows (plist-get result :rows))
             (overdue (plist-get result :overdue))
             (in-range-count (apply #'+ (mapcar #'cadr rows))))
        (should (= overdue 1))
        (should (= in-range-count 1))))))

(provide 'mnemodeck-test)
;;; mnemodeck-test.el ends here
