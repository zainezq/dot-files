;;; custom-agenda.el --- Custom Org Agenda Commands and Helper Functions

;; Author: Zaine
;; Version: 1.0
;; URL: https://github.com/zainezq/dot-files/

;;; Commentary:

;; Custom file for org agenda

;;; Code:

;; ----------------------------------------------------------------------------
;; Custom Org Agenda Commands
;; ----------------------------------------------------------------------------

(setq org-agenda-custom-commands
      '(("br" "Reading Books"
         ((tags "+Group=\"Reading\"+Status=\"Reading\""
                ((org-agenda-overriding-header "Books I am Reading")))
          (tags "+Group=\"Reading\"+Status=\"To Read\""
                ((org-agenda-overriding-header "Books To Read")))
          (tags "+Group=\"Reading\"+Status=\"Read\""
                ((org-agenda-overriding-header "Books I Have Read")))))

        ("bs" "Study Books"
         ((tags "+Group=\"Study\"+Status=\"Studying\""
                ((org-agenda-overriding-header "Books I am Studying")))
          (tags "+Group=\"Study\"+Status=\"To Study\""
                ((org-agenda-overriding-header "Books To Study")))
          (tags "+Group=\"Study\"+Status=\"Studied D\""
                ((org-agenda-overriding-header "Books I Have Studied (Darsi)")))
          (tags "+Group=\"Study\"+Status=\"Additional Study\""
                ((org-agenda-overriding-header "Additional Study Books")))
          (tags "+Group=\"Study\"+Status=\"Studied A\""
                ((org-agenda-overriding-header "Books I Have Studied (Additional)")))
          (tags "+Group=\"Study\"+Status=\"Reference\""
                ((org-agenda-overriding-header "Reference Books")))))

        ("bo" "Other Books"
         ((tags "+Group=\"Others\"+Status=\"Reading\""
                ((org-agenda-overriding-header "Books I am Reading")))
          (tags "+Group=\"Others\"+Status=\"To Read\""
                ((org-agenda-overriding-header "Books To Read")))
          (tags "+Group=\"Others\"+Status=\"Read\""
                ((org-agenda-overriding-header "Books I Have Read")))))

        ("za" "All Job Applications"
         ((tags "+STATUS={Applied\\|Not applied\\|Rejected\\|Interviewing\\|Assessment}"
                ((org-agenda-overriding-header "All Job Applications")))))

        ("zb" "Filtered Job Applications"
         ((tags "+STATUS=\"Applied\""
                ((org-agenda-overriding-header "Applied Jobs")))
          (tags "+STATUS=\"Not applied\""
                ((org-agenda-overriding-header "Not Yet Applied Jobs")))
          (tags "+STATUS=\"Rejected\""
                ((org-agenda-overriding-header "Rejected Jobs")))
          (tags "+STATUS=\"Interviewing\""
                ((org-agenda-overriding-header "Jobs in Interviewing Stage")))
          (tags "+STATUS=\"Assessment\""
                ((org-agenda-overriding-header "Jobs in Assessment Stage")))))))

;; ----------------------------------------------------------------------------
;; Helper Functions for Books
;; ----------------------------------------------------------------------------

(defun update-book-categories ()
  "Update categories based on the Status and Group properties."
  (interactive)
  (org-map-entries
   (lambda ()
     (let ((status (org-entry-get nil "Status"))
           (group (org-entry-get nil "Group")))
       (cond
        ((and (string= group "Reading") (string= status "Reading"))
         (org-set-property "CATEGORY" "Reading"))
        ((and (string= group "Reading") (string= status "To Read"))
         (org-set-property "CATEGORY" "To Read"))
        ((and (string= group "Reading") (string= status "Read"))
         (org-set-property "CATEGORY" "Read"))
        ((and (string= group "Others") (string= status "Reading"))
         (org-set-property "CATEGORY" "Reading"))
        ((and (string= group "Others") (string= status "To Read"))
         (org-set-property "CATEGORY" "To Read"))
        ((and (string= group "Others") (string= status "Read"))
         (org-set-property "CATEGORY" "Read"))
        ((and (string= group "Study") (string= status "Studying"))
         (org-set-property "CATEGORY" "Studying"))
        ((and (string= group "Study") (string= status "To Study"))
         (org-set-property "CATEGORY" "To Study"))
        ((and (string= group "Study") (string= status "Studied D"))
         (org-set-property "CATEGORY" "Studied D"))
        ((and (string= group "Study") (string= status "Additional Study"))
         (org-set-property "CATEGORY" "Additional Study"))
        ((and (string= group "Study") (string= status "Studied A"))
         (org-set-property "CATEGORY" "Studied A"))
        ((and (string= group "Study") (string= status "Reference"))
         (org-set-property "CATEGORY" "Reference")))))))

(defun calculate-book-durations ()
  "Calculate the total days spent and days since finished for books."
  (interactive)
  (org-map-entries
   (lambda ()
     (let* ((started (org-entry-get nil "STARTED"))
            (finished (org-entry-get nil "FINISHED"))
            (current-time (current-time))
            (total-days (when (and started finished)
                          (round (- (float-time (org-time-string-to-time finished))
                                    (float-time (org-time-string-to-time started)))
                                 86400)))
            (days-since-finished (when finished
                                   (round (- (float-time current-time)
                                             (float-time (org-time-string-to-time finished)))
                                          86400))))
       (when total-days
         (org-set-property "TOTAL_DAYS_SPENT" (number-to-string total-days)))
       (when days-since-finished
         (org-set-property "DAYS_SINCE_FINISHED" (number-to-string days-since-finished)))))))

;; Add hooks to update book categories and calculate durations before saving
(add-hook 'org-mode-hook
          (lambda () (add-hook 'before-save-hook 'update-book-categories nil 'local)))

(add-hook 'org-mode-hook
          (lambda () (add-hook 'before-save-hook 'calculate-book-durations nil 'local)))

;; ----------------------------------------------------------------------------
;; Template Insertion Functions
;; ----------------------------------------------------------------------------

(defun insert-new-book-entry ()
  "Insert a new book entry with predefined properties."
  (interactive)
  (org-insert-heading)
  (insert "Book Title\n")
  (org-do-demote)
  (insert ":PROPERTIES:\n")
  (insert ":Author: \n")
  (insert ":Status: \n")
  (insert ":Group: \n")
  (insert ":Started: \n")
  (insert ":CATERGORY: \n")
  (insert ":END:\n")
  (insert "\n"))

(defun insert-job-application-template ()
  "Insert a job application template with properties and a description placeholder."
  (interactive)
  (insert "*** \n:PROPERTIES:\n:STATUS: Not applied\n:DateApplied:\n:END:\n**** desc\n"))

(defun insert-commit-template ()
  "Insert a commit template with placeholders for date, details, and next tasks."
  (interactive)
  (let ((commit-date (format-time-string "%Y-%m-%d %a")))
    (insert (format "**  Commit on <%s>\n" commit-date))
    (insert "*** Details\n")
    (insert "*** What to work on next:\n")
    (insert "*** Commit message:\n")))

;; ----------------------------------------------------------------------------
;; Provide
;; ----------------------------------------------------------------------------

(provide 'custom-agenda)

;;; custom-agenda.el ends here
