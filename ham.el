;;; ham.el --- a ham toolkit for Emacs users           -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: IRendy <irendy@qq.com>

;;; Commentary:

;;; Todo:
;; M-x ham-mode
;; Logbook log search filter
;; expand:
;; ham-calculate-wavelength ham-dipole-length
;; ham-query-callsign
;; control rig: ham-rig-interface
;; QSL card control


;;; Code:
(require 'cl-lib)
(require 'seq)
(require 'json)

(defgroup ham-mode nil
  "Emacs mode for Amateur Radio (HAM) operators."
  :group 'external
  :prefix "ham-")

(defcustom ham-log-file (expand-file-name "ham_log.org" user-emacs-directory)
  "File path for saving the HAM radio contact log."
  :type 'file
  :group 'ham-mode)

(defvar ham-log-entries (list)
  "List of ham radio contact log entries.")

(defun ham-add-entry (&optional call-sign frequency mode rst-sent rst-recv notes)
  "Add a new entry to the HAM radio log.
CALL-SIGN is the other party's call sign.
FREQUENCY is in MHz.
MODE is the communication mode (e.g., 'SSB, 'CW, 'FM).
RST-SENT is the signal report you sent.
RST-RECV is the signal report you received.
NOTES is a string for any additional notes."
  (interactive
   (list
    (read-string "Call Sign: ")
    (read-number "Frequency (MHz): " 14.250)
    (intern (completing-read "Mode: " '(SSB CW FM DIGITAL) nil t))
    (read-string "RST Sent: " "59")
    (read-string "RST Received: " "59")
    (read-string "Notes: ")))
  (let ((entry (list :call-sign call-sign
                     :timestamp (format-time-string "%Y-%m-%d %H:%M UTC" nil t)
                     :frequency frequency
                     :mode mode
                     :rst-sent rst-sent
                     :rst-recv rst-recv
                     :notes notes)))
    (push entry ham-log-entries)
    (ham--save-log)
    (message "Logged contact with %s" call-sign)))

(defun ham--save-log ()
  "Save the current log entries to `ham-log-file' in Org-mode format."
  (with-temp-buffer
    (insert "#+TITLE: Amateur Radio (HAM) Contact Log\n\n")
    (insert "| Call Sign | Date/Time (UTC) | Frequency (MHz) | Mode | RST Sent | RST Recv | Notes |\n")
    (insert "|-----------+-----------------+-----------------+------+----------+----------+-------|\n")
    (dolist (entry (reverse ham-log-entries))
      (insert (format "| %s | %s | %.3f | %s | %s | %s | %s |\n"
                      (plist-get entry :call-sign)
                      (plist-get entry :timestamp)
                      (plist-get entry :frequency)
                      (plist-get entry :mode)
                      (plist-get entry :rst-sent)
                      (plist-get entry :rst-recv)
                      (plist-get entry :notes))))
    (write-region (point-min) (point-max) ham-log-file)))

(defun ham-load-log ()
  "Load the log entries from `ham-log-file'."
  (interactive)
  (setq ham-log-entries (list))
  (when (file-exists-p ham-log-file)
    (with-temp-buffer
      (insert-file-contents ham-log-file)
      (goto-char (point-min))
      (while (re-search-forward "^| \\([^|]+\\) | \\([^|]+\\) | \\([^|]+\\) | \\([^|]+\\) | \\([^|]+\\) | \\([^|]+\\) | \\([^|]+\\) |" nil t)
        (let ((entry (list :call-sign (match-string 1)
                           :timestamp (match-string 2)
                           :frequency (string-to-number (match-string 3))
                           :mode (intern (match-string 4))
                           :rst-sent (match-string 5)
                           :rst-recv (match-string 6)
                           :notes (match-string 7))))
          (push entry ham-log-entries))))
  (message "Loaded %d log entries." (length ham-log-entries)))

(defun ham-view-log ()
  "Display the HAM radio contact log in a new buffer."
  (interactive)
  (ham-load-log)
  (let ((buffer (get-buffer-create "*HAM Log*")))
    (switch-to-buffer buffer)
    (with-current-buffer buffer
      (erase-buffer)
      (org-mode)
      (insert "#+TITLE: Amateur Radio (HAM) Contact Log\n\n")
      (if ham-log-entries
          (progn
            (insert "| Call Sign | Date/Time (UTC) | Frequency (MHz) | Mode | RST Sent | RST Recv | Notes |\n")
            (insert "|-----------+-----------------+-----------------+------+----------+----------+-------|\n")
            (dolist (entry (reverse ham-log-entries))
              (insert (format "| %s | %s | %.3f | %s | %s | %s | %s |\n"
                              (plist-get entry :call-sign)
                              (plist-get entry :timestamp)
                              (plist-get entry :frequency)
                              (plist-get entry :mode)
                              (plist-get entry :rst-sent)
                              (plist-get entry :rst-recv)
                              (plist-get entry :notes))))
            (goto-char (point-min)))
        (insert "No log entries found.")))))

(defun ham-search-log (search-term)
  "Search the HAM radio log for entries matching SEARCH-TERM."
  (interactive "sSearch log for: ")
  (ham-load-log)
  (let ((matching-entries
         (seq-filter (lambda (entry)
                       (cl-some (lambda (field)
                                  (string-match-p search-term (format "%s" (plist-get entry field))))
                                '(:call-sign :timestamp :frequency :mode :rst-sent :rst-recv :notes)))
                     ham-log-entries)))
    (if matching-entries
        (progn
          (message "Found %d matching entries." (length matching-entries))
          (let ((buffer (get-buffer-create "*HAM Search Results*")))
            (switch-to-buffer buffer)
            (erase-buffer)
            (org-mode)
            (insert (format "#+TITLE: HAM Log Search Results for: %s\n\n" search-term))
            (insert "| Call Sign | Date/Time (UTC) | Frequency (MHz) | Mode | RST Sent | RST Recv | Notes |\n")
            (insert "|-----------+-----------------+-----------------+------+----------+----------+-------|\n")
            (dolist (entry (reverse matching-entries))
              (insert (format "| %s | %s | %.3f | %s | %s | %s | %s |\n"
                              (plist-get entry :call-sign)
                              (plist-get entry :timestamp)
                              (plist-get entry :frequency)
                              (plist-get entry :mode)
                              (plist-get entry :rst-sent)
                              (plist-get entry :rst-recv)
                              (plist-get entry :notes))))
            (goto-char (point-min))))
      (message "No matching entries found."))))

;; Keymap and mode definition
(defvar ham-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c h a") 'ham-add-entry)
    (define-key map (kbd "C-c h v") 'ham-view-log)
    (define-key map (kbd "C-c h s") 'ham-search-log)
    map)
  "Keymap for HAM mode.")

(define-minor-mode ham-mode
  "Toggle HAM mode for Amateur Radio operations.
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode."
  :init-value nil
  :lighter " HAM"
  :keymap ham-mode-map
  :global t
  :group 'ham-mode
  (if ham-mode
      (progn
        (ham-load-log)
        (message "HAM mode enabled"))
    (message "HAM mode disabled")))

(provide 'ham-mode)
(provide 'ham)
;;; ham.el ends here.
