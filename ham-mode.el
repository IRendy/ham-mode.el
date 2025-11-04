;;; ham-mode.el --- a ham toolkit for Emacs users           -*- lexical-binding: t; -*-

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
    (intern (completing-read "Mode: " '(SSB CW FM DIGITAL WEB) nil t))
    (read-string "RST Sent: " "59")
    (read-string "RST Received: " "59")
    (read-string "Notes: ")))
  (let ((entry (list :call-sign (upcase call-sign)
                     :timestamp (format-time-string "%Y-%m-%d %H:%M UTC" nil t)
                     :frequency frequency
                     :mode mode
                     :rst-sent rst-sent
                     :rst-recv rst-recv
                     :notes notes)))
    (push entry ham-log-entries)
    (ham-save-log)
    (message "Logged contact with %s" call-sign)))

(defun ham-save-log ()
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
  (message "Loaded %d log entries." (length ham-log-entries))))

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

(defconst light-speed 299792458 "Amateur const number (Unit: m/s).")

(defconst ham-band-frequencies
  '((1.8 . 2.0)    ; 160m
    (3.5 . 4.0)    ; 80m
    (7.0 . 7.3)    ; 40m
    (10.1 . 10.15) ; 30m
    (14.0 . 14.35) ; 20m
    (18.068 . 18.168) ; 17m
    (21.0 . 21.45) ; 15m
    (24.89 . 24.99) ; 12m
    (28.0 . 29.7)  ; 10m
    (50.0 . 54.0)  ; 6m
    (144.0 . 148.0) ; 2m
    (430.0 . 440.0)) ; 70cm
  "Amateur radio band frequencies in MHz (lower . upper).")

(defun ham-calculate-wavelength (&optional frequency)
  "Calculate wavelength in meters for given FREQUENCY in MHz.
Warns if frequency is outside common amateur radio bands."
  (interactive
   (list (read-number "Frequency (MHz): " 14.250)))

  (unless frequency
    (error "Frequency must be provided"))

  (when (<= frequency 0)
    (error "Frequency must be positive"))

  (let ((wavelength (/ light-speed (* frequency 1e6)))
        (in-band-p nil)
        (suggested-band nil))

    ;; Check if frequency is in amateur bands
    (dolist (band ham-band-frequencies)
      (when (and (>= frequency (car band))
                 (<= frequency (cdr band)))
        (setq in-band-p t)
        (setq suggested-band band)))

    (if (called-interactively-p 'any)
        (progn
          (message "Wavelength for %.3f MHz is: %.3f meters%s"
                   frequency wavelength
                   (if in-band-p
                       " (in amateur band)"
                     (format " (not in amateur bands - try %.1f-%.1f MHz)"
                             (car (car ham-band-frequencies))
                             (cdr (car (last ham-band-frequencies)))))))
      wavelength)))

(defconst ham-band-wavelengths
  '((160 . (1.8 . 2.0))    ; 160m band
    (80  . (3.5 . 4.0))    ; 80m band
    (40  . (7.0 . 7.3))    ; 40m band
    (30  . (10.1 . 10.15)) ; 30m band
    (20  . (14.0 . 14.35)) ; 20m band
    (17  . (18.068 . 18.168)) ; 17m band
    (15  . (21.0 . 21.45)) ; 15m band
    (12  . (24.89 . 24.99)) ; 12m band
    (10  . (28.0 . 29.7))  ; 10m band
    (6   . (50.0 . 54.0))  ; 6m band
    (2   . (144.0 . 148.0)) ; 2m band
    (0.7 . (430.0 . 440.0))) ; 70cm band
  "Amateur radio bands with wavelength in meters and frequency range in MHz.")

(defun ham-calculate-frequency (&optional wavelength)
  "Calculate frequency in MHz for given WAVELENGTH in meters.
Also identifies the corresponding amateur radio band."
  (interactive
   (list (read-number "Wavelength (meters): " 21.037)))

  (unless wavelength
    (error "Wavelength must be provided"))

  (when (<= wavelength 0)
    (error "Wavelength must be positive"))

  (let ((frequency (/ light-speed (* wavelength 1e6)))
        (band-name nil)
        (band-freq nil))

    ;; Find the corresponding amateur band
    (dolist (band ham-band-wavelengths)
      (let ((band-wavelength (car band))
            (freq-range (cdr band)))
        (when (and (>= frequency (car freq-range))
                   (<= frequency (cdr freq-range)))
          (setq band-name (format "%sm" band-wavelength))
          (setq band-freq freq-range))))

    (if (called-interactively-p 'any)
        (progn
          (message "Frequency for %.3f meters is: %.3f MHz%s"
                   wavelength frequency
                   (if band-name
                       (format " (%s band: %.1f-%.1f MHz)"
                               band-name (car band-freq) (cdr band-freq))
                     " (not in amateur bands)")))
      frequency)))


(provide 'ham-mode)
;;; ham-mode.el ends here.
