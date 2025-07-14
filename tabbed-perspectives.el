;;; tabbed-perspectives-mode.el --- Save and restore tab-bar configs for individual perspectives -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Your Name

;; Author: Michael Schlösser <michael.schloesser@gmail.com>
;; Maintainer: Michael Schlösser <michael.schloesser@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (perspective "20250523.1316"))
;; Keywords: perspective extension tabs tab-bar
;; Homepage: https://github.com/mschloesser/tabbed-perspectives-mode
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Saves and restores individual tab-bar configs for every perspective.
;; Extends perspective.el to include tab-bar configs into individual setups.

;;; Code:

;; Define the customization group

(defgroup tabbed-perspectives nil
  "Perspective enhancements with tabbed interface."
  :group 'convenience
  :prefix "tpersp-")

;;;###autoload
(define-minor-mode tabbed-perspectives-mode
  "Tabbed Perspectives Mode."
  :lighter " TabPersp"
  :global t
  :group 'tabbed-perspectives

  (cond
   ;; Only allow enabling if perspective-mode is active
   ((and tabbed-perspectives-mode (not (bound-and-true-p persp-mode)))
    (message "tabbed-perspectives-mode requires perspective-mode to be active")
    (setq tabbed-perspectives-mode nil))
   
   ;; Mode is being enabled and perspective is active
   (tabbed-perspectives-mode
    (add-hook 'persp-before-switch-hook 'tpersp-persp-before-switch-hook)
    (add-hook 'persp-switch-hook 'tpersp-persp-after-switch-hook)
    (add-hook 'persp-killed-hook 'tpersp-persp-killed-hook)
    (add-hook 'persp-created-hook 'tpersp-persp-created-hook)
    (message "tabbed-perspectives-mode enabled"))
   
   ;; Mode is being disabled
   (t
    (remove-hook 'persp-before-switch-hook 'tpersp-persp-before-switch-hook)
    (remove-hook 'persp-switch-hook 'tpersp-persp-after-switch-hook)
    (remove-hook 'persp-killed-hook 'tpersp-persp-killed-hook)
    (remove-hook 'persp-created-hook 'tpersp-persp-created-hook)
    (message "tabbed-perspectives-mode disabled"))))


(defcustom tpersp-storage-directory (locate-user-emacs-file "tabbed-perspectives/")
  "Directory in which tab-bar configurations for perspectives are saved."
  :type 'directory
  :group 'tabbed-perspectives)


(defun tpersp-cleanup-session-files ()
  "Clean up tab configuration files for the current Emacs process."
  (let ((pattern (concat (regexp-quote 
                         (concat "sess_" (number-to-string (emacs-pid)) "_"))
                        ".*\\.tabs")))
    (dolist (file (directory-files tpersp-storage-directory t pattern))
      (when (file-exists-p file)
        (delete-file file)))))


(defun tpersp-session-file-name ()
  "Generate an unique file path for the current Emacs session and perspective"
  (concat tpersp-storage-directory
	  "sess_"
	  (number-to-string (emacs-pid))
	  "_"
	  (persp-current-name)
	  ".tabs"))


(defun tpersp-reset-tab-bar ()
  "Reset tab-bar for current perspective. Also fixes broken tab-bar config."
  (interactive)
  (tab-bar-tabs-set nil))


(defun tpersp-persp-before-switch-hook ()
  "Store current tab-bar config"
  (let ((tabs (funcall tab-bar-tabs-function)))
    (when tabs
      (unless (file-exists-p tpersp-storage-directory)
	(make-directory tpersp-storage-directory t))
      (with-temp-file (tpersp-session-file-name)
        (prin1 (tpersp--sanitize-tab-data tabs) (current-buffer))))))


(defun tpersp-persp-after-switch-hook ()
  "Load tab-bar config for perspective that was switched to."
  (let ((tpersp-file-name (tpersp-session-file-name)))
    (when (file-exists-p tpersp-file-name)
      (with-temp-buffer
        (insert-file-contents tpersp-file-name)
	(let ((tabs (read (current-buffer))))
	  (tab-bar-tabs-set tabs))))))


(defun tpersp-persp-killed-hook ()
  "Delete config for killed perspective."
  (let ((tpersp-file-name (tpersp-session-file-name)))
    (when (file-exists-p tpersp-file-name)
      (delete-file tpersp-file-name))))


(defun tpersp-persp-created-hook ()
  "Create default config for newly created perspective."
    (unless (file-exists-p tpersp-storage-directory)
      (make-directory tpersp-storage-directory t))
    (with-temp-file (tpersp-session-file-name)
      (prin1 '((current-tab (name . "*scratch*") (explicit-name))) (current-buffer))))


(defun tpersp--sanitize-tab-data (obj)
  "Recursively sanitize output of tab-bar-tabs-function replacing internal
Emacs objects with safe representations."
  (cond
   ;; Basic types are safe
   ((or (stringp obj) (numberp obj) (symbolp obj) (null obj))
    obj)

   ;; If it’s a buffer, replace with buffer name
   ((bufferp obj)
    (buffer-name obj))

   ;; If it’s a marker, replace with marker position (number)
   ((markerp obj)
    (marker-position obj))

   ;; If it’s a window-configuration, replace with a string placeholder
   ((window-configuration-p obj)
    "<window-configuration>")

   ;; If it’s a list or cons cell, recursively tpersp--sanitize each element
   ((consp obj)
    (cons (tpersp--sanitize-tab-data (car obj))
          (tpersp--sanitize-tab-data (cdr obj))))

   ;; If it’s a vector, tpersp--sanitize each element and return vector
   ((vectorp obj)
    (vconcat (mapcar #'tpersp--sanitize-tab-data obj)))

   ;; Otherwise, replace unknown objects with their printed representation string
   (t
    (format "<object: %s>" obj))))


;; Setup function
(defun tpersp-setup ()
  "Setup tabbed-perspectives-mode and attach it to perspective-mode."
  (require 'tab-bar)
  (require 'perspective)

  ;; Ensure cleanup on exit
  (add-hook 'kill-emacs-hook #'tpersp-cleanup-session-files) 

  ;; Make sure tabbed-perspectives-mode syncs with perspective-mode state
  (add-hook 'perspective-mode-hook
            (lambda ()
              (if perspective-mode
                  (tabbed-perspectives-mode 1)
                (tabbed-perspectives-mode -1)))))

(tpersp-setup)

(provide 'tabbed-perspectives-mode)

;;; tabbed-perspectives-mode.el ends here
