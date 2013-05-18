;;; fullscreen.el --- fullscreen window support for Emacs

;;; Author: Ryan Mulligan <ryan@ryantm.com>
;;; Url: https://github.com/ryantm/fullscreen
;;; Version: 0.0.1

;; Fullscreen

(defvar windowed-frame-state (frame-parameter nil 'fullscreen)
  "State of the frame-parameter for the nil frame. Stored so fullscreen-toggle
can go back to it.")

(defun save-windowed-frame-state ()
  "Save windowed-frame-state with the current frame-parameter state"
  (let ((fullscreen-frame-parameter (frame-parameter nil 'fullscreen)))
    (if (not (equal fullscreen-frame-parameter 'fullboth))
      (setq windowed-frame-state fullscreen-frame-parameter))))

(defun fullscreen-toggle ()
  "Toggles the frame's fullscreen state"
  (interactive)
  (if (fullscreen?)
      (windowed)
    (fullscreen)))

(global-set-key [f11] 'fullscreen-toggle)

(defun fullscreen? ()
  "Predicate for fullscreen frame parameter being set to 'fullboth"
  (equal (frame-parameter nil 'fullscreen) 'fullboth))

(defun fullscreen ()
  "Sets frame's fullscreen parameter to fullboth"
  (interactive)
  (save-windowed-frame-state)
  (set-frame-parameter nil 'fullscreen 'fullboth))

(defun windowed ()
  "Set frame's fullscreen parameter back to it's previous windowed state"
  (interactive)
  (set-frame-parameter nil 'fullscreen windowed-frame-state))

;;; fullscreen.el ends here
