;;; fullscreen-mode.el --- fullscreen window support for Emacs

;; Author: Ryan Mulligan <ryan@ryantm.com>
;; Version: 0.0.1
;; URL: https://github.com/ryantm/fullscreen-mode
;; Keywords: fullscreen, fullscreen-mode

;; This file is not part of GNU Emacs.

;;; Install:

;;  (package-install 'fullscreen-mode)

;;; Commentary:

;; Initially-on global minor mode that provides fullscreen-toggle,
;; which toggles the frame between fullscreen and windowed.
;; fullscreen-toggle is bound to F11.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Code:
(defvar fullscreen-windowed-frame-state (make-hash-table :weakness 'key)
  "Hash keyed by frames with the value of the fullscreen frame parameter before going to fullscreen.
 Stored so fullscreen-toggle can go back to it.")

(defun fullscreen-windowed-frame-state-update ()
  "Save fullscreen-windowed-frame-state with the current frame-parameter state"
  (let ((fullscreen-frame-parameter (frame-parameter nil 'fullscreen)))
    (if (not (equal fullscreen-frame-parameter 'fullboth))
        (puthash
         (selected-frame)
         fullscreen-frame-parameter
         fullscreen-windowed-frame-state))))

(defun fullscreen-windowed-frame-state-restore ()
  "Restore the frame-parameter stored in fullscreen-windowed-frame-state"
  (let ((fullscreen-frame-parameter (gethash (selected-frame) fullscreen-windowed-frame-state)))
    (set-frame-parameter nil 'fullscreen fullscreen-frame-parameter)))

(defun fullscreen-p ()
  "Predicate for fullscreen frame parameter being set to 'fullboth"
  (equal (frame-parameter nil 'fullscreen) 'fullboth))

;;;###autoload
(defun fullscreen ()
  "Sets frame's fullscreen parameter to fullboth"
  (interactive)
  (fullscreen-windowed-frame-state-update)
  (set-frame-parameter nil 'fullscreen 'fullboth))

;;;###autoload
(defun fullscreen-windowed ()
  "Set frame's fullscreen parameter back to it's previous windowed state"
  (interactive)
  (fullscreen-windowed-frame-state-restore))

;;;###autoload
(defun fullscreen-toggle ()
  "Toggles the frame's fullscreen state"
  (interactive)
  (if (fullscreen-p)
      (fullscreen-windowed)
    (fullscreen)))

;;;###autoload
(defvar fullscreen-mode-map
  (make-sparse-keymap)
  "fullscreen minor mode keymap binds F11 to fullscreen-toggle")

;;;###autoload
(define-key fullscreen-mode-map (kbd "<f11>") 'fullscreen-toggle)

;;;###autoload
(add-to-list 'minor-mode-map-alist `(fullscreen-mode . ,fullscreen-mode-map) t)

;;;###autoload
(define-minor-mode fullscreen-mode
  " Provides fullscreen-mode-toggle, bound to F11 that toggles the frame between fullscreen and windowed."
  :init-value t
  :global t
  :keymap fullscreen-mode-map)

(provide 'fullscreen-mode)
;;; fullscreen-mode.el ends here
