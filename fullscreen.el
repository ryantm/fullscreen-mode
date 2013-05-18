;;; fullscreen.el --- fullscreen window support for Emacs

;; Author: Ryan Mulligan <ryan@ryantm.com>
;; URL: https://github.com/ryantm/fullscreen
;; Version: 0.0.1
;; Keywords: fullscreen

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides fullscreen-toggle, bound to F11 that toggles the frame between
;; fullscreen and windowed.

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
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

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


(provide 'fullscreen)
;;; fullscreen.el ends here
