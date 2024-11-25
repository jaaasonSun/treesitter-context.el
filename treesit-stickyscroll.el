;;; treesit-stickyscroll.el --- Show context information around current point -*- lexical-binding: t; -*-

;; Author: zbelial
;; Maintainer: zbelial
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (posframe "1.4.2"))
;; Homepage: https://bitbucket.org/zbelial/treesit-stickyscroll
;; Keywords: Package Emacs


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Require 
(require 'cl-lib)
(require 'treesit)
(require 'posframe)

(defgroup treesit-stickyscroll nil
  "Show context information around current point."
  :group 'treesit
  :version "28.2")

(defcustom treesit-stickyscroll-idle-time 2.0
  "How many seconds to wait before refreshing context information."
  :version "29.1"
  :type 'float
  :safe 'floatp
  :group 'treesit-stickyscroll)

(defcustom treesit-stickyscroll-frame-autohide-timeout 15
  "Child frame will hide itself after this seconds."
  :version "29.1"
  :type 'integer
  :safe 'integerp
  :group 'treesit-stickyscroll)

(defvar treesit-stickyscroll--buffer-name "*treesit-stickyscroll*"
  "Name of buffer used to show context.")

(defvar treesit-stickyscroll-background-color "#1d1f21"
  "Background color for treesit-stickyscroll posframe")

(defvar treesit-stickyscroll-border-color "#FFFFFF"
  "Border color for treesit-stickyscroll posframe")

(defvar treesit-stickyscroll-border-width 0
  "Border width for treesit-stickyscroll posframe")

(defvar-local treesit-stickyscroll--refresh-timer nil
  "Idle timer for refreshing context.")

(defvar-local treesit-stickyscroll--context-list nil
  "A list storing context. The outmost one is at the front.")

(defvar-local treesit-stickyscroll--child-frame nil
  "Child frame showing the context.")

(defun treesit-stickyscroll--posframe-hidehandler-when-buffer-change (info)
  "Posframe hidehandler function.

This function let posframe hide when user switch buffer/kill buffer.
See `posframe-show' for more infor about hidehandler and INFO ."
  (let ((parent-buffer (cdr (plist-get info :posframe-parent-buffer))))
    (or (not (buffer-live-p parent-buffer))
        (and (buffer-live-p parent-buffer)
             (not (equal parent-buffer (current-buffer)))))))

(defun treesit-stickyscroll--string-pad-left (s len)
  (let ((extra (max 0 (- len (length s)))))
    (concat (make-string extra ?\s) s)))

(defun treesit-stickyscroll--string-pad-right (s len)
  (let ((extra (max 0 (- len (length s)))))
    (concat s (make-string extra ?\s))))

(defun treesit-stickyscroll-poshandler (info)
    (cons (+ (plist-get info :parent-window-left)
             (* (frame-char-width) (+ (fringe-columns 'left)
                                      (car (window-margins)))))
          (plist-get info :parent-window-top)))

(defun treesit-stickyscroll--show-context ()
  "Show context in a child frame."
  (let* ((buffer (get-buffer-create treesit-stickyscroll--buffer-name))
         (contexts treesit-stickyscroll--context-list)
         (bg-mode (frame-parameter nil 'background-mode))
         (prefix-len 0)
         (blank-prefix "")
         (padding " ")
         (font-height (face-attribute 'default :height))
         first-line-p
         (line-count 1)
         (linum-width (line-number-display-width))
         visible-pos
         (width (window-width)))
    (when (> linum-width 0)
      (setq prefix-len (1+ linum-width))
      (setq blank-prefix (make-string prefix-len ?\s)))
    (with-current-buffer buffer
      (erase-buffer)
      (goto-char (point-min))
      (cl-dolist (context contexts)
        (setq first-line-p t)
        (cl-dolist (line (cdr context))
          (setq line-count (1+ line-count))
          (if (> linum-width 0)
              (if first-line-p
                  (setq line (concat (propertize (treesit-stickyscroll--string-pad-left (format "%s" (car context)) prefix-len) 'face 'line-number) padding line))
                (setq line (concat blank-prefix padding line))))
          (setq first-line-p nil)
          (if (length> line width)
              (setq line (concat (substring line 0 width) "\n")))
          (insert line)))
      (goto-char (point-max))
      (backward-char)
      (add-face-text-property (line-beginning-position) (point-max) `(:underline (:color ,(face-foreground 'shadow) :position t) :extend t)))
    (setq treesit-stickyscroll--child-frame (posframe-show buffer
                                                         :poshandler #'treesit-stickyscroll-poshandler
                                                         :font nil
                                                         :border-width 0
                                                         :background-color treesit-stickyscroll-background-color
                                                         :internal-border-width 0
                                                         :min-width (window-width)
                                                         :min-height 0
                                                         :accept-focus nil
                                                         :hidehandler #'treesit-stickyscroll--posframe-hidehandler-when-buffer-change
                                                         :timeout treesit-stickyscroll-frame-autohide-timeout))
    (setq visible-pos (save-excursion (goto-char (window-start)) (forward-line line-count) (point)))
    (while (< (point) visible-pos) (next-line)))
  nil)

(defun treesit-stickyscroll--refresh-context ()
  "Refresh context at the point."
  (unless (or (minibufferp)
              (equal (buffer-name) treesit-stickyscroll--buffer-name))
    (setq treesit-stickyscroll--context-list nil)
    (ignore-errors
      (if (eq 1 (window-start))
          (treesit-stickyscroll--hide-frame)
        (setq treesit-stickyscroll--context-list (treesit-stickyscroll-collect-contexts))
        (if (length= treesit-stickyscroll--context-list 0)
            (treesit-stickyscroll--hide-frame)
          (treesit-stickyscroll--show-context))))))

(defun treesit-stickyscroll--refresh-when-idle ()
  (when treesit-stickyscroll--refresh-timer
    (cancel-timer treesit-stickyscroll--refresh-timer)
    (setq treesit-stickyscroll--refresh-timer nil))
  (setq treesit-stickyscroll--refresh-timer (run-with-idle-timer treesit-stickyscroll-idle-time nil #'treesit-stickyscroll--refresh-context)))

(defun treesit-stickyscroll--hide-frame ()
  (posframe-hide (get-buffer treesit-stickyscroll--buffer-name)))

;;;###autoload
(define-minor-mode treesit-stickyscroll-mode
  "Show context information in treesit-based mode."
  :init-value nil
  :lighter " TSS"
  (if treesit-stickyscroll-mode
      (progn
        (if (and (treesit-available-p)
                 (posframe-workable-p)
                 (member major-mode treesit-stickyscroll--supported-mode))
            (progn
              (add-hook 'post-command-hook #'treesit-stickyscroll--refresh-when-idle nil t)
              (treesit-stickyscroll--refresh-context))
          (setq treesit-stickyscroll-mode nil)))
    (when treesit-stickyscroll--refresh-timer
      (cancel-timer treesit-stickyscroll--refresh-timer)
      (setq treesit-stickyscroll--refresh-timer nil))
    (treesit-stickyscroll--hide-frame)
    (remove-hook 'post-command-hook #'treesit-stickyscroll--refresh-when-idle t)))

(require 'treesit-stickyscroll-c)
(require 'treesit-stickyscroll-cpp)

(provide 'treesit-stickyscroll)
