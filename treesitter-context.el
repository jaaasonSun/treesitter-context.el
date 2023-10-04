;;; treesitter-context.el --- Show context information around current point -*- lexical-binding: t; -*-

;; Author: zbelial
;; Maintainer: zbelial
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (posframe "1.4.2"))
;; Homepage: https://bitbucket.org/zbelial/treesitter-context
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
(require 'posframe-plus)

(defgroup treesitter-context nil
  "Show context information around current point."
  :group 'treesit
  :version "28.2")

(defcustom treesitter-context-idle-time 2.0
  "How many seconds to wait before refreshing context information."
  :version "29.1"
  :type 'float
  :safe 'floatp
  :group 'treesitter-context)

(defcustom treesitter-context-show-context-always t
  "If t, show context all the time.
If nil, show context only when the outmost parent is invisible."
  :version "29.1"
  :type 'boolean
  :group 'treesitter-context)

(defcustom treesitter-context-show-line-number t
  "If t, show line number in the child frame."
  :version "29.1"
  :type 'boolean
  :group 'treesitter-context)

(defcustom treesitter-context-frame-autohide-timeout 15
  "Child frame will hide itself after this seconds."
  :version "29.1"
  :type 'integer
  :safe 'integerp
  :group 'treesitter-context)

(defcustom treesitter-context-frame-indent-offset 4
  "Indent offset in the child frame."
  :version "29.1"
  :type 'integer
  :safe 'integerp
  :group 'treesitter-context)

(defcustom treesitter-context-frame-min-width 60
  "Minimal width of the child frame."
  :version "29.1"
  :type 'integer
  :safe 'integerp
  :group 'treesitter-context)

(defcustom treesitter-context-frame-min-height 5
  "Minimal height of the child frame."
  :version "29.1"
  :type 'integer
  :safe 'integerp
  :group 'treesitter-context)

(defcustom treesitter-context-frame-font nil
  "Font of the child frame."
  :version "29.1"
  :type 'string
  :group 'treesitter-context)

(defvar treesitter-context--buffer-name "*treesitter-context*"
  "Name of buffer used to show context.")

(defvar treesitter-context--indent-level 0
  "Indent level used to generate context information.")

(defvar-local treesitter-context--refresh-timer nil
  "Idle timer for refreshing context.")

(defvar-local treesitter-context--context-list nil
  "A list storing context. The outmost one is at the front.")

(defvar-local treesitter-context--child-frame nil
  "Child frame showing the context.")

(defun treesitter-context--posframe-hidehandler-when-buffer-change (info)
  "Posframe hidehandler function.

This function let posframe hide when user switch buffer/kill buffer.
See `posframe-show' for more infor about hidehandler and INFO ."
  (let ((parent-buffer (cdr (plist-get info :posframe-parent-buffer))))
    (or (not (buffer-live-p parent-buffer))
        (and (buffer-live-p parent-buffer)
             (not (equal parent-buffer (current-buffer)))))))

(defun treesitter-context--string-pad-left (s len)
  (let ((extra (max 0 (- len (length s)))))
    (concat (make-string extra ?\s) s)))

(defun treesitter-context--string-pad-right (s len)
  (let ((extra (max 0 (- len (length s)))))
    (concat s (make-string extra ?\s))))

(defun treesitter-context--show-context ()
  "Show context in a child frame."
  (let* ((buffer (get-buffer-create treesitter-context--buffer-name))
         (contexts treesitter-context--context-list)
         (bg-mode (frame-parameter nil 'background-mode))
         (background-color
          (cond ((eq bg-mode 'dark)
                 (treesitter-context--color-blend (face-background 'default) "#000000" 0.1))
                ((eq bg-mode 'light)
                 (treesitter-context--color-blend (face-background 'default) "#000000" 0.8))))
         (max-line-no 0)
         (prefix-len 0)
         (line-no-prefix "")
         (blank-prefix "")
         (padding "  ")
         first-line-p)
    (when treesitter-context-show-line-number
      (cl-dolist (context contexts)
        (when (> (car context) max-line-no)
          (setq max-line-no (car context))))
      (setq prefix-len (length (format "%s" max-line-no)))
      (setq blank-prefix (make-string prefix-len ?\s)))
    (with-current-buffer buffer
      (erase-buffer)
      (goto-char (point-min))
      (if treesitter-context-show-line-number
          (progn
            (cl-dolist (context contexts)
              (setq first-line-p t)
              (cl-dolist (line (cdr context))
                (if first-line-p
                    (progn
                      (insert (concat (treesitter-context--string-pad-left (format "%s" (car context)) prefix-len) padding line))
                      (setq first-line-p nil))
                  (insert (concat blank-prefix padding line))))))
        (cl-dolist (context contexts)
          (cl-dolist (line (cdr context))
            (insert line)))))
    (setq treesitter-context--child-frame (posframe-plus-show buffer nil nil
                                                              :poshandler #'posframe-poshandler-window-top-right-corner
                                                              :font treesitter-context-frame-font
                                                              :border-width 1
                                                              :background-color background-color
                                                              :internal-border-color "orange"
                                                              :internal-border-width 1
                                                              :min-width (min (max treesitter-context-frame-min-width (/ (window-width) 2)) (window-width))
                                                              :min-height treesitter-context-frame-min-height
                                                              :accept-focus nil
                                                              :hidehandler #'treesitter-context--posframe-hidehandler-when-buffer-change
                                                              :timeout treesitter-context-frame-autohide-timeout)))
  nil)

(defun treesitter-context--refresh-context ()
  "Refresh context at the point."
  (unless (or (minibufferp)
              (equal (buffer-name) treesitter-context--buffer-name))
    (setq treesitter-context--context-list nil)
    (ignore-errors
      (setq treesitter-context--context-list (treesitter-context-collect-contexts))
      (treesitter-context--show-context))))

(defun treesitter-context--refresh-when-idle ()
  (when treesitter-context--refresh-timer
    (cancel-timer treesitter-context--refresh-timer)
    (setq treesitter-context--refresh-timer nil))
  (setq treesitter-context--refresh-timer (run-with-idle-timer treesitter-context-idle-time nil #'treesitter-context--refresh-context)))

(defun treesitter-context--hide-frame ()
  (posframe-hide (get-buffer treesitter-context--buffer-name)))

(define-minor-mode treesitter-context-mode
  "Show context information in treesit-based mode."
  :init-value nil
  :lighter " TC"
  (if treesitter-context-mode
      (progn
        (if (and (treesit-available-p)
                 (posframe-workable-p)
                 (member major-mode treesitter-context--supported-mode))
            (progn
              (add-hook 'post-command-hook #'treesitter-context--refresh-when-idle nil t)
              (treesitter-context--refresh-context))
          (setq treesitter-context-mode nil)))
    (when treesitter-context--refresh-timer
      (cancel-timer treesitter-context--refresh-timer)
      (setq treesitter-context--refresh-timer nil))
    (treesitter-context--hide-frame)
    (remove-hook 'post-command-hook #'treesitter-context--refresh-when-idle t)))

(require 'treesitter-context-java)
(require 'treesitter-context-python)
(require 'treesitter-context-c)
(require 'treesitter-context-cpp)
(require 'treesitter-context-rust)
(require 'treesitter-context-go)
(require 'treesitter-context-json)
(require 'treesitter-context-javascript)
(require 'treesitter-context-typescript)
(require 'treesitter-context-yaml)
(require 'treesitter-context-toml)

(provide 'treesitter-context)
