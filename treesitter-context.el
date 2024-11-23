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
(require 'posframe)

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

(defcustom treesitter-context-frame-min-height 0
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

(defcustom treesitter-context-frame-font-fraction nil
  "Fraction of font height in the child frame. Prefer this to `treesitter-context-frame-font'."
  :version "29.1"
  :type 'float
  :safe 'floatp
  :group 'treesitter-context)

(defvar treesitter-context--buffer-name "*treesitter-context*"
  "Name of buffer used to show context.")

(defvar treesitter-context--indent-level 0
  "Indent level used to generate context information.")

(defvar treesitter-context-background-color "#1d1f21"
  "Background color for treesitter-context posframe")

(defvar treesitter-context-border-color "#FFFFFF"
  "Border color for treesitter-context posframe")

(defvar treesitter-context-border-width 0
  "Border width for treesitter-context posframe")

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

(defun treesitter-context-poshandler (info)
    (cons (+ (plist-get info :parent-window-left)
             (* (frame-char-width) (+ (fringe-columns 'left)
                                      (car (window-margins)))))
          (plist-get info :parent-window-top)))

(defun treesitter-context--show-context ()
  "Show context in a child frame."
  (let* ((buffer (get-buffer-create treesitter-context--buffer-name))
         (contexts treesitter-context--context-list)
         (bg-mode (frame-parameter nil 'background-mode))
         (prefix-len 0)
         (line-no-prefix "")
         (blank-prefix "")
         (padding " ")
         (font-height (face-attribute 'default :height))
         first-line-p
         (line-count 1)
         (linum-width (line-number-display-width))
         visible-pos)
    (when (> linum-width 0)
      (setq prefix-len (1+ linum-width))
      (setq blank-prefix (make-string prefix-len ?\s)))
    (with-current-buffer buffer
      (erase-buffer)
      (goto-char (point-min))
      (if (> linum-width 0)
          (progn
            (cl-dolist (context contexts)
              (setq first-line-p t)
              (cl-dolist (line (cdr context))
                (setq line-count (1+ line-count))
                (if first-line-p
                    (progn
                      (insert (concat (propertize (treesitter-context--string-pad-left (format "%s" (car context)) prefix-len) 'face 'line-number) padding line))
                      (setq first-line-p nil))
                  (insert (concat blank-prefix padding line))))))
        (cl-dolist (context contexts)
          (cl-dolist (line (cdr context))
            (setq line-count (1+ line-count))
            (insert line)))))
    (when (and font-height
               treesitter-context-frame-font-fraction
               (> treesitter-context-frame-font-fraction 0.0))
      (setq treesitter-context-frame-font nil)
      (setq font-height (round (* font-height treesitter-context-frame-font-fraction))))
    (setq treesitter-context--child-frame (posframe-show buffer
                                                         :poshandler #'treesitter-context-poshandler
                                                         :font treesitter-context-frame-font
                                                         :border-width treesitter-context-border-width
                                                         :background-color treesitter-context-background-color
                                                         :internal-border-color treesitter-context-border-color
                                                         :internal-border-width treesitter-context-border-width
                                                         :min-width (window-width)
                                                         :min-height treesitter-context-frame-min-height
                                                         :accept-focus nil
                                                         :hidehandler #'treesitter-context--posframe-hidehandler-when-buffer-change
                                                         :timeout treesitter-context-frame-autohide-timeout
                                                         :lines-truncate t
                                                         :override-parameters '((alpha-background . 100))))
    (when font-height
      (set-face-attribute 'default treesitter-context--child-frame :height font-height))
    (setq visible-pos (save-excursion (goto-char (window-start)) (forward-line line-count) (point)))
    (while (< (point) visible-pos) (next-line)))
  nil)

(defun treesitter-context--refresh-context ()
  "Refresh context at the point."
  (unless (or (minibufferp)
              (equal (buffer-name) treesitter-context--buffer-name))
    (setq treesitter-context--context-list nil)
    (ignore-errors
      (setq treesitter-context--context-list (treesitter-context-collect-contexts))
      (if (length> treesitter-context--context-list 0)
          (treesitter-context--show-context)
        (treesitter-context--hide-frame)))))

(defun treesitter-context--refresh-when-idle ()
  (when treesitter-context--refresh-timer
    (cancel-timer treesitter-context--refresh-timer)
    (setq treesitter-context--refresh-timer nil))
  (setq treesitter-context--refresh-timer (run-with-idle-timer treesitter-context-idle-time nil #'treesitter-context--refresh-context)))

(defun treesitter-context--hide-frame ()
  (posframe-hide (get-buffer treesitter-context--buffer-name)))

;;;###autoload
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
