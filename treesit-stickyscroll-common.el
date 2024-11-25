;;; treesit-stickyscroll-common.el --- Show context information around current point -*- lexical-binding: t; -*-

(require 'treesit)
(require 'cl-generic)
(require 'cl-lib)
(require 'seq)

(defvar treesit-stickyscroll--supported-mode nil
  "Major modes that are support by `treesit-stickyscroll-mode'.")

(defvar treesit-stickyscroll--fold-supported-mode nil
  "Major modes that are support by `treesit-stickyscroll-fold-mode'.")

(defvar treesit-stickyscroll--focus-supported-mode nil
  "Major modes that are support by `treesit-stickyscroll-focus-mode'.")

(defvar treesit-stickyscroll--which-func-supported-mode nil
  "Major modes that are support by treesit-stickyscroll which-func.")

;;; general
(defun treesit-stickyscroll--color-blend (c1 c2 alpha)
  "Blend two colors C1 and C2 with ALPHA. C1 and C2 are hexidecimal strings.
ALPHA is a number between 0.0 and 1.0 which corresponds to the influence of C1 on the result."
  (apply #'(lambda (r g b)
             (format "#%02x%02x%02x"
                     (ash r -8)
                     (ash g -8)
                     (ash b -8)))
         (cl-mapcar
          (lambda (x y)
            (round (+ (* x alpha) (* y (- 1 alpha)))))
          (color-values c1) (color-values c2))))

(defun treesit-stickyscroll--parent-nodes (node-types point)
  "Get the parent nodes whose node type is in NODE-TYPES from POINT."
  (unless (or (minibufferp)
              (equal (buffer-name) treesit-stickyscroll--buffer-name))
    (ignore-errors
      (let ((node (treesit-node-at point))
            node-type parents)
        (while node
          (setq node-type (treesit-node-type node))
          (when (member node-type node-types)
            (cl-pushnew node parents))
          (setq node (treesit-node-parent node)))
        parents))))

;;; context
(defun treesit-stickyscroll--capture (node query &optional beg end node-only)
  "Capture nodes and return them as a pair.
The car of the pair is context, and the cdr is context.end."
  (let (captures
        index
        total
        result
        first
        second
        third)
    (setq captures (treesit-query-capture node query (or beg (treesit-node-start node)) (or end (1+ (point)))))
    (when captures
      (setq index 0)
      (setq total (length captures))
      (while (< index total)
        (setq first (nth index captures)
              second (nth (1+ index) captures)
              third (nth (+ index 2) captures))
        (cond
         ((and (eq (car first) 'context)
               (eq (car second) 'context.real)
               (eq (car third) 'context.end))
          (cl-pushnew (list first second third) result)
          (setq index (+ index 3)))
         ((and (eq (car first) 'context)
               (eq (car second) 'context.end))
          (cl-pushnew (list first second) result)
          (setq index (+ index 2)))
         ((and (eq (car first) 'context)
               (eq (car second) 'context))
          (cl-pushnew (list first) result)
          (setq index (1+ index)))
         ((and (eq (car first) 'context)
               (eq second nil))
          (cl-pushnew (list first) result)
          (setq index (1+ index)))
         (t
          (setq index (1+ index)))))
      (setq result (nreverse result)))
    result))

(defun treesit-stickyscroll-indent-context (context)
  (let ((lines (string-split context "\n" t))
        result)
    (cl-dolist (line lines)
      (when (length> (string-trim line) 0)
        (cl-pushnew (concat line "\n") result)))
    (nreverse result)))

;; not used yet
(defun treesit-stickyscroll--cut-context (beg end)
  (let ((beg-line-no (line-number-at-pos beg))
        (end-line-no (line-number-at-pos end))
        (first-indent 0)
        beg-column
        beg-column0-pos
        lines)
    (if (= beg-line-no end-line-no)
        (buffer-substring beg end)
      (save-excursion
        (save-restriction
          (goto-char beg)
          (setq beg-column0-pos (line-beginning-position))
          (setq beg-column (- beg beg-column0-pos))
          (push (buffer-substring beg (line-end-position)) lines)
          (forward-line)
          (while (< (line-number-at-pos) end-line-no)
            (if (> (current-indentation) beg-column)
                (push (buffer-substring (+ (line-beginning-position) beg-column) (line-end-position)) lines)
              (push (buffer-substring (line-beginning-position) (line-end-position)) lines))
            (forward-line))
          (if (> (current-indentation) beg-column)
              (push (buffer-substring (+ (line-beginning-position) beg-column) end) lines)
            (push (buffer-substring (line-beginning-position) end) lines))
          (mapconcat #'identity (nreverse lines) "\n"))))))

(defun treesit-stickyscroll-collect-contexts-base (node-types query-patterns)
  "Collect all of current node's parent nodes with node-type in NODE-TYPES.
Use QUERY-PATTERNS to capture potential nodes.
Each node is indented according to INDENT-OFFSET."
  (let* ((point (save-excursion (goto-char (window-start)) (line-end-position)))
         (current-line 0)
         (line-count 0)
         contexts)
    (while (and (< current-line 10) (not (< line-count current-line)))
      (setq contexts nil
            line-count 0)
      (let* ((parents (treesit-stickyscroll--parent-nodes node-types point))
             (root (nth 0 parents))
             groups
             node-pairs)
        (when root
          (setq groups (treesit-stickyscroll--capture root query-patterns (treesit-node-start root) (1+ (point))))
          (when groups
            (setq node-pairs (seq-filter (lambda (group) (member (cdr (nth 0 group)) parents)) groups))
            (when node-pairs
              (let (context
                    context.real
                    len
                    start-pos
                    end-pos
                    (visible-pos (save-excursion (goto-char point) (forward-line) (line-beginning-position)))
                    line-no
                    (old-line-no -1)
                    ctx-string)
                (save-excursion
                  (save-restriction
                    (widen)
                    (cl-dolist (np node-pairs)
                      (setq len (length np))
                      (setq context (cdr (nth 0 np))
                            start-pos (treesit-node-start context))
                      (cond
                       ((or (= len 1) (= len 2))
                        (save-excursion (goto-char start-pos) (setq start-pos (line-beginning-position)) (setq end-pos (line-end-position))))
                       ((= len 3)
                        (setq context.real (cdr (nth 1 np))
                              context context.real
                              end-pos (treesit-node-start context))
                        (save-excursion (goto-char end-pos) (setq start-pos (line-beginning-position))
                                        (goto-char end-pos) (setq end-pos (line-end-position)))))
                      (setq line-no (line-number-at-pos start-pos))
                      (unless (or (>= start-pos visible-pos) (eq old-line-no line-no))
                        (setq ctx-string (buffer-substring start-pos end-pos))
                        (setq line-count (+ line-count (length (split-string ctx-string "\n"))))
                        (cl-pushnew (cons line-no (treesit-stickyscroll-indent-context ctx-string)) contexts)
                        (setq old-line-no line-no))
                      ))))))))
      (setq point (save-excursion (goto-char point) (forward-line) (line-end-position)))
      (setq current-line (1+ current-line)))
    (nreverse contexts)))

(cl-defgeneric treesit-stickyscroll-collect-contexts ()
  "Collect all of current node's parent nodes."
  (user-error "%s is not supported by treesit-stickyscroll." major-mode))

;;; focus
(defun treesit-stickyscroll--focus-bounds (node-types)
  (let ((node (treesit-node-at (point)))
        result
        (begin (point-min))
        (end (point-max))
        stop)
    (while (and node
                (not stop))
      (if (member (treesit-node-type node) node-types)
          (progn
            (setq stop t)
            (setq begin (treesit-node-start node)
                  end (treesit-node-end node)))
        (setq node (treesit-node-parent node))))
    (if node
        (list node begin end)
      (list (treesit-buffer-root-node) begin end))))

(cl-defgeneric treesit-stickyscroll-focus-bounds ()
  "Return the bound that should be focused."
  (user-error "%s is not supported by treesit-stickyscroll-focus." major-mode))

;;; fold
(defun treesit-stickyscroll-fold--get-region-base (node-types)
  "Get current code node's region."
  (let ((node (treesit-node-at (point)))
        (node-type)
        (start-pos)
        (end-pos)
        (current-line (line-number-at-pos nil t)))
    (when node
      (setq node-type (treesit-node-type node))
      (if (and (member node-type node-types)
               (>= current-line (line-number-at-pos (treesit-node-start node) t)))
          (progn
            (setq start-pos (treesit-node-start node)
                  end-pos (treesit-node-end node)))
        (setq node (treesit-parent-until node
                                         (lambda (n)
                                           (and (member (treesit-node-type n) node-types)
                                                (>= current-line (line-number-at-pos (treesit-node-start n) t))))))
        (when node
          (setq start-pos (treesit-node-start node)
                end-pos (treesit-node-end node)))))
    (if (and start-pos end-pos)
        (progn
          (save-excursion
            (goto-char start-pos)
            (setq start-pos (line-end-position)))
          (list start-pos end-pos node))
      (message "No code region to fold.")
      nil)))

(cl-defgeneric treesit-stickyscroll-fold-get-region ()
  "Get current code node's region."
  (user-error "%s is not supported by treesit-stickyscroll-fold." major-mode))

(provide 'treesit-stickyscroll-common)
