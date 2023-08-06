;;; treesit-context-rust.el --- Show context information around current point -*- lexical-binding: t; -*-

(require 'treesit-context-common)

(defconst treesit-context--rust-node-types '("if_expression" "else_clause" "match_expression" "for_expression" "while_expression" "loop_expression" "closure_expression" "function_item" "impl_item" "trait_item" "struct_item" "enum_item" "mod_item")
  "Node types should be showed.")

(defconst treesit-context--rust-query
  '(
    (if_expression consequence: (_) @context.end) @context
    (else_clause (block (_)) @context.end) @context
    (match_expression body: (_) @context.end) @context
    (match_arm (block (_) @context.end)) @context
    (for_expression body: (_) @context.end) @context
    (while_expression body: (_) @context.end) @context
    (loop_expression body: (_) @context.end) @context
    (closure_expression body: (_) @context.end) @context
    (function_item body: (_) @context.end) @context
    (impl_item body: (_) @context.end) @context
    (trait_item body: (_) @context.end) @context
    (struct_item body: (_) @context.end) @context
    (enum_item body: (_) @context.end) @context
    (mod_item body: (_) @context.end) @context
    )
  "Query patterns to capture desired nodes.")

(cl-defmethod treesit-context-collect-contexts (&context (major-mode rust-ts-mode))
  "Collect all of current node's parent nodes."
  (treesit-context-collect-contexts-base treesit-context--rust-node-types treesit-context--rust-query rust-ts-mode-indent-offset))

(add-to-list 'treesit-context--supported-mode 'rust-ts-mode t)

(provide 'treesit-context-rust)
