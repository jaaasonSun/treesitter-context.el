;;; treesit-stickyscroll-c.el --- Show context information around current point -*- lexical-binding: t; -*-

(require 'treesit-stickyscroll-common)

(defconst treesit-stickyscroll--c++-node-types '("preproc_if" "preproc_ifdef" "preproc_else" "function_definition" "for_statement" "if_statement" "else_clause" "while_statement" "do_statement" "struct_specifier" "enum_specifier" "for_range_loop" "class_specifier" "namespace_definition" "linkage_specification" "switch_statement" "case_statement")
  "Node types that may be showed.")

(defconst treesit-stickyscroll--c++-query
  (treesit-query-compile 'cpp '(
                                (preproc_if (_) (_) @context.end) @context
                                (preproc_ifdef name: (identifier) :anchor (_) @context.end) @context
                                (preproc_else (_) @context.end) @context
                                (function_definition declarator: (function_declarator) @context.real body: (_) @context.end) @context
                                (function_definition declarator: (pointer_declarator declarator: (_) @context.real) body: (_) @context.end) @context
                                (function_definition declarator: (reference_declarator (_) @context.real) body: (_) @context.end) @context
                                (for_statement (compound_statement) @context.end) @context
                                (if_statement consequence: (_) @context.end) @context
                                (else_clause (_) @context.end) @context
                                (while_statement body: (_) @context.end) @context
                                (do_statement body: (_) @context.end) @context
                                (switch_statement body: (_) @context.end) @context
                                (case_statement value: (_) :anchor) @context
                                (case_statement value: (_) :anchor (_) @context.end) @context
                                (case_statement "default" :anchor (_) @context.end) @context
                                (struct_specifier body: (_) @context.end) @context
                                (enum_specifier body: (_) @context.end) @context    
                                (for_range_loop body: (_) @context.end) @context
                                (namespace_definition body: (_) @context.end) @context
                                (class_specifier body: (_) @context.end) @context
                                (linkage_specification body: (declaration_list (_) @context.end)) @context
                                ))
  "Query patterns to capture desired nodes.")

(cl-defmethod treesit-stickyscroll-collect-contexts (&context (major-mode c++-ts-mode))
  "Collect all of current node's parent nodes."
  (treesit-stickyscroll-collect-contexts-base treesit-stickyscroll--c++-node-types treesit-stickyscroll--c++-query))

;;; supported mode
(add-to-list 'treesit-stickyscroll--supported-mode 'c++-ts-mode t)

(provide 'treesit-stickyscroll-cpp)
