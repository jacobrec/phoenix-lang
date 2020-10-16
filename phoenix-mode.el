;;; phoenix.el --- major mode for my phoenix programming language
;; Author: Jacob Reckhard
;;; Commentary:
;; I really wanted syntax highlighting

;;; Code
(setq phoenix-font-lock-keywords
      (let* ((phoenix-identifier-regex "[A-Za-z\\$\\@\\?_][0-9A-Za-z_\\$\\@\\?\\!]*")
             (x-keywords '("if" "then" "else" "fn" "defn" "def" "true" "false"))
             (x-builtins '("println" "print" "exit" "int?" "bool?" "string?" "list?" "array?"
                            "hash?" "function?" "char?" "float?" "eof?" "file?" "length" "@" "set@"
                            "substring" "subprefix" "push!" "pop!" "set@!" "insert!"
                            "remove!" "get" "has" "chr" "ord" "open!" "close!" "readchar!"
                            "writechar!" "car" "cdr" "shift"))
             (x-symbols '("+" "-" "*" "/" "&&" "||" ";" ";;" ":" "==" "<" "<=" ">" ">=" "=" "=>"
                          "[|" "|]" "[" "]" "{" "}"))
             (x-keyword-regex (regexp-opt x-keywords 'words))
             (x-builtin-regex (regexp-opt x-builtins 'symbols))
             (x-symbols-regex (regexp-opt x-symbols))
             (x-fn-name-regex (concat "defn \\(" phoenix-identifier-regex "\\)")))
        `((,x-fn-name-regex . (1 font-lock-variable-name-face))
          (,x-builtin-regex . (1 font-lock-builtin-face))
          (,x-keyword-regex . font-lock-keyword-face)
          (,x-symbols-regex . font-lock-variable-name-face))))


(define-derived-mode phoenix-mode nil "Phoenix mode"
  "Major mode for editing Phoenix code"
  (setq font-lock-defaults '((phoenix-font-lock-keywords)))
  (modify-syntax-entry ?' "\"")
  (modify-syntax-entry ?# "< b")
  (modify-syntax-entry ?\n "> b")
  (modify-syntax-entry ?< "(>")
  (modify-syntax-entry ?> ")<")
  (let ((syms '(?$ ?@ ?! ?_ ??)))
    (dolist (x syms)
      (modify-syntax-entry x "_"))))

(add-to-list 'auto-mode-alist '("\\.phx\\'" . phoenix-mode))

(provide 'phoenix)
;;; phoenix.el ends here
