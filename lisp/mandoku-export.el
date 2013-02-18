;; export backends for mandoku files

(org-export-define-derived-backend md-latex latex
  :translate-alist ((headline . md-latex-headline-translator)
                    (template . md-latex-template)))


(org-export-define-derived-backend md-odt odt
  :translate-alist ((headline . md-odt-headline-translator)
                    (template . md-odt-template)))
