;; export backends for mandoku files

(org-export-define-derived-backend mandoku-latex latex
  :filters-alist ((:filter-parse-tree . mandoku-separate-elements))
  :translate-alist ((headline . mandoku-latex-headline-translator)
                    (template . mandoku-latex-template)))


(org-export-define-derived-backend mandoku-odt odt
  :translate-alist ((headline . mandoku-odt-headline-translator)
                    (template . mandoku-odt-template)))
