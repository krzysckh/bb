(import
 (owl toplevel)
 (owl args)
 (owl eval)
 (owl regex)
 (owl sort)
 (prefix (owl sys) sys/))

(define cfg-default
  '((pandoc . "pandoc")
    (pandoc-args . "-f gfm -t html")
    (template-before . "template-before.html")
    (template-after . "template-after.html")
    (markdown-folder . "md/")
    (output-folder . "b/")))

(define (aq v a) (cdr* (assq v a)))

(define (basename s)
  (last ((string->regex "c/\\//") s) "invalid-path"))

(define (Tsort a b)
  (let ((ta (aq 'mtim (sys/stat a #t)))
        (tb (aq 'mtim (sys/stat b #t))))
    (< ta tb)))

(define (f->f of i)
  (for-each
   (λ (s) (print-to of s))
   (force-ll (lines i))))

(define (md->html path cfg)
  (let* ((nam (basename path))
         (ofp ((string->regex "s/\\.md$/.html/") (string-append (aq 'output-folder cfg) "/" nam)))
         (of (open-output-file ofp))
         (bf (open-input-file (aq 'template-before cfg)))
         (af (open-input-file (aq 'template-after cfg))))
    (lets ((r w (popen (fold (λ (a s) (string-append a " " s)) ""
                             (list
                              (aq 'pandoc cfg)
                              (aq 'pandoc-args cfg)
                              "-o -"
                              path)))))
      (f->f of bf)
      (f->f of r)
      (f->f of af)
      (map sys/close (filter self (list of w r bf af))))))

(define (write-index cfg)
  (print "write-index")
  (let* ((o (aq 'output-folder cfg))
         (_ (sys/chdir o))
         (l (sort Tsort (sys/dir->list "."))) ;; before creating index.html
         (_ (sys/chdir ".."))
         (idx (open-output-file (string-append o "/index.html")))
         (bf (open-input-file (aq 'template-before cfg)))
         (af (open-input-file (aq 'template-after cfg))))
    (f->f idx bf)
    (print-to idx "<ul>")
    (for-each
     (λ (s)
       (let* ((title ((string->regex "s/[_\\-]/ /g") s))
              (title ((string->regex "s/\\.html$//g") title)))
         (print-to idx (string-append "<li><a href=\"" s "\">" title "</a></li>"))))
     l)
    (print-to idx "</ul>")
    (f->f idx af)))

(λ (args)
  (let* ((path-start (if (null? (cdr args)) "." (cadr args)))
         (cfg (if (sys/file? (string-append path-start "/cfg.scm"))
                  (exported-eval (read (open-input-file (string-append path-start "/cfg.scm"))) *toplevel*)
                  cfg-default))
         (path (string-append path-start "/" (aq 'markdown-folder cfg)))
         (md-files (sort
                    Tsort
                    (filter
                     (λ (s) ((string->regex "m/\\.md$/") s))
                     (map (λ (s) (string-append path "/" s)) (sys/dir->list path)))))
         (op (aq 'output-folder cfg)))
    (print "md-files: " md-files)

    (when (not (sys/file? op))
      (sys/mkdir (aq 'output-folder cfg) #o777))

    (for-each (λ (s)
                (print "delete " s)
                (sys/unlink s))
              (map (λ (s) (string-append path-start "/" op "/" s)) (sys/dir->list (string-append path-start "/" op))))

    (for-each
     (λ (path) (md->html path cfg))
     md-files)
    (write-index cfg)
    (print "ok")
    0))
