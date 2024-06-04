(import
 (owl toplevel)
 (owl args)
 (owl eval)
 (owl regex)
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
      (for-each (λ (s) (write-bytes of (string->list s))) (force-ll (lines bf)))
      (for-each (λ (s) (write-bytes of (string->list s))) (force-ll (lines r)))
      (for-each (λ (s) (write-bytes of (string->list s))) (force-ll (lines af)))
      (map sys/close (filter self (list of w r bf af))))))

(λ (args)
  (let* ((path-start (if (null? (cdr args)) "." (cadr args)))
         (cfg (if (sys/file? (string-append path-start "/cfg.scm"))
                  (exported-eval (read (open-input-file (string-append path-start "/cfg.scm"))) *toplevel*)
                  cfg-default))
         (path (string-append path-start "/" (aq 'markdown-folder cfg)))
         (md-files (filter
                    (λ (s) ((string->regex "m/\\.md$/") s))
                    (map (λ (s) (string-append path "/" s)) (sys/dir->list path)))))
    (print "md-files: " md-files)
    (when (not (sys/file? (aq 'output-folder cfg)))
      (sys/mkdir (aq 'output-folder cfg) #o777))

    (for-each
     (λ (path) (md->html path cfg))
     md-files)
    (print "ok")))
