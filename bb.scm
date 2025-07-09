(import
 (owl toplevel)
 (owl eval)
 (prefix (robusta encoding html) H/)
 (prefix (owl sys) sys/))

(define cfg-default
  '((pandoc . "pandoc")
    (pandoc-args . "-f gfm -t html")
    (template-before . "template-before.html")
    (template-after . "template-after.html")
    (markdown-folder . "md/")
    (output-folder . "b/")
    (site-name . "")
    (rss-description . "")
    (rss . "posts.rss")))

(define (aq v a) (cdr* (assq v a)))

(define (basename s)
  (last ((string->regex "c/\\//") s) "invalid-path"))

(define -spl (string->regex "c/-/"))

(define undate (string->regex "s/^....-..-..-//"))

(define (Tsort a b)
  (string-ci>? a b))

(define (f->f of i)
  (for-each
   (λ (s) (print-to of s))
   (force-ll (lines i))))

(define (md->html path cfg)
  (let* ((nam (undate (basename path)))
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

(define (md-name->html-basename s)
  ((string->regex "s/\\.md$/.html/") (basename s)))

(define (filename->title f)
  (let* ((s (undate f))
         (s ((string->regex "s/[_\\-]/ /g") s))
         (s ((string->regex "s/(\\.html$)|(\\.md$)//g") s)))
    s))

(define (write-index cfg md-files)
  (print "write-index")
  (let* ((o (aq 'output-folder cfg))
         (l (map md-name->html-basename md-files))
         (idx (open-output-file (string-append o "/index.html")))
         (bf (open-input-file (aq 'template-before cfg)))
         (af (open-input-file (aq 'template-after cfg))))
    (f->f idx bf)
    (print-to idx "<ul>")
    (for-each
     (λ (s)
       (let* ((date (take (-spl s) 3))
              (title (filename->title s)))
         (print-to idx (string-append "<li><a href=\"" (undate s) "\">"
                                      title
                                      "</a> <sub> (" (lref date 2) "/" (lref date 1) "/" (car date) ")"
                                      "</sub></li>"))))
     l)
    (print-to idx "</ul>")
    (f->f idx af)))

(define month-names (tuple "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(define (date-rfc822 yr mon day)
  (lets ((_ d (week-info day mon yr)))
    (format
     #f "~a, ~2,'0d ~a ~d 00:00:01 +0200"
     (substring (ref day-names-en d) 0 3)
     day (ref month-names mon) yr)))

(define (write-rss cfg md-files)
  (print "write-rss")
  (let* ((loc (aq 'rss cfg))
         (f (open-output-file loc))
         (desc (aq 'rss-description cfg))
         (out (aq 'output-folder cfg))
         (site-name (aq 'site-name cfg)))
    (write-bytes
     f
     (string->bytes
      (str
       "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n"
       (H/encode*
        `((rss (version . "2.0"))
          (channel
           (title ,site-name)
           (link ,(str "https://" site-name))
           (description ,desc)
           ,@(map
              (λ (f)
                (let ((s (md-name->html-basename f)))
                  `(item
                    (pubDate ,(apply date-rfc822 (map string->number (take (-spl s) 3))))
                    (title ,(filename->title (basename f)))
                    (link ,(str "https://" site-name "/" out (undate s))))))
              md-files)))
        #n))))))

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
    (write-index cfg md-files)
    (write-rss cfg md-files)
    (print "ok")
    0))
