#lang racket

(require web-server/servlet
         web-server/servlet-env)

;; Remove stuff from approved list!

(require (planet neil/csv:1:6/csv))

(define config-file-path (build-path (find-system-path 'home-dir) "winkcfg.rkt"))

(define current-config
  (with-handlers ([exn:fail:filesystem? (lambda (_)
                                          '((approve "chrome.exe" "DrRacket.exe" "System Idle Process" "System")
                                            (disapprove "iPodService.exe")))])
    (with-input-from-file config-file-path read)))

(define (update-and-write-config new-config)
  (set! current-config new-config)
  (with-output-to-file config-file-path (lambda () (write new-config)) #:exists 'replace))

(define (approved? prog-name)
  (match current-config
    [`((approve . ,approves) (disapprove . ,disapproves))
     (member prog-name approves)]))

(define (disapproved? prog-name)
  (match current-config
    [`((approve . ,approves) (disapprove . ,disapproves))
     (member prog-name disapproves)]))

(define (approve-prog prog-name)
  (update-and-write-config
   (match current-config
     [`((approve . ,approves) (disapprove . ,disapproves))
      `((approve . ,(cons prog-name approves))
        (disapprove . ,(remove prog-name disapproves)))])))

(define (disapprove-prog prog-name)
  (update-and-write-config
   (match current-config
     [`((approve . ,approves) (disapprove . ,disapproves))
      `((approve . ,(remove prog-name approves))
        (disapprove . ,(cons prog-name disapproves)))])))

;; Get the process data from Windows

(define (blank-csv-output-line? line)
  (equal? line '("")))
(define (remove-blank-csv-line! locus)
  (unless (blank-csv-output-line? (next-line))
    (error 'wink "~a expected to be but not blank" locus)))

;; Headers are re-ordered to reflect the order in which wmic produces them
;; The Node header is automatically prepended to each line
(define desired-headers '("ExecutablePath" "Name" "ParentProcessId" "ProcessId"))
(define actual-headers (cons "Node" desired-headers))
(define headers-string
  (let l ([h desired-headers]) (if (empty? (rest h)) (first h) (string-append (first h) "," (l (rest h))))))

(define system-output
  (with-output-to-string 
   (lambda ()
     (system (string-append "wmic process get " headers-string " /format:csv")))))
(define next-line (make-csv-reader system-output))

(remove-blank-csv-line! "first line")
(remove-blank-csv-line! "second line")
(define headers (next-line))
(remove-blank-csv-line! "line after headers")

(define processes
  (let loop ([lines empty])
    (let ([l(next-line)])
      (if (empty? l)
          lines
          (if (blank-csv-output-line? l)
              (loop lines)
              (loop (cons l lines)))))))

;; Create a browser for it

(define (start request)
  (send/suspend/dispatch
   (lambda (embed-url)
     `(html
       (head (title "Window Monitor")
             (style ([type "text/css"])
                    ".disapproved{font-size: x-large;
                         background-color: #ff6347;
			 text-align: center; padding: .5em}
                  .undecided{font-size: x-large;
                         background-color: #ffd700;
			 text-align: center; padding: .5em}
                  .approved{font-size: x-large;
                         background-color: #2e8b57;
			 text-align: center; padding: .5em}
                  .quit-bar{font-size: x-large;
                         background-color: #fffffc;
			 text-align: center; padding: .5em}
                  body{background-color: #eee8aa;}
                  .approve-link-area{color:green; display:inline-block; width:5em; text-align:center}
                  .disapprove-link-area{color:red; display:inline-block; width:5em; text-align:center}
		  .evenparity{background-color: #ffffe0;}
                  .oddparity{background-color: #ffe4b5;}
                  .pid {display:inline-block;width:3em;text-align:right}
                  .ppid {display:inline-block;width:5em;text-align:center}
                  .prog {display:inline-block;font-family:monospace;padding:2px 0px 2px 0px;text-align:left}
                  .links {display:inline-block;padding:0px 0px 0px 5px;text-align:center}"))
       ,(let ([approved (filter (match-lambda
                                  [(list _ ep n ppid pid)
                                   (approved? n)])
                                processes)]
              [disapproved (filter (match-lambda
                                     [(list _ ep n ppid pid)
                                      (disapproved? n)])
                                   processes)]
              [undecided (filter (match-lambda
                                   [(list _ ep n ppid pid)
                                    (and (not (approved? n))
                                         (not (disapproved? n)))])
                                 processes)])
          (define (display-one approval-status)
            (match-lambda
              [(list _ ep n ppid pid)
               `(span
                 (a ([name ,pid])
                    (span ([class "pid"])
                          ,pid))
                 (span ([class "ppid"])
                       "[" (a ([href ,(string-append "#" ppid)]) "parent") "]")
                 (span ([class "actions"])
                       ,@(case approval-status
                           [(approved)
                            `((span ([class "disapprove-link-area"])
                                    (a ([href ,(embed-url (lambda (r)
                                                            (disapprove-prog n)
                                                            (start r)))])
                                       "Disapprove")))]
                           [(disapproved)
                            `((span ([class "approve-link-area"]) 
                                    (a ([href ,(embed-url (lambda (r)
                                                            (approve-prog n)
                                                            (start r)))])
                                       "Approve")))]
                           [(undecided)
                            `((span ([class "approve-link-area"]) 
                                    (a ([href ,(embed-url (lambda (r)
                                                            (approve-prog n)
                                                            (start r)))])
                                       "Approve"))
                              (span ([class "disapprove-link-area"]) 
                                    (a ([href ,(embed-url (lambda (r)
                                                            (disapprove-prog n)
                                                            (start r)))])
                                       "Disapprove")))]))
                 (span ([class "prog"] [title ,ep]) ,n)
                 (span ([class "links"])
                       "(" (a ([href ,(string-append "http://www.processlibrary.com/search/?q=" n)])
                              "Process Library") 
                       "), "
                       "(" (a ([href ,(string-append "http://www.google.com/search?q=" n)])
                              "Google") ")"))]))
          (define (even-odd spans)
            (define (helper spans even?)
              (cond
                [(empty? spans) empty]
                [(cons? spans) (cons `(div ([class ,(if even? "evenparity" "oddparity")])
                                           ,(first spans))
                                     (helper (rest spans) (not even?)))]))
            (helper spans true))
          `(body
            (div ([class "quit-bar"]) (a ([href "/quit"]) "Quit"))
            (div ([class "disapproved"]) "Disapproved")
            ,@(even-odd (map (display-one 'disapproved) disapproved))
            (div ([class "undecided"]) "Undecided")
            ,@(even-odd (map (display-one 'undecided) undecided))
            (div ([class "approved"]) "Approved")
            ,@(even-odd (map (display-one 'approved) approved))))))))

(serve/servlet start #:banner? false #:quit? true)