(eserver-register-site "/pastebin"
  "Paste & share.")

(defvar pastebin-root (expand-file-name "pastebin" (temporary-file-directory))
  "Directory to store pastebin entries.")

(defvar pastebin-entries nil
  "An alist of astebin entries.
Each entry is a cons struct of entry name and file name.")

(mkdir pastebin-root t)

(defun file-mime-type (filename)
  (string-trim
   (with-output-to-string
     (with-current-buffer standard-output
       (shell-command (combine-and-quote-strings
                       (list "file" "-bi" filename))
                      t nil)))))

(defun pastebin-welcome ()
  "Return a string of welcome information containing available posts."
  "This is a very simple pastebin.\n"
  ;; (apply 'concat
  ;;        "This is a very simple pastebin.\n"
  ;;        "Available posts:\n"
  ;;        (mapcar (lambda (pair)
  ;;                  (format "  %s (%s)\n"
  ;;                          (car pair)     ; entry name
  ;;                          (caddr pair))) ; file mime type
  ;;                (setq pastebin-entries
  ;;                      (sort pastebin-entries (lambda (a b)
  ;;                                               (string-lessp
  ;;                                                (car a) (car b)))))))
  )

(defun httpd/pastebin (proc path query request)
  (if (or (string-equal path "/pastebin")
          (string-equal path "/pastebin/"))
      ;; list available posts
      (with-httpd-buffer proc "text/plain; charset=utf-8"
        (insert (pastebin-welcome)))
    ;; either return or store content by name
    (let ((entry-name (string-remove-prefix "/pastebin/" path))
          (content (car (eserver-request-get "Content" request)))
          file-name mime-type)
      (if (string-match "[^-._/a-zA-Z0-9]+" entry-name)
          ;; `entry-name' illegal
          (with-httpd-buffer proc "text/plain; charset=utf-8"
            (insert (format "ERROR: post name \"%s\" is invalid.\n" entry-name)
                    "  Name can only ontain \"-._/\" or alphanumeric characters.\n"))
        ;; `entry-name' ok
        (if (string-empty-p content)
            ;; content empty, then I can only return content
            (if (null (assoc entry-name pastebin-entries))
                ;; no such buffer, issue warning
                (with-httpd-buffer proc "text/plain; charset=utf-8"
                  (insert "WARNING: I don't have anything to store or return.\n"))
              ;; buffer exists, return its content
              (let ((entry (assoc entry-name pastebin-entries)))
                (setq file-name (cadr entry))
                (setq mime-type (caddr entry))
                (with-temp-buffer
                  (set-buffer-multibyte nil)
                  (insert-file-contents-literally file-name)
                  (httpd-send-header proc mime-type 200))))
          ;; content not empty, then store as entry
          (let ((temporary-file-directory pastebin-root))
            (setq file-name (make-temp-file "entry-")))
          (with-temp-file file-name
            ;; don't care about encoding, store as binary file
            (set-buffer-file-coding-system 'binary)
            (insert content))
          (setq mime-type (file-mime-type file-name))
          (setf (alist-get entry-name pastebin-entries
                           nil nil 'string-equal)
                (list file-name mime-type))
          (with-httpd-buffer proc "text/plain; charset=utf-8"
            (insert (format "Content inserted to entry: %s (%s)\n"
                            entry-name mime-type))))))))
