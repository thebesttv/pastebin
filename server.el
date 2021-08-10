(eserver-register-site "/pastebin"
                       "Paste & share.")

(defun httpd/pastebin (proc path query request)
  (with-httpd-buffer proc "text/plain"
    (if (or (string-equal path "/pastebin")
            (string-equal path "/pastebin/"))
        ;; list available posts
        (progn
          (insert "This is a very simple pastebin.\n"
                  "Available posts:\n")
          (mapc
           (lambda (buffer)
             (let ((name (buffer-name buffer)))
               (when (and (string-prefix-p "*pastebin: " name)
                          (string-suffix-p "*" name))
                 (setq name (string-remove-prefix
                             "*pastebin: "
                             (string-remove-suffix "*" name)))
                 (insert (concat "  " name "\n")))))
           (buffer-list)))
      ;; either return or store content by name
      (let ((name (string-remove-prefix "/pastebin/" path))
            (content (car (alist-get "Content" request
                                     "" nil 'string-equal)))
            buffer-name)
        (if (string-match "[^-_/a-zA-Z0-9]+" name)
            ;; illegal name
            (insert "ERROR: post name invalid\n"
                    "  Name can only ontain '-' '/' or alphanumeric characters.\n")
          (setq buffer-name (format "*pastebin: %s*" name))
          (get-buffer buffer-name)
          (if (string-empty-p content) ; content empty, then I can only return content
              (if (get-buffer buffer-name)
                  ;; return buffer content
                  (insert-buffer (get-buffer buffer-name))
                ;; no available buffer, issue warning
                (insert "WARNING: I don't have anything to store or return.\n"))
            ;; store content to buffer
            (with-current-buffer (get-buffer-create buffer-name)
              (erase-buffer)
              (insert content))
            (insert "content inserted to " name "\n")))))))
