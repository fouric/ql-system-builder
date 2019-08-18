(in-package #:ql-system-builder)

(defun main-loop (system-name-buffer)
  ;; what we want is several fields: system name,
  ;; option: symlink to local-projects/put in local-projects/change environment variable to point to project/none
  ;; specify system name, package name should default to system name, ASD package name default should be based on system name
  ;; specify dependencies, tie in to package declaration
  ;; default packages: package.lisp, main.lisp, package-name.lisp with option for defun package-name
  ;; specify .lisp or .lsp file extension
  ;; offer to create git repo + default commit, .gitignore
  ;; include options for blank curses/charms and sdl2 templates

  (f:update-swank)
  (f:update-charms-dimensions)

  ;; TODO: move draw code into separate function so we can initial paint without input
  (let* ((input (f:get-char)))
    (when input
      (f:clear-window)
      (when (eq 'quit
                (cond
                  ((char= input #\newline)
                   (format t "creating system with name ~s~%" system-name-buffer)
                   'quit)
                  ((char= input #\rubout)
                   (when (plusp (length system-name-buffer))
                     (setf system-name-buffer (subseq system-name-buffer 0 (1- (length system-name-buffer))))))
                  ((<= (char-code #\space) (char-code input) (char-code #\tilde))
                   (setf system-name-buffer (concatenate 'string system-name-buffer (coerce (list input) 'string))))))
        (return-from main-loop system-name-buffer))
      (f:write-string-at (format nil "system name: ~a" system-name-buffer) 1 1)
      (f:refresh-window))
    (main-loop system-name-buffer)))

(defun launch ()
  (f:with-charms (:timeout 100 :raw-input t :interpret-control-characters t)
    (main-loop "")))
