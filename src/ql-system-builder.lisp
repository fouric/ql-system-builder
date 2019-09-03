(in-package #:ql-system-builder)

(defun draw (system-name-buffer)
  (f:clear-window)
  (f:write-string-at (format nil "system name: ~a" system-name-buffer) 1 1)
  (f:refresh-window))

(defun create-system (containing-directory name)
  (ensure-directories-exist (concatenate 'string (namestring containing-directory) "/" (namestring name) "/")))

(defun test-create-system ()
  (shell-command (format nil "rm -rf ~a/ramdisk/test-system" (namestring (user-homedir-pathname))))
  (create-system (format nil "~a/ramdisk" (namestring (user-homedir-pathname))) "test-system")
  (format t "files in ~a~a: ~s~%" (namestring (user-homedir-pathname)) "/ramdisk/test-system/" (uiop:directory-files (format nil "~a/ramdisk/test-system/" (namestring (user-homedir-pathname))))))

(defun main-loop (system-name-buffer current-directory)
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

  (let* ((input (f:get-char)))
    (when input
      (when (eq 'quit
                (cond
                  ((char= input #\newline)
                   (create-system current-directory system-name-buffer)
                   'quit)
                  ((char= input #\rubout)
                   (when (plusp (length system-name-buffer))
                     (setf system-name-buffer (subseq system-name-buffer 0 (1- (length system-name-buffer))))))
                  ((<= (char-code #\space) (char-code input) (char-code #\tilde))
                   (setf system-name-buffer (concatenate 'string system-name-buffer (coerce (list input) 'string))))))
        (return-from main-loop system-name-buffer))
      (draw system-name-buffer))
    (main-loop system-name-buffer current-directory)))

(defun launch (&optional current-directory)
  (f:with-charms (:timeout 100 :raw-input t :interpret-control-characters t)
    (draw "")
    (main-loop "" current-directory)))

(defun dev-launch ()
  (f:emacs-eval '(slime-enable-concurrent-hints))
  (shell-command (format nil "rm -rf ~a/ramdisk/test-system" (namestring (user-homedir-pathname))))
  (f:with-charms (:timeout 100 :raw-input t :interpret-control-characters t)
    (draw "test-system")
    (main-loop "test-system" (format nil "~a/ramdisk/" (namestring (user-homedir-pathname))))))
