(in-package #:ql-system-builder)

(defun draw (system-name-buffer)
  (f:clear-window)
  (f:write-string-at (format nil "system name: ~a" system-name-buffer) 1 1)
  (f:refresh-window))

(defun cat (&rest strings)
  (apply #'concatenate 'string strings))

(defun gitignore (project-directory)
  (f:write-lines (cat project-directory "/.gitignore") '("*.fasl" "*.elf")))

(defun create-system (containing-directory name)
  (let ((our-directory (cat (namestring containing-directory) "/" (namestring name) "/")))
    (ensure-directories-exist our-directory)
    (gitignore our-directory)))

(defun delete-test-system (location)
  (shell-command (format nil "rm -rf ~a" location)))

(defun test-create-system ()
  (let* ((homedir (namestring (user-homedir-pathname)))
         (ramdisk (format nil "~a/ramdisk/" homedir)))
    (delete-test-system (cat ramdisk "/test-system/"))
    (create-system ramdisk "test-system")
    (let ((files (uiop:directory-files (format nil "~a/test-system/" ramdisk))))
      (format t "files in ~a: ~s~%"  (format nil "~a/test-system/" ramdisk) files)
      (dolist (file files)
        (format t "contents of ~a: ~s~%" file (f:file-lines file))))))

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
                   (setf system-name-buffer (cat system-name-buffer (coerce (list input) 'string))))))
        (return-from main-loop system-name-buffer))
      (draw system-name-buffer))
    (main-loop system-name-buffer current-directory)))

(defun launch (&optional current-directory)
  (f:with-charms (:timeout 100 :raw-input t :interpret-control-characters t)
    (draw "")
    (main-loop "" current-directory)))

(defun dev-launch ()
  (f:emacs-eval '(slime-enable-concurrent-hints))
  (let ((test-system-location (format nil "~a/ramdisk/" (namestring (user-homedir-pathname)))))
    (delete-test-system test-system-location)
    (f:with-charms (:timeout 100 :raw-input t :interpret-control-characters t)
      (draw "test-system")
      (main-loop "test-system" test-system-location))))
