(in-package #:ql-system-builder)

;; TODO: allow for source files that are in project root instead of src/

(defun draw (system-name-buffer)
  (f:clear-window)
  (f:write-string-at (format nil "system name: ~a" system-name-buffer) 1 1)
  (f:refresh-window))

(defun cat (&rest strings)
  (apply #'concatenate 'string strings))

(defun gitignore (project-directory)
  (f:write-lines (cat project-directory "/.gitignore") '("*.fasl" "*.elf")))

;; TODO: split into two layers: one that generates the defsystem form as a regular lisp list, and then another that prettily formats that list
(defun asd (containing-directory name dependencies &key description version author license)
  (flet ((keystr (key str)
           "wraps a given STRing with a KEYword argument"
           (cat "  :" key " \"" str "\"")))
    (let* ((modeline ";;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-")
           (asd-package (cat ":" name "-asd"))
           (asd-defpackage (list (cat "(defpackage #" asd-package)
                                 "  (:use :cl :asdf))"))
           (asd-inpackage (cat "(in-package " asd-package ")"))
           (defsystem (remove-if #'null (list (cat "(defsystem " name)
                                              (keystr "name" name)
                                              (when description
                                                (keystr "description" description))
                                              (when version
                                                (keystr "version" version))
                                              (when author
                                                (keystr "author" author))
                                              (when license
                                                (keystr "license" license))
                                              (let ((str "  :depends-on ("))
                                                (dolist (dep dependencies)
                                                  (unless (equal dep (first dependencies))
                                                    (setf str (cat str " ")))
                                                  (setf str (cat str ":" (string-downcase (string dep)))))
                                                (cat str ")"))
                                              "  :serial t"))))
      (f:write-lines (format nil "~a/~a/~a.asd" containing-directory name name)
                     (a:flatten (list modeline
                                      ""
                                      asd-defpackage
                                      ""
                                      asd-inpackage
                                      ""
                                      defsystem
                                      ")"))))))

(defun create-system (containing-directory name dependencies &key description version)
  (let ((our-directory (cat (namestring containing-directory) "/" (namestring name) "/")))
    (ensure-directories-exist our-directory)
    (gitignore our-directory)
    (asd containing-directory name dependencies :description description :version version)))

(defun delete-test-system (location)
  (shell-command (format nil "rm -rf ~a" location)))

(defun test-create-system (&key description version)
  (let* ((homedir (namestring (user-homedir-pathname)))
         (ramdisk (format nil "~a/ramdisk/" homedir)))
    (delete-test-system (cat ramdisk "/test-system/"))
    (create-system ramdisk "test-system" '(cl-fouric sdl2) :description description :version version)
    (let ((files (uiop:directory-files (format nil "~a/test-system/" ramdisk))))
      (format t "files in ~a: ~s~%"  (format nil "~a/test-system/" ramdisk) files)
      (dolist (file files)
        (format t "contents of ~a:~%~s~%" file (f:file-lines file))))))

(defun main-loop (system-name-buffer current-directory)
  ;; what we want is several fields: system name,
  ;; option: symlink to local-projects/put in local-projects/change environment variable to point to project/none
  ;; specify system name, package name should default to system name, ASD package name default should be based on system name
  ;; specify dependencies, tie in to package declaration
  ;; default packages: package.lisp, main.lisp, package-name.lisp with option for defun package-name
  ;; specify .lisp or .lsp file extension
  ;; offer to create git repo + default commit, .gitignore
  ;; include options for blank curses/charms and sdl2 templates

  ;; i'm not trying to micro-benchmark, only remind myself not to benchmark things like this until the program itself starts to have noticeable performance problems
  (f:update-swank) ; about 20 μs/call
  (f:update-charms-dimensions) ; about 50 μs/call

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

(defun cli-launch (&optional args)
  ;; first arg is the name of the binary, like in posix c with argc/argv
  (unless args
    (setf args sb-ext:*posix-argv*))
  #++(format t "count: ~a~%args: ~s~%" (length args) args)
  (let (flags)
    (loop :named popper :do
      (let ((val (pop args)))
        #++(format t "checking ~s~%" val)
        (cond
          ;; check to see if we've reached the end of the argument list
          ((null val)
           (return-from popper flags))
          ((or (string= val "-n") (string= val "--name"))
           (push (pop args) flags)
           (push :name flags))
          ((or (string= val "-d") (string= val "--description"))
           (push (pop args) flags)
           (push :description flags))
          ((or (string= val "-l") (string= val "--license"))
           (let ((license (pop args)))
             (push (if (string= license "arr")
                       "all rights reserved"
                       license) flags)
             (push :license flags)))
          ((or (string= val "-e") (string= val "--dependencies"))
           (let (deps)
             (loop :named deps :do
               ;; try to get the next arg provided on the command-line
               (let ((dep (pop args)))
                 ;; did we actually get one or is this the end of the line
                 (if dep
                     ;; if we got one, and it starts with a dash, it's probably another option
                     (if (not (char= #\- (schar dep 0)))
                         (push dep deps)
                         (return-from deps))
                     (return-from deps))))
             (push (loop :for dep :in deps :collect (intern (string-upcase dep) :keyword)) flags)
             (push :dependencies flags)))
          ((or (string= val "-a") (string= val "--author"))
           (push (pop args) flags)
           (push :author flags))
          (t
           (format t "warning: unbound argument: ~a~%" val)))))
    #++(print flags)
    flags))

(defun interactive-launch (&optional current-directory)
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
