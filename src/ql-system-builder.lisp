(in-package #:ql-system-builder)

(defparameter *charms-win* nil)
(defparameter *screen-width* 1)
(defparameter *screen-height* 1)

(defun clamp (val min max)
  (max (min val max) min))
(defun clamp-w (x)
  (clamp x 0 *screen-width*))
(defun clamp-h (y)
  (clamp y 0 *screen-height*))

(defmacro defcolors (&rest colors)
  `(progn
     ,@(loop :for n :from 0 :for blob :in colors :collect `(defparameter ,(first blob) ,n))
     ;; TODO: fix so that when you recompile defcolors, it automatically updates the thing
     (defun init-colors ()
       ;; each `blob` is a list (constant fg bg)
       ,@(loop :for blob :in colors :collect `(charms/ll:init-pair ,(first blob) ,(second blob) ,(third blob))))))

(defmacro with-color (color &body body)
  (alexandria:once-only (color)
    `(unwind-protect
          (progn
            (charms/ll:attron (charms/ll:color-pair ,color))
            ,@body)
       (charms/ll:attroff (charms/ll:color-pair ,color)))))

;; TODO: add special case for writing to the character in the lower-right-hand corner of the screen, or otherwise figure out what the heck is going on
(defun write-string-at (string x y &optional colors)
  (if colors
      (with-color colors
        (charms:write-string-at-point *charms-win* (subseq string 0 (- (clamp-w (+ (length string) x)) x)) (clamp-w x) (clamp-h y)))
      (charms:write-string-at-point *charms-win* (subseq string 0 (- (clamp-w (+ (length string) x)) x)) (clamp-w x) (clamp-h y)))
  (length string))

(defcolors
    ;; need some way of, when this is recompiled, patching it into running instance
    (+color-white-black+  charms/ll:COLOR_WHITE   charms/ll:COLOR_BLACK)
    (+color-black-white+  charms/ll:COLOR_BLACK charms/ll:COLOR_WHITE)

  (+color-black-black+  charms/ll:COLOR_BLACK charms/ll:COLOR_BLACK))

(defun init-charms (timeout color raw-input interpret-control-characters)
  (force-output *terminal-io*)
  (charms:initialize)
  ;; timeout set in milliseconds
  (charms/ll:timeout timeout)
  (setf *charms-win* (charms:standard-window))
  (charms:disable-echoing)
  (charms/ll:curs-set 0) ;; invisible cursor
  (when color
    (charms/ll:start-color)
    (init-colors))
  (if raw-input
      (charms:enable-raw-input :interpret-control-characters interpret-control-characters)
      (charms:disable-raw-input)))

(defmacro with-charms ((&key (timeout 100) (color nil) (raw-input t) (interpret-control-characters t)) &body body)
  `(unwind-protect
        (progn
          (init-charms ,timeout ,color ,raw-input ,interpret-control-characters)
          ,@body)
     (charms:finalize)))

(defun update-charms-dimensions ()
  (multiple-value-bind (width height) (charms:window-dimensions *charms-win*)
    (setf *screen-width* (1- width)
          ;; ok so this is monumentally stupid BUT you apparently can't write to the cell in the very bottom right-hand corner without causing an error in charms...
          *screen-height* height)))

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
  (update-charms-dimensions)

  ;; TODO: move draw code into separate function so we can initial paint without input
  (let* ((input (charms:get-char *charms-win* :ignore-error t)))
    (when input
      (charms:clear-window *charms-win*)
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
      (write-string-at (format nil "system name: ~a" system-name-buffer) 1 1 +color-white-black+)
      (charms:refresh-window *charms-win*))
    (main-loop system-name-buffer)))

(defun launch ()
  (with-charms (:timeout 100 :color t :raw-input t :interpret-control-characters t)
    (main-loop "")))
