;;;; swm-clim-message.lisp

(in-package #:swm-clim-message)

(defvar *message-window-frame* nil ; not the same as a stumpwm frame
  "Only allow one message window to exist at a time.")

(defvar *clim-message-window-keymap* (stumpwm:make-sparse-keymap)
  "A keymap for keybindings to get sent from stumpwm to the clim message window")

(stumpwm:define-key *clim-message-window-keymap* (stumpwm:kbd "C-n")
  "swm-clim-message-command (com-select-next)")
(stumpwm:define-key *clim-message-window-keymap* (stumpwm:kbd "C-p")
  "swm-clim-message-command (com-select-prev)")
(stumpwm:define-key *clim-message-window-keymap* (stumpwm:kbd "RET")
  "swm-clim-message-command (com-select-choose)")
(stumpwm:define-key *clim-message-window-keymap* (stumpwm:kbd "C-RET")
  "swm-clim-message-command (com-select-choose-and-quit)")
(stumpwm:define-key *clim-message-window-keymap* (stumpwm:kbd "ESC")
  "swm-clim-message-command (com-quit-overwrite)")
(stumpwm:define-key *clim-message-window-keymap* (stumpwm:kbd "C-g")
  "swm-clim-message-command (com-quit-overwrite)")

;; Evaluate something within the without messaging the result. Use the
;; :swm-clim-message package by default
(stumpwm:defcommand swm-clim-message-command (cmd) ((:rest "CMD: "))
  (handler-case
      (if cmd
          (let ((*package* (find-package :swm-clim-message)))
            (execute-frame-command *message-window-frame* (read-from-string cmd)))
          (throw 'error :abort))
    (error (c)
      (stumpwm:err "^B^1*~A" c))))

;; Calculate the positioning of the window
(defun calculate-gravity (screen head gravity width height)
  (xlib:with-state ((stumpwm:screen-root screen))
    (let* ((w width)
           (h height)
           (head-x (stumpwm::head-x head))
           (head-y (stumpwm::head-y head))
           (head-maxx (+ head-x (stumpwm::head-width head)))
           (head-maxy (+ head-y (stumpwm::head-height head))))
      (stumpwm::gravity-coords gravity w h head-x head-y head-maxx head-maxy))))

;; Specifically used for the message window frame. Could be more general, might
;; be a generic to do what we want already, but this is good enough for now. 
(defun move-resize-message-window (frame &optional
                                           (pane (find-pane-named frame 'display)))
  (when pane
    (with-sheet-medium (medium pane)
      (let* ((text-style (medium-text-style medium))
             (line-height (+ (text-style-height text-style medium)
                             (stream-vertical-spacing pane)))
             (strings (message-window-strings frame))
             (w 0)
             (h 0))
        (loop for (string . ig) in strings
              with highlight = (message-window-highlight frame)
              for x from 0
              for new-width = (stream-string-width
                               pane
                               (concatenate 'string " " string)
                               :text-style (if (= x highlight)
                                               (make-text-style (text-style-family
                                                                 text-style)
                                                                :bold
                                                                (text-style-size
                                                                 text-style))
                                               text-style))
              do (incf h line-height)
                 (when (> new-width w)
                   (setf w new-width)))
        (multiple-value-bind (x y)
            (calculate-gravity (message-window-stumpwm-screen frame)
                               (message-window-stumpwm-head frame)
                               stumpwm:*message-window-gravity*
                               w
                               h)
          (move-and-resize-sheet (frame-top-level-sheet frame) x y w h))))))

(defmacro bold ((stream) &body body)
  `(with-text-face (,stream :bold)
     ,@body))

;; Define our frame
(define-application-frame message-window ()
  ((persistent :initarg :persistent
               :initform nil
               :accessor message-window-persistent)
   (strings :initarg :strings
            :accessor message-window-strings)
   (highlight :initarg :highlight
              :initform -1
              :accessor message-window-highlight)
   (proc :accessor message-window-process)
   (stumpwm-screen :initarg :screen
                   :initform (stumpwm:current-screen)
                   :accessor message-window-stumpwm-screen)
   (stumpwm-head :initarg :head
                 :initform (stumpwm:current-head)
                 :accessor message-window-stumpwm-head)
   (return-value :initform (cons nil nil)
                 :initarg :default-return-value)
   (mutex :initform (sb-thread:make-mutex :name "message-window-mutex")
          :accessor message-window-frame-mutex))
  (:panes (display :application
                   :display-function 'display-message-window
                   :scroll-bars nil
                   :background +black+
                   :foreground +white+))
  (:layouts (default display)))

;; We dont want this to be treated as a regular window. 
(defmethod clim-extensions:find-frame-type ((frame message-window))
  :override-redirect)

;; We need to redisplay when we change a slot that impacts behavior. So for
;; example when we change the strings we need to redisplay so that the new
;; strings are displayed. when we change the persistance we need to redisplay so
;; that the strings are displayed with the correct presentation to command
;; translator. 
(defmethod (setf message-window-strings) :after (new (frame message-window))
  (declare (ignore new))
  (redisplay-frame-panes frame :force-p t))

(defmethod (setf message-window-persistent) :after (new (frame message-window))
  (declare (ignore new))
  (redisplay-frame-panes frame :force-p t))

(defmethod (setf message-window-stumpwm-screen) :after (new (frame message-window))
  (move-resize-message-window frame))

(defmethod (setf message-window-stumpwm-head) :after (new (frame message-window))
  (move-resize-message-window frame))

(defgeneric message-window-return (frame &key wait timeout default))

(defmethod message-window-return ((frame message-window) &key wait timeout default)
  (multiple-value-call
      (lambda (&optional (return-value) (state :failure) &rest rest)
        (declare (ignore rest))
        (if (eql state :failure)
            (values default :timeout)
            (values return-value state)))
    (sb-thread:with-mutex ((message-window-frame-mutex frame) :wait-p wait
                                                              :timeout timeout)
      (let* ((cell (slot-value frame 'return-value))
             (gotten (cdr cell)))
        (setf (cdr cell) :stale)
        (values (car cell)
                gotten)))))

(defmethod (setf message-window-return) (new (frame message-window))
  (sb-thread:with-mutex ((message-window-frame-mutex frame) :wait-p t)
    (let ((cell (slot-value frame 'return-value)))
      (setf (cdr cell) :fresh
            (car cell) new))))

;;;;;;;;;;;;;;;;
;;; COMMANDS ;;;
;;;;;;;;;;;;;;;;

;;; Commands and their presentation types & translators

;;;;;;;;;;;;;;;;;;;;
;; Quit The Frame ;;
;;;;;;;;;;;;;;;;;;;;

(define-message-window-command (com-quit :name t)
    ()
  (frame-exit *application-frame*))

(define-message-window-command (com-quit-overwrite :name t)
    ()
  (setf (message-window-return *application-frame*) nil)
  (com-quit))

(define-presentation-type close-message-window ())
(define-presentation-to-command-translator close-mw 
    (close-message-window com-quit message-window :gesture :select)
    (txt)
  nil)

;;;;;;;;;;;;;;;;
;; Selections ;;
;;;;;;;;;;;;;;;;

(define-message-window-command (com-select-next :name t)
    ()
  (setf (message-window-highlight *application-frame*)
        (if (= (message-window-highlight *application-frame*)
               (1- (length (message-window-strings *application-frame*))))
            0
            (1+ (message-window-highlight *application-frame*)))))

(define-message-window-command (com-select-prev :name t)
    ()
  (setf (message-window-highlight *application-frame*)
        (if (= (message-window-highlight *application-frame*) 0)
            (1- (length (message-window-strings *application-frame*)))
            (1- (message-window-highlight *application-frame*)))))

(define-message-window-command (com-select-choose :name t)
    ()
  (let ((selection (elt (message-window-strings *application-frame*)
                        (message-window-highlight *application-frame*))))
    (if (message-window-persistent *application-frame*)
        (com-call-thunk (cdr selection))
        (com-call-thunk-and-exit (cdr selection)))))

(define-message-window-command (com-select-choose-and-quit :name t)
    ()
  (let ((selection (elt (message-window-strings *application-frame*)
                        (message-window-highlight *application-frame*))))
    (com-call-thunk-and-exit (cdr selection))))

;;;;;;;;;;;;;;;;;
;; Call Thunks ;;
;;;;;;;;;;;;;;;;;

(define-message-window-command (com-call-thunk :name t)
    ((thunk function))
  (setf (message-window-return *application-frame*) (funcall thunk)))

(define-message-window-command (com-call-thunk-and-exit :name t)
    ((thunk function))
  (com-call-thunk thunk)
  (com-quit))

(define-presentation-type evaluate-entry ())
(define-presentation-to-command-translator eventry
    (evaluate-entry com-call-thunk message-window
     :gesture :select 
     :priority 1
     :documentation "Evaluate Entry")
    (thnk)
  (list thnk))

(define-presentation-to-command-translator eventry-and-quit
    (evaluate-entry com-call-thunk-and-exit message-window
     :gesture :select 
     :priority 9
     :documentation "Evaluate Entry And Quit")
    (thnk)
  (list thnk))

(define-presentation-type evaluate-entry-persist ())
(define-presentation-to-command-translator eventry-p
    (evaluate-entry-persist com-call-thunk message-window
     :gesture :select 
     :priority 1
     :documentation "Evaluate Entry")
    (thnk)
  (list thnk))

(define-presentation-to-command-translator eventry-and-quit-p
    (evaluate-entry-persist com-call-thunk-and-exit message-window
     :gesture :select 
     :priority 0
     :documentation "Evaluate Entry And Quit")
    (thnk)
  (list thnk))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Display Function ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defun display-message-window (frame pane)
  (with-sheet-medium (medium pane)
    (move-resize-message-window frame pane)
    (loop for x from 0
          for string in (message-window-strings frame)
          with highlight = (message-window-highlight frame)
          do (with-output-as-presentation
                 (pane (cdr string) (if (message-window-persistent frame)
                                        'evaluate-entry-persist
                                        'evaluate-entry))
               (if (= x highlight)
                   (bold (pane)
                     (format pane "~&~A~%" (car string)))
                   (format pane "~&~A~%" (car string)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Messaging Function ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun clim-message (messages &key repeatable-actions
                                (screen (stumpwm:current-screen))
                                (head (stumpwm:current-head))
                                (highlight -1)
                                (bind-keys t))
  "Run a message-window frame, with the string list provided, "
  (let ((frame (or *message-window-frame*
                   (make-application-frame 'message-window)))
        (thread-name (format nil "Message-Window-~A" (gensym))))
    (unless *message-window-frame*
      (setf *message-window-frame* frame))
    (macrolet ((set-slot (slot thing)
                 `(setf (slot-value frame ,slot) ,thing)))
      (set-slot 'strings messages)
      (set-slot 'persistent repeatable-actions)
      (set-slot 'stumpwm-screen screen)
      (set-slot 'stumpwm-head head)
      (set-slot 'highlight highlight))
    (setf (message-window-process frame)
          (sb-thread:make-thread 
           (lambda ()
             (block message-process-block
               (unwind-protect
                    (progn (when bind-keys
                             (stumpwm::push-top-map *clim-message-window-keymap*))
                           (run-frame-top-level frame)
                           (return-from message-process-block
                             (message-window-return frame)))
                 (when bind-keys
                   (stumpwm::pop-top-map)))))
           :name thread-name))
    (values frame
            (message-window-process frame))))

(defun generate-clim-message (list-of-objects &optional
                                                (string-generation-function
                                                 (lambda (x) (format nil "~A" x)))
                                                (operation-function #'identity))
  "Take a list of objects and generate a list suitable for passing to
clim-message.  

LIST-OF-OBJECTS must be a list of objects you wish to act upon.

STRING-GENERATION-FUNCTION must be a function of arity one that returns a
string. It will be called once on every object. By default this is a function
that pretty prints the object to a string via formats ~A directive. These
strings are used to display the object.

OPERATION-FUNCTION must be a function of arity one that operates upon the
object. This will be passed further along to the clim message object, and when
an object is selected, it will be called with that object. By default this is a
function that ignores its argument and returns nil.

Example:

(generate-clim-message
  windows ; from stumpwm
  (lambda (w) (stumpwm:format-expand stumpwm:*window-formatters* fmt w))
  (lambda (w) (stumpwm:group-focus-window group w)))

will construct a list of conses, where each cons' car is a string generated from
stumpwm:format-expand, and its cdr is is a thunk which will call the provided
function with the appropriate window."
  (do ((ls list-of-objects (cdr ls))
       (head nil)
       (tail nil))
      ((null ls) head)
    (let ((newcdr (list (cons (funcall string-generation-function (car ls))
                              (let ((el (car ls)))
                                (lambda ()
                                  (funcall operation-function el)))))))
      (if head
          (setf (cdr tail) newcdr
                tail (cdr tail))
          (setf head newcdr
                tail head)))))

(defun message (things &rest rest &key formatter operator &allow-other-keys)
  "Display things as formatted by FORMATTER using clim-message. Return the
object returned by OPERATOR."
  (remf rest :formatter)
  (remf rest :operator)
  (apply #'clim-message
         (generate-clim-message things
                                (or formatter (lambda (x) (format nil "~A" x)))
                                (or operator #'identity))
         rest))

(defun message-window-current-value (&key default timeout (wait t))
  (if *message-window-frame*
      (message-window-return *message-window-frame* :wait wait
                                                    :timeout timeout
                                                    :default default)
      (values default :default)))

(defun message-window-final-value (&key process
                                     default timeout)
  (let ((proc (or process
                  (and *message-window-frame*
                       (message-window-process *message-window-frame*)))))
    (if (and proc (typep proc 'sb-thread:thread))
        (sb-thread:join-thread proc :default default :timeout timeout)
        (values default :default))))

(defun select-from-menu (things &rest keys)
  (multiple-value-bind (frame process)
      (apply #'message things keys)
    (declare (ignore frame))
    (message-window-final-value :process process)))
