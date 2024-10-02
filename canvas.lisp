(in-package #:calm)

;;
;; CALM version check
;; version check won't work on JSCL, since the lack of ASDF
;;
#-jscl
(let ((required-version "1.3.1")
      (calm-version (slot-value (asdf:find-system 'calm) 'asdf:version)))
  (when (uiop:version< calm-version required-version)
    (format t
            "Sorry, this is built on CALM ~A, older version (current: ~A) of CALM won't work.~%"
            required-version calm-version)
    (uiop:quit)))


;; DEBUGGING, please uncomment the correspoding line
;;
;; for Emacs - SLIME
;;        https://slime.common-lisp.dev/
;;
(unless (str:starts-with? "dist" (uiop:getenv "CALM_CMD")) (swank:create-server :dont-close t))
;;
;; for LEM - micros
;;        https://github.com/lem-project/micros
;;
;; (unless (str:starts-with? "dist" (uiop:getenv "CALM_CMD")) (micros:create-server :dont-close t))
;;
;; for Alive - Visual Studio Code
;;        https://github.com/nobody-famous/alive
;; please config `alive.lsp.startCommand':
;;
;;        {
;;            "alive.lsp.startCommand": [
;;                "calm",
;;                "alive"
;;            ]
;;        }



;;
;; by default, the screensaver is disabled,
;; if you want to enable screensaver,
;; please uncomment the following line
;;
;; (setf (uiop:getenv "SDL_VIDEO_ALLOW_SCREENSAVER") "1")

;;
;; setting window properties, for others, please check
;;      https://github.com/VitoVan/calm/blob/main/src/config.lisp
;;
(setf *calm-window-width* 300)
(setf *calm-window-height* 250)
(setf *calm-window-title* "Countdown Alarm")


(defparameter *alarm-time* nil)
(defparameter *alarm-digits* nil)
(defparameter *alarm-rang* nil)

(defun time->string (time)
  (multiple-value-bind
        (second minute hour day month year day-of-week dst-p tz)
      (decode-universal-time time)
    (declare (ignore day month year day-of-week dst-p tz))
    (format nil "~2,'0d:~2,'0d:~2,'0d" hour minute second)))

(defun window-is-on-top ()
  (member :always-on-top (sdl2:get-window-flags *calm-window*)))

(defun on-keyup (key)
  (cond
    ((c:keq key :SCANCODE-T)
     (sdl2-ffi.functions:sdl-set-window-always-on-top
      *calm-window*
      (if (window-is-on-top) sdl2-ffi:+false+ sdl2-ffi:+true+)))

    ((c:keq key :SCANCODE-R)
     (setf *alarm-digits* nil
           *alarm-time* nil
           *alarm-rang* nil)
     (c:halt-music))

    ((c:keq key :SCANCODE-BACKSPACE)
     (pop *alarm-digits*))

    ((ppcre:all-matches "[0-9]" (sdl2:scancode-key-name key))
     (format t "~%NUMBER PRESSED: ~a~%" (sdl2:scancode-key-name key))
     (when (< (length *alarm-digits*) 6)
         (push (parse-integer (sdl2:scancode-key-name key)) *alarm-digits*))
     (when (= (length *alarm-digits*) 6)
         (multiple-value-bind
               (second minute hour day month year day-of-week dst-p tz)
             (get-decoded-time)
           (declare (ignore second minute hour day-of-week dst-p tz))
           (setf *alarm-time*
                 (encode-universal-time
                  (+ (nth 0 *alarm-digits*) (* 10 (nth 1 *alarm-digits*)))
                  (+ (nth 2 *alarm-digits*) (* 10 (nth 3 *alarm-digits*)))
                  (+ (nth 4 *alarm-digits*) (* 10 (nth 5 *alarm-digits*)))
                  day month year)))))

    (t (format t "~%KEY PRESSED: ~a~%" (sdl2:scancode-key-name key)))

    ))

(defun draw-setting ()
  (c:set-source-rgb (/ 12 255) (/ 55 255) (/ 132 255))
  (c:paint)
  (c:set-source-rgb 1 1 1)


  (c:move-to 32 220)
  (c:set-font-size 42)

  (c:set-source-rgb 0.94 0.87 0.47)

  (c:with-state
    (c:select-font-face "Noto Emoji" :normal :normal)
    (c:show-text "⏰"))

  (c:show-text
   (format nil "~a~a:~a~a:~a~a"
           (or (nth 0 (reverse *alarm-digits*)) "_")
           (or (nth 1 (reverse *alarm-digits*)) "_")
           (or (nth 2 (reverse *alarm-digits*)) "_")
           (or (nth 3 (reverse *alarm-digits*)) "_")
           (or (nth 4 (reverse *alarm-digits*)) "_")
           (or (nth 5 (reverse *alarm-digits*)) "_"))
   )


  (c:set-font-size 22)
  
  (c:move-to 40 60)
  (c:show-text "press ")
  (c:with-state
    (c:set-source-rgb 1 1 1)
    (c:select-font-face "monospace" :normal :normal)
    (c:show-text "0-9"))
  (c:show-text " to set alarm")

  (c:move-to 40 100)
  (c:show-text "press ")
  (c:with-state
    (c:set-source-rgb 1 1 1)
    (c:select-font-face "monospace" :normal :normal)
    (c:show-text " r "))
  (c:show-text " to reset")  
  
  (c:move-to 40 140)
  (c:show-text "press ")
  (c:with-state
    (c:set-source-rgb 1 1 1)
    (c:select-font-family "monospace" :normal :normal)
    (c:show-text " t "))
  (c:show-text " to float on top"))

(defun draw-alarm ()
  (c:set-source-rgb (/ 12 255) (/ 55 255) (/ 132 255))
  (c:paint)

  #-linux
  (c:select-font-face "Arial" :normal :normal)
  #+linux
  (c:select-font-face "Liberation Sans" :normal :normal)

  (c:move-to 32 50)
  (c:set-font-size 42)
  (c:set-source-rgb 0.6 0.6 0.6)
  (c:with-state
    (c:select-font-face "Noto Emoji" :normal :normal)
    (c:show-text "⌚️"))

  (c:show-text (time->string (get-universal-time)))

  (c:move-to 20 140)
  (c:set-font-size 66)
  (c:set-source-rgb 1 1 1)
  (let* ((time-span (- *alarm-time* (get-universal-time)))
         (hour (truncate (/ (abs time-span) 3600)))
         (minute (truncate (/ (mod (abs time-span) 3600) 60)))
         (second (mod (abs time-span) 60)))
    (when (< time-span 0)
      (c:set-source-rgb 1 0.1 0.1)
      (unless *alarm-rang*
        (c:play-music "assets/alarm.mp3")
        (setf *alarm-rang* t)))
    (c:show-text (format nil "~2,'0d:~2,'0d:~2,'0d" hour minute second)))

  (c:move-to 32 220)
  (c:set-font-size 42)

  (c:set-source-rgb 0.94 0.87 0.47)

  (c:with-state
    (c:select-font-face "Noto Emoji" :normal :normal)
    (c:show-text "⏰"))

  (c:show-text (time->string *alarm-time*)))

(defun draw-forever ()
  (if *alarm-time*
      (draw-alarm)
      (draw-setting)))
