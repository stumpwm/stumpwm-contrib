;; Spatial Groups for StumpWM
;;
;; Copyright (C) 2022 Russell Adams
;;
;; Maintainer: Russell Adams <rladams@adamsinfoserv.com>
;;

;; This module is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) any later version.
;;
;; This module is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this software.  If not, see <http://www.gnu.org/licenses/>.

;;; Usage:
;;
;; Just add the following line to your .stumpwmrc file:
;;
;; (load-module :stumpwm-spatial)
;;

(in-package :spatial-groups)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define global vars

(defparameter *z-cursors* '()
  "Association list of groups by Z coordinate for preserving last position on that plane.")

(defparameter *last-cursor* NIL
  "Previous group name string for popping back.")

(defparameter *spatial-banish-on-move* t
  "Banish the cursor to bottom right when switching screens? Defaults to true.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Coordinate commands

;; Control arrows between groups
(defcommand coord-left      () () (coord-group-change -1  0  0))
(defcommand coord-right     () () (coord-group-change  1  0  0))
(defcommand coord-up        () () (coord-group-change  0  1  0))
(defcommand coord-down      () () (coord-group-change  0 -1  0))
(defcommand coord-taskleft  () () (coord-group-change  0  0 -1))
(defcommand coord-taskright () () (coord-group-change  0  0  1))

;; return to 0,0 on the current taskplane as a shortcut to return to the core task
(defcommand coord-taskorigin () ()
  (gselect
   (group-name
    (spatial-find-group (current-screen)
                   (format nil "~{~a~^,~}" (list 0 0 (parse-integer (third (split-string (group-name (current-group)) ",")))))))))

;; pop back to last location
(defcommand coord-taskpop () () (when *last-cursor* (spatial-gselect *last-cursor*)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key bindings for spatial group movement

(defun install-default-keybinds ()
  "Install default keybinds. Use this as an example for making your own keybinds."

  ;; Shift arrows between adjacent frames on the current screen
  (define-key *top-map* (kbd "S-Up")      "move-focus up")
  (define-key *top-map* (kbd "S-Down")    "move-focus down")
  (define-key *top-map* (kbd "S-Left")    "move-focus left")
  (define-key *top-map* (kbd "S-Right")   "move-focus right")

  ;; Control arrows move between screens on the current desktop
  (define-key *top-map* (kbd "C-Left")    "coord-left")
  (define-key *top-map* (kbd "C-Right")   "coord-right")
  (define-key *top-map* (kbd "C-Up")      "coord-up")
  (define-key *top-map* (kbd "C-Down")    "coord-down")

  ;; Control-Shift left/right to switch desktop Z
  (define-key *top-map* (kbd "C-S-Left")  "coord-taskleft")
  (define-key *top-map* (kbd "C-S-Right") "coord-taskright")

  ;; Control-Shift-Up to return to origin 0,0 on current desktop Z
  (define-key *top-map* (kbd "C-S-Up")    "coord-taskorigin")

  ;; "Pop" back to last desktop position
  (define-key *top-map* (kbd "C-S-Down")  "coord-taskpop") )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spatial groups and coordinate functions

;; Groups will manage the coordinate system
;; format: 0,0,0 with positive and negative numbers
;; create groups on the fly as needed
;; only supports one screen atm, would like multimonitor support later

(defun spatial-find-group (screen name)
  "Simple function to find group by string name"
  (find name (screen-groups screen) :key 'group-name :test 'string=))

(defun spatial-gselect (name)
  "Wrap gselect, preserve prior location for pop, and handle when group is new"
  (when *spatial-banish-on-move* (banish))
  (setf *last-cursor* (group-name (current-group)))
  (gselect (group-name
             (or (spatial-find-group (current-screen) name)
               (gnew name)))))

(defun coord-group-change (xo yo zo)
  "Navigate a 3d array of groups using x,y,z coordinates by passing the offset of the change."
  (let* ((current-coords
          (mapcar #'parse-integer
                  (split-string (group-name (current-group)) ",")))
         (new-coords (mapcar #'+ current-coords (list xo yo zo)))
         (new-group-name (format nil "~{~a~^,~}" new-coords)) )

    (if (= 0 zo)

        ;; Not changing desktop, so just move by coordinates
        (spatial-gselect new-group-name)

        ;; Changing Z across desktops, RESTORE z-cursor for that plane
        (let ((old-z (third current-coords))
              (new-z (third new-coords)))

          ;; Save current location for desktop so we can return to same spot
          (if (assoc old-z *z-cursors*)
              (setf (cdr (assoc old-z *z-cursors*)) (current-group))
              (push (cons old-z (current-group)) *z-cursors*))

          ;; Try to restore prior desktop location
          (let ((z-cursor (cdr (or (assoc new-z *z-cursors*) '(nil . nil)))))
            (if z-cursor
                ;; Restore saved location
                (spatial-gselect (group-name z-cursor))
                ;; create new taskplane
                (spatial-gselect new-group-name)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Startup

(defun spatial-startup ()
  "Must goto 0,0,0 on startup"
  (spatial-gselect "0,0,0"))

(when *initializing* (spatial-startup))
