;;;; swm-ssh.asd

(asdf:defsystem "swm-ssh"
  :description "A simple menu selector for ssh to a remote host for stumpwm that parses your ssh config to get available hosts"
  :author "haris@2f30.org"
  :license  "GPLv3"
  :version "0.1"
  :serial t
  :depends-on ("stumpwm")
  :components ((:file "package")
               (:file "swm-ssh")))
