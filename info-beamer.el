;;; info-beamer.el --- Send commands to info-beamer         -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Daniel Kraus

;; Author: Daniel Kraus <daniel@kraus.my>
;; Version: 0.1
;; Package-Requires: ((osc "0.1") (emacs "24.4"))
;; Keywords: tools, processes, comm
;; URL: https://github.com/dakra/info-beamer.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Send data to your info-beamer node

;;; Code:

(require 'thingatpt)

(defgroup info-beamer nil
  "info-beamer"
  :prefix "info-beamer"
  :group 'tools)

(defcustom info-beamer-binary-path "info-beamer"
  "Path to the info-beamer executable."
  :type 'file)

(defcustom info-beamer-udp-host "127.0.0.1"
  "Info beamer UDP host."
  :type 'string
  :safe #'stringp)

(defcustom info-beamer-udp-port 4444
  "Info beamer UDP port."
  :type 'integer
  :safe #'integerp)

(defcustom info-beamer-tcp-host "127.0.0.1"
  "Info beamer TCP host."
  :type 'string
  :safe #'stringp)

(defcustom info-beamer-tcp-port 4444
  "Info beamer TCP port."
  :type 'integer
  :safe #'integerp)

(defvar info-beamer-udp-process nil)
(defvar info-beamer-tcp-process nil)

(defun info-beamer-get-current-node ()
  "Return current directory."
  (file-name-nondirectory (directory-file-name default-directory)))

(defun info-beamer-get-udp-process ()
  "Get info-beamer UDP network connection."
  (if (and info-beamer-udp-process (process-live-p info-beamer-udp-process))
      info-beamer-udp-process
    (setq info-beamer-udp-process
          (make-network-process
           :name "info-beamer-udp-process"
           :host info-beamer-udp-host
           :service info-beamer-udp-port
           :type 'datagram))))

(defun info-beamer-delete-udp-process ()
  "Deletes UDP process to info-beamer."
  (interactive)
  (when (and info-beamer-udp-process (process-live-p info-beamer-udp-process))
    (delete-process info-beamer-udp-process)))

(defun info-beamer-get-tcp-process ()
  "Get info-beamer TCP network connection."
  (if (and info-beamer-tcp-process (process-live-p info-beamer-tcp-process))
      info-beamer-tcp-process
    (setq info-beamer-tcp-process
          (make-network-process
           :name "info-beamer-tcp-process"
           :host info-beamer-tcp-host
           :service info-beamer-tcp-port
           :filter #'info-beamer-tcp-listen-filter))))

(defun info-beamer-tcp-listen-filter (_proc str)
  "Log output STR from info-beamer TCP connection."
  (message "Info-beamer: %s" str))

(defun info-beamer-disconnect ()
  "Deletes TCP process to info-beamer."
  (interactive)
  (when (and info-beamer-tcp-process (process-live-p info-beamer-tcp-process))
    (delete-process info-beamer-tcp-process)))

;;;###autoload
(defun info-beamer-connect (&optional node)
  "Connect to info-beamer node NODE."
  (interactive)
  (let ((node (or node (info-beamer-get-current-node))))
    (info-beamer-input node)))

(defun info-beamer-input (line)
  "Send LINE via TCP to info-beamer."
  (interactive "sSend line to info beamer: ")
  (let ((process (info-beamer-get-tcp-process)))
    (process-send-string process (concat line "\n"))))

;;;###autoload
(defun info-beamer-data (data &optional node)
  "Send DATA via UDP to info-beamer NODE.
If NODE is NIL use current directory."
  (interactive "sSend data to info beamer: ")
  (let* ((process (info-beamer-get-udp-process))
         (node (or node (info-beamer-get-current-node)))
         (path (format "%s:%s" node data)))
    (process-send-string process path)))

;;;###autoload
(defun info-beamer-osc (suffix &optional node &rest args)
  "Send OSC packet with ARGS to path /NODE/SUFFIX."
  (interactive "sSend OSC packet to path: ")
  (if (require 'osc nil t)
      (let* ((process (info-beamer-get-udp-process))
             (node (or node (info-beamer-get-current-node)))
             (path (format "/%s/%s" node suffix)))
        (message "Sending OSC message to %s" path)
        (apply 'osc-send-message process path args))
    (error "You need to install OSC to send OSC packets")))

;;;###autoload
(defun info-beamer-run (&optional node)
  "Run info-beamer NODE or current directory."
  (interactive)
  (let* ((path (or node "."))
         (command (format "%s %s" info-beamer-binary-path path))
         (buf-name (format "info-beamer %s%s"
                           (info-beamer-get-current-node)
                           (if node (format "/%s" node) ""))))
    (compilation-start command nil (lambda (_mode) buf-name))))

;;;###autoload
(defun info-beamer-doc (&optional anchor)
  "Open info-beamer documentation for ANCHOR or symbol under cursor."
  (interactive)
  (let* ((symbol (or anchor (thing-at-point 'filename)))
         (anchor (if symbol (concat "#" symbol) ""))
         (url (format "https://info-beamer.com/doc/info-beamer%s" anchor)))
    (browse-url url)))

(provide 'info-beamer)
;;; info-beamer.el ends here
