;;; info-beamer.el --- Send commands to info-beamer         -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Daniel Kraus

;; Author: Daniel Kraus <daniel@kraus.my>
;; Version: 0.1
;; Package-Requires: ((request "0.3.0") (emacs "24.4"))
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

(defgroup info-beamer nil
  "info-beamer"
  :prefix "info-beamer"
  :group 'tools)

(defcustom info-beamer-binary-path "info-beamer"
  "Path to the info-beamer executable."
  :type 'file
  :group 'info-beamer)

(defcustom info-beamer-udp-host "127.0.0.1"
  "Info beamer UDP host."
  :type 'string
  :safe #'stringp
  :group 'info-beamer)

(defcustom info-beamer-udp-port 4444
  "Info beamer UDP port."
  :type 'integer
  :safe #'integerp
  :group 'info-beamer)

(defvar info-beamer-network-process nil)

(defun info-beamer-get-current-node ()
  "Return current directory."
  (file-name-nondirectory (directory-file-name default-directory)))

(defun info-beamer-get-network-process ()
  "Get info-beamer network connection."
  (if (and info-beamer-network-process (process-live-p info-beamer-network-process))
      info-beamer-network-process
    (setq info-beamer-network-process
          (make-network-process
           :name "info-beamer-udp-socket"
           :host info-beamer-udp-host
           :service info-beamer-udp-port
           :type 'datagram))))

;;;###autoload
(defun info-beamer-send-string (data &optional path)
  "Send DATA to info-beamer PATH."
  (interactive "sSend string to info beamer: ")
  (let* ((process (info-beamer-get-network-process))
         (path (or path (info-beamer-get-current-node)))
         (str (format "%s:%s" path data)))
    (process-send-string process str)))

;;;###autoload
(defun info-beamer-run (&optional node)
  "Run info-beamer NODE or current directory."
  (interactive)
  (let* ((path (or node "."))
         (command (format "%s %s" info-beamer-binary-path path))
         (buf-name (format "info-beamer %s%s"
                           (info-beamer-get-current-node)
                           (if node (format "/%s" node) ""))))
    (compilation-start command nil (lambda (m) buf-name))))

(provide 'info-beamer)
;;; info-beamer.el ends here
