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

(defcustom info-beamer-udp-remote [127 0 0 1 4444]
  "Info beamer UDP remote."
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
          (make-network-process :name "info-beamer-udp-socket"
                                :remote '[127 0 0 1 4444]
                                :type 'datagram))))

;;;###autoload
(defun info-beamer-send-string (data &optional path)
  "Send DATA to info-beamer PATH."
  (interactive "sSend string to info beamer: ")
  (let* ((process (info-beamer-get-network-process))
         (path (or path (info-beamer-get-current-node)))
         (str (format "%s:%s" path data)))
    (process-send-string process str)))

(provide 'info-beamer)
;;; info-beamer.el ends here
