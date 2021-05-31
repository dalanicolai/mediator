;;; mediator.el --- Launch file in mime-type compatible external application -*- lexical-binding: t; -*-

;; Author: Daniel Laurens Nicolai <dalanicolai@gmail.com>
;; Version: 0.1
;; Created: 28 May 2021
;; Keywords: files, unix
;; Package-Requires: ((emacs "26.1"))
;; URL: https://github.com/dalanicolai/mediator


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
;; 

;;; Code:

(require 'url-parse)
(require 'subr-x)

(require 'all-the-icons-ivy-rich nil t)
(require 'embark nil t)
(require 'ivy nil t)

;; (defvar mediator-data-directories (cl-subseq (split-string (shell-command-to-string "$XDG_DATA_DIRS")":") 2 -1)
;;   "List of directories with xdg data.")

;;;###autoload
(defun mediator-get-mime-type (&optional file arg)
  "Return mime type of currently visited FILE.
When prefixed with universal ARG \\[universal-argument], presents
list of in `/etc/mime.types' defined mime types in completion
menu and insert mime type on selection."
  (interactive (list buffer-file-name
                     current-prefix-arg))
  (if arg
      (let (mime-list)
        (with-temp-buffer
          (insert-file-contents-literally "/etc/mime.types")
          (re-search-forward "# MIME")
          (while (not (eobp))
            (forward-line)
            (push (car (split-string (thing-at-point 'line t))) mime-list)))
        (insert (completing-read "Select mime-type: " (nreverse mime-list))))
    (let ((mime-type (string-trim-right
                      (shell-command-to-string (format "xdg-mime query filetype '%s'" file))
                      "\n")))
      (if (called-interactively-p 'any)
          (print mime-type)
        mime-type))))

;; (defun mediator--get-mime-app-desktop-files (mime-type)
;;   "Retrieve desktop file names of with MIME-TYPE associated applications.
;; MIME-TYPE should be a string that complies with the XDG standard."
;;   (let (dirs-files-alist)
;;     (mapc
;;      (lambda (x)
;;        (when (file-exists-p (concat (string-trim x) "/applications/mimeinfo.cache"))
;;          (let (file-names-list)
;;            (with-temp-buffer
;;              (insert-file-contents-literally (concat (string-trim x)
;;                                                      "/applications/mimeinfo.cache"))
;;              (while (re-search-forward (concat "^" mime-type) nil t)
;;                (setq file-names-list (append file-names-list
;;                                              (split-string
;;                                               (string-trim-right
;;                                                (cadr (split-string
;;                                                       (string-trim-right (thing-at-point 'line t)
;;                                                                          "\n")
;;                                                       "="))
;;                                                ";")
;;                                               ";")))))
;;                   (push (cons x file-names-list) dirs-files-alist))))
;;             (xdg-data-dirs))
;;     dirs-files-alist))

;;;###autoload
(defun mediator-get-desktop-file ()
  "Quickly navigate to and open a desktop file."
  (interactive)
  (let ((initial-dir (completing-read "Select desktop files directory: "
                                      (mapcar (lambda (dir)
                                                (concat (file-name-as-directory dir)
                                                        "applications"))
                                              (xdg-data-dirs)))))
    (find-file (read-file-name "Select desktop file: " initial-dir))
    (when buffer-read-only
      (warn "Buffer is read-only. Run `M-x mediator-sudo-edit' to edit file."))))

(defun mediator-sudo-edit ()
  (interactive)
  (find-file
     (concat "/sudo:root@localhost:" buffer-file-name)))

(defun mediato-update-mime-database ()
  (interactive)
  (when (= (shell-command (concat "echo " (shell-quote-argument (read-passwd "Sudo password? "))
                          " | sudo -S update-desktop-database")) 0)
    (message "Desktop database succesfully updated.")))

(defun mediator--get-app-data (desktop-file-path)
  "Extract application name and relevant part of command from desktop-file.
Argument should be the DESKTOP-FILE-PATH (string)."
  (with-temp-buffer
    (insert-file-contents-literally desktop-file-path)
    (mapcar (lambda (x)
              (goto-char (point))
              (when (re-search-forward x nil t)
                (substring (cadr (split-string (thing-at-point 'line t) "="))
                           0 -1)))
            ;; '("^Name" "^Exec" "^Icon")))))
            '("^Name" "^Exec"))))

;;;###autoload
(defun mediator-open-with (file-path &optional arg)
  "Select application to open file of its mime-type in a separate process.
FILE-PATH should be the full path to the file to open. When
called interactively, the FILE-PATH of the current visited file
is used. When called with a prefix ARG \\[universal-argument],
only print the name and used command of the selected application
but don't open the file.

When selecting the option `default', the `xdg-open' shell script
is used to open the file."
  (interactive (list (url-filename (url-generic-parse-url buffer-file-name)) current-prefix-arg))
  (let* ((mime (if file-path
                   (string-trim-right
                    (shell-command-to-string (format "xdg-mime query filetype '%s'" (expand-file-name file-path)))
                    "\n")
                 (user-error "Buffer not associated with a file")))
         ;; (mimetypes-guess-mime (expand-file-name file-path))))
         (apps (mapcar (lambda (x)
                         (mediator--get-app-data x))
                       (xdg-mime-apps mime)))
         (apps-with-default (cons "default (xdg-open)" apps))
         (app (completing-read "Open file with: "
                               apps-with-default))
         (command (if (string= app "default (xdg-open)")
                      "xdg-open"
                    (car (split-string (car (alist-get app
                                                       apps nil nil 'equal)))))))
    (if arg
        (message "App name (to match in all-the-icons-app-icon-alist): %s.
Command extracted from desktop file: %s."
                 app
                 command)
      (call-process command nil 0 nil file-path)
      (message "Open file in application %s using the command: %s %s"
               (propertize app 'face 'italic)
               (propertize command 'face 'italic)
               (propertize file-path 'face 'italic)))))


;;; Optional definitions for supporting features of some external packages

(when (featurep 'all-the-icons-ivy-rich)
  (defvar all-the-icons-app-icon-alist
    '(("Other"         all-the-icons-faicon "rocket")

      ("\\b[Cc]hrom"   all-the-icons-faicon "chrome"       :height 1.0 :face all-the-icons-lblue)
      ("[Ff]irefox"    all-the-icons-faicon "firefox"                  :face all-the-icons-orange)
      ("Web"           all-the-icons-faicon "globe"        :height 1.0 :face all-the-icons-blue)
      ("qutebrowser"   all-the-icons-faicon "globe"        :height 1.0 :face all-the-icons-blue)

      ("[Ee]macs"      all-the-icons-fileicon "elisp"      :height 1.0 :v-adjust -0.1 :face all-the-icons-purple)))

  (defun all-the-icons-icon-for-app (app &rest arg-overrides)
    "Get the formatted icon for APP.
ARG-OVERRIDES should be a plist containining `:height',
`:v-adjust' or `:face' properties like in the normal icon
inserting functions."
    (let* ((icon (or (all-the-icons-match-to-alist app all-the-icons-app-icon-alist)
                     ;; (cdr (assoc app
                     ;;             all-the-icons-app-icon-alist))
                     (cdr (assoc "Other"
                                 all-the-icons-app-icon-alist))))
           (args (cdr icon)))
      (when arg-overrides (setq args (append `(,(car args)) arg-overrides (cdr args))))
      (apply (car icon) args))))

(when (featurep 'embark)
  (defun embark-open-with (file)
    "Help function to call mediator-open-with from embark.
Embarks passes the FILE as argument."
    (mediator-open-with file)))

(when (featurep 'ivy)
  (ivy-add-actions
   t
   '(("a" mediator-open-with "open-with"))))

(provide 'mediator)

;;; mediator.el ends here
