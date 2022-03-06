;;; org-roam-desktop.el --- Consult integration for org-roam  -*- lexical-binding: t; -*-

;;; Commentary:
;; Copyright (C) 2022 jgru
;; based on Grant Rosson's zk-implementation

;;; Code:
(require 'org-roam)

(defgroup org-roam-desktop nil
  "Desktop interfaces for org-roam."
  :group 'text
  :group 'files
  :prefix "org-roam-desktop")

(defcustom org-roam-desktop-directory nil
  "Directory for saved org-roam-Desktops."
  :type 'directory)

(defcustom org-roam-desktop-basename "*Org-Roam-Desktop:"
  "Basename for org-roam-Desktops.
The names of all org-roam-Desktops should begin with this string."
  :type 'string)

(defvar org-roam-desktop-current nil)

(defun org-roam-desktop ()
  "Open org-roam-Desktop."
  (interactive)
  (let ((buffer (if (and org-roam-desktop-current
                         (buffer-live-p (get-buffer org-roam-desktop-current)))
                    org-roam-desktop-current
                  (org-roam-desktop-select)))
        (choice (read-char "Choice: \[s\]witch or \[p\]op-up?")))
    (pcase choice
      ('?s (switch-to-buffer buffer))
      ('?p (pop-to-buffer buffer
                          '(display-buffer-at-bottom)))
      (_ (org-roam-desktop)))))


;;;###autoload
(defun org-roam-desktop-select ()
  "Select a org-roam-Desktop to work with."
  (interactive)
  (let* ((last-command last-command)
         (desktop
          (completing-read "Select or Create org-roam-Desktop: "
                           (directory-files
                            org-roam-desktop-directory
                            nil
                            (concat
                             org-roam-desktop-basename
                             ".*"))
                           nil nil org-roam-desktop-basename))
         (file (concat org-roam-desktop-directory "/" desktop)))
    (if (file-exists-p (expand-file-name file))
        (setq org-roam-desktop-current
              (find-file-noselect file))
      (progn
        (generate-new-buffer desktop)
        (setq org-roam-desktop-current desktop)))
    (with-current-buffer org-roam-desktop-current
      (setq require-final-newline 'visit-save)
      (unless (bound-and-true-p truncate-lines)
        (toggle-truncate-lines))
      (set-visited-file-name file t t)
      (setq org-roam-desktop-mode t)
      (org-mode)
      (save-buffer))
    (if (and (not (eq last-command 'org-roam-desktop))
             (y-or-n-p (format "Visit %s? " org-roam-desktop-current)))
        (progn
          (switch-to-buffer org-roam-desktop-current)
          (setq org-roam-desktop-mode t))
      (message "Desktop set to: %s" org-roam-desktop-current)))
  org-roam-desktop-current)

;;###autoload
(defun org-roam-switch-to-desktop ()
  "Switch to ORG-ROAM-Desktop.
With prefix-argument, raise ORG-ROAM-Desktop in other frame."
  (interactive)
  (unless (and org-roam-desktop-current
               (buffer-live-p (get-buffer org-roam-desktop-current)))
    (org-roam-desktop-select))
  (let ((buffer org-roam-desktop-current))
    (if current-prefix-arg
        (if (get-buffer-window buffer 'visible)
            (display-buffer-pop-up-frame
             buffer
             ;; not general
             '((pop-up-frame-parameters . ((top . 80)
                                           (left . 850)
                                           (width . 80)
                                           (height . 35)))))
          (switch-to-buffer-other-frame buffer))
      (switch-to-buffer buffer)
      (org-mode))))

(defun org-roam-desktop-node-find-and-add (&optional initial-input filter-fn)
  "Find and send a node to a desktop."
  (interactive)
  (let ((node (org-roam-node-read initial-input filter-fn)))
    (if (org-roam-node-file node)
        (org-roam-desktop-node-add node))))

(defun org-roam-desktop-node-add (&optional node)
  "Send provided node or node at point to a desktop."
  (interactive)
  (unless org-roam-desktop-directory
    (error "Please set 'org-roam-desktop-directory'"))
    (let ((buffer)
        (node (if node node
          (when (not (org-roam-file-p (buffer-file-name)))
               (org-roam-desktop-node-find-and-add))
             (org-roam-node-at-point)))
        (link (org-link-make-string
                  (concat "id:" (org-roam-node-id node))
                   (org-roam-node-title node))))
    (if (and org-roam-desktop-current
             (buffer-live-p (get-buffer org-roam-desktop-current)))
        (setq buffer org-roam-desktop-current)
      (setq buffer (org-roam-desktop-select)))
    (unless (get-buffer buffer)
      (generate-new-buffer buffer))
    (with-current-buffer buffer
      (setq org-roam-desktop-mode t)
      (setq require-final-newline 'visit-save)
       (progn
         (goto-char (point-max))
          (beginning-of-line)
          (org-insert-heading)
          (insert link "\n#+transclude: " link)))
    (if (string= (buffer-name) "*Org-Roam*")
        (message "Sent to %s - press D to switch" buffer)
      (message "Sent to %s" buffer))))

(provide 'org-roam-desktop)
;;; org-roam-desktop.el ends here
