;;; letterbox-mode.el --- hide sensitive text on a buffer -*- lexical-binding: t; -*-

;; Author: Fernando Leboran <f.leboran@gmail.com>
;; URL: http://github.com/pacha64/letterbox-mode
;; Version: 0.3
;; Keywords: password, convenience
;; URL: http://www.github.com/pacha64/letterbox-mode
;; Package-Requires: ((emacs "24.3"))

;; Copyright (C) 2017 Fernando Leboran
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.	 If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Letterbox-mode is a simple minor mode to add letterboxing to sensitive text.
;;
;; Select a region of text you want to censor and call M-x
;; letterbox-add, the selected region will be letterboxed/censored.
;; Call M-x letterbox-remove to remove all letterboxes, or M-x
;; letterbox-toggle to toggle the letterboxes (without removing them).
;; Some use cases:
;;
;;	 This is a buffer with some sensitive information. 123456 is
;;	 the password for my bank account, and qwerty is my account
;;	 name. please don't read it.
;;
;; Select "123456" and call letterbox-add, you will see that part
;; letterboxed:
;;
;;	 This is a buffer with some sensitive information. ██████ is
;;	 the password for my bank account, and qwerty is my account
;;	 name. please don't read it.
;;
;; Select "qwerty" and call letterbox-add again, you will see that
;; part letterboxed as well:
;;
;;	 This is a buffer with some sensitive information. ██████ is
;;	 the password for my bank account, and ██████ is my account
;;	 name. please don't read it.
;;
;; Call letterbox-toggle to hide/show the sensitive text, or
;; letterbox-remove to remove them.

;;; Code:

(defvar letterbox-current-text nil
  "Letterboxed regions of the current buffer.

Each element is a cons cell (BEG . END) of markers, so the
letterboxes follow the text when the buffer is edited.")
(make-variable-buffer-local 'letterbox-current-text)

(defvar letterbox-is-visible t
  "Whether the letterboxes of the current buffer are displayed.")
(make-variable-buffer-local 'letterbox-is-visible)

(defgroup letterbox nil
  "Letterbox text in current buffer."
  :group 'convenience
  :prefix "letterbox-")

(defun letterbox--color ()
  "Return the color used to draw a letterbox.

The text is drawn in this color on a background of the same
color, so a letterbox looks like a solid, unreadable block.  The
color is taken from the foreground of the `default' face so it
follows the current theme."
  (let ((color (face-attribute 'default :foreground nil t)))
    (if (or (not (stringp color))
            (member color '("unspecified-fg" "unspecified-bg")))
        "grey40"
      color)))

(defface letterbox-face
  '((t :background "grey40" :foreground "grey40"))
  "Face used to hide letterboxed text.

This is the fallback used for the letterbox color; the actual
colors are computed from the `default' face by
`letterbox-refresh-overlays', see `letterbox--color'."
  :group 'letterbox)

(defun letterbox--bounds (letterbox)
  "Return the (BEG . END) markers described by LETTERBOX.

LETTERBOX is normally a cons of markers, but plain buffer
positions (the format used by older versions of this package) are
accepted and converted on the fly."
  (let* ((beg (car letterbox))
         (end (if (consp (cdr letterbox)) (cadr letterbox) (cdr letterbox))))
    (unless (markerp beg) (setq beg (copy-marker beg)))
    (unless (markerp end) (setq end (copy-marker end)))
    (cons beg end)))

(defun letterbox--normalize ()
  "Rewrite `letterbox-current-text' as (BEG . END) pairs of markers.

This drops entries that do not describe two buffer positions, so
that stale data (for example buffer positions stored by an older
version of this package) cannot break the commands."
  (setq letterbox-current-text
        (delq nil
              (mapcar (lambda (letterbox)
                        (when (and (consp letterbox)
                                   (integer-or-marker-p (car letterbox))
                                   (integer-or-marker-p
                                    (if (consp (cdr letterbox))
                                        (cadr letterbox)
                                      (cdr letterbox))))
                          (letterbox--bounds letterbox)))
                      letterbox-current-text))))

;;;###autoload
(define-minor-mode letterbox-mode
  "Mode for hiding sensitive text on a buffer.

After activating this mode in current buffer, select region and
press \\[letterbox-add] to letterbox, press \\[letterbox-toggle] to
toggle letterbox visibility, press \\[letterbox-remove] to remove
all active letterboxes.

Turning the mode off only hides the letterboxes of the buffer;
they are redisplayed when the mode is turned on again."
  :init-value nil
  :lighter " Letterbox"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c a") 'letterbox-add)
            (define-key map (kbd "C-c t") 'letterbox-toggle)
            (define-key map (kbd "C-c r") 'letterbox-remove)
            map)
  (if letterbox-mode
      (letterbox-refresh-overlays)
    (remove-overlays (point-min) (point-max) 'category 'letterbox)))

;;;###autoload
(defun letterbox-add ()
  "Letterbox the active region of the current buffer."
  (interactive)
  (if (not (use-region-p))
      (message "No region active to add text to a letterbox.")
    (letterbox--normalize)
    (push (cons (copy-marker (region-beginning))
                (copy-marker (region-end)))
          letterbox-current-text)
    (setq letterbox-is-visible t)
    (deactivate-mark)
    (letterbox-refresh-overlays)
    (message "Letterbox added")))

;;;###autoload
(defun letterbox-remove ()
  "Remove all the letterboxes of the current buffer."
  (interactive)
  (letterbox--normalize)
  (dolist (letterbox letterbox-current-text)
    (set-marker (car letterbox) nil)
    (set-marker (cdr letterbox) nil))
  (setq letterbox-current-text nil)
  (letterbox-refresh-overlays)
  (message "All letterboxes removed"))

;;;###autoload
(defun letterbox-toggle ()
  "Hide or show the letterboxes of the current buffer."
  (interactive)
  (setq letterbox-is-visible (not letterbox-is-visible))
  (message "Letterbox %s" (if letterbox-is-visible "enabled" "disabled"))
  (letterbox-refresh-overlays))

(defun letterbox-refresh-overlays ()
  "Redraw the letterboxes of the current buffer."
  (letterbox--normalize)
  (remove-overlays (point-min) (point-max) 'category 'letterbox)
  (when (and letterbox-is-visible letterbox-current-text)
    (let ((color (letterbox--color)))
      (dolist (letterbox letterbox-current-text)
        (let ((beg (car letterbox))
              (end (cdr letterbox)))
          (when (and (markerp beg) (marker-position beg)
                     (markerp end) (marker-position end)
                     (< (marker-position beg) (marker-position end)))
            (let ((ov (make-overlay beg end)))
              (overlay-put ov 'category 'letterbox)
              (overlay-put ov 'evaporate t)
              ;; In a face list the earlier faces take precedence, so
              ;; the computed colors win over `letterbox-face'.
              (overlay-put ov 'face
                           (list (list :background color
                                       :foreground color)
                                 'letterbox-face)))))))))

(provide 'letterbox-mode)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; letterbox-mode.el ends here
