(require 'obj3-mode)
(require 'ansi-color)

(setf obj3-command-options (list "-interactive" "-ansi-color"))
(setf obj3-command
      (if (eq system-type 'windows-nt)
          "d:\\dev\\app\\obj3\\obj3.exe"
        "obj3"))

(swap-parens obj3-mode-map)
(define-key obj3-mode-map "\C-c\C-d" 'obj3-send-paragraph)
(define-key obj3-mode-map "\C-c\C-r" 'obj3-send-region)
(define-key obj3-mode-map (kbd "<f12>") 'my-obj3-send-paragraph)
(define-key obj3-mode-map (kbd "<f6>") 'obj3-send-region)
(define-key obj3-mode-map (kbd "<f5>")
            (lambda ()
              (interactive)
              (scroll-buffer-to-end "*Obj3*")
              (save-buffer)
              (obj3-send-buffer)))

(define-key obj3-mode-map (kbd "<mouse-2>") (at-click 'my-obj3-send-paragraph))

(defun scroll-buffer-to-end (buf)
  (with-current-buffer buf
    (set-window-point (get-buffer-window "*Obj3*") (point-max))))

(defun my-obj3-send-paragraph ()
  (interactive)
  (scroll-buffer-to-end "*Obj3*")
  (obj3-send-paragraph))

(run-obj3)
