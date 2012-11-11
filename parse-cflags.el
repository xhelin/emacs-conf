;;; find makefile
;;;

(require 'mode-local)
(require 'semantic/analyze)
(defvar clang-flags-file ".clang")
(defvar clang-src-extensions '("cpp" "cc" "c" "cxx"))

(defun exists-makefile-p (dir)
  (let ((dir-name (file-name-as-directory dir)))
    (some (lambda (f)
            (file-exists-p (concat dir-name f))) '("Makefile" "makefile" "makefile.mk" "Makefile.mk"))))

(defun get-make-output-list ()
  (let ((tmp-buffer (get-buffer-create "*make-print*"))
        (result nil))
    (with-current-buffer tmp-buffer
      (unwind-protect
          (progn (call-process "make" nil '("*make-print*" nil) nil "-Bn")
                 (split-string (buffer-string) "\n" t))
        (and (buffer-name tmp-buffer)
             (kill-buffer tmp-buffer))))))

(defun find-compile-command (file-name)
  (let ((lines (get-make-output-list)))
    (some (lambda (l)
            (if (string-match (format "\\b%s\\b" file-name) l)
                l)) lines)))

(defun find-flags (file-name)
  (if (exists-makefile-p default-directory)
      (let ((cmd (find-compile-command file-name)))
        (append (find-include-flags cmd) (find-define-flags cmd)))))

(defun find-include-flags (cmd)
  (let ((result nil))
    (if cmd
        (dolist (flag (split-string cmd))
          (setq case-fold-search nil)
          (if (string-match "\\(-I\\)\\(.*\\)" flag)
              (setq result (cons (match-string 2 flag) result)))))
    (mapcar (lambda (x)
              (concat "-I" (expand-file-name x))) result)))

(defun find-define-flags (cmd)
  (let ((result nil))
    (if cmd
        (dolist (flag (split-string cmd))
          (setq case-fold-search nil)
          (if (string-match "\\(-D.+\\)" flag)
              (setq result (cons (match-string 1 flag) result)))))
    result))

(defun up-traverse-dir ()
  (if (string= default-directory "/")
      (setq tmp (cons default-directory tmp))
    (progn (format default-directory)
           (setq tmp (cons default-directory tmp))
           (cd "..")
           (up-traverse-dir))))

(defun recursive-find-flags (file-name)
  (labels ((iter ()
                 (if (string= default-directory "/")
                     (find-flags file-name)
                   (let ((flags (find-flags file-name)))
                     (if flags
                         flags
                       (progn (cd "..")
                              (iter)))))))
    (with-temp-buffer (iter))))

(defun find-buffer-file-flags ()
  (interactive)
  (recursive-find-flags (file-name-nondirectory (buffer-file-name))))

(defun write-clang-flags ()
  (let ((flags (find-buffer-file-flags)))
    (if (and (file-writable-p (file-name-directory (buffer-file-name))) flags)
        (with-temp-file clang-flags-file
          (dolist (f flags)
            (insert f)
            (insert "\n"))
          t))))

(defun read-clang-flags ()
  (if (file-exists-p clang-flags-file)
      (let ((buf (find-file-noselect clang-flags-file)))
        (with-current-buffer buf
          (unwind-protect
              (split-string (buffer-string) "\n" t)
            (and (buffer-name buf)
                 (kill-buffer buf)))))))

(defun clang-src-p (file-name)
  (some (lambda (x)
          (string= x (file-name-extension file-name))) clang-src-extensions))

(defun set-local-clang-flags ()
  (interactive)
  (if (clang-src-p (buffer-file-name))
      (progn (make-local-variable 'ac-clang-flags)
             (make-local-variable 'c-macro-cppflags)
             (cond ((file-exists-p clang-flags-file)
                    (setq ac-clang-flags (append ac-clang-flags (read-clang-flags))))
                   ((write-clang-flags) (setq ac-clang-flags (append ac-clang-flags (read-clang-flags)))))
             (defvar-mode-local c++-mode c-macro-cppflags (mapconcat 'identity ac-clang-flags " "))
             (let ((dirs nil))
               (dolist (flag ac-clang-flags)
                 (if (string-match "^\\(-I\\)\\(.*\\)" flag)
                     (setq dirs (cons (match-string 2 flag) dirs))))
               (make-local-variable 'semantic-c-dependency-system-include-path)
               (setq semantic-c-dependency-system-include-path nil)
               (setq semantic-c-dependency-system-include-path (append semantic-c-dependency-system-include-path dirs))
               (dolist (dir semantic-c-dependency-system-include-path)
                 (semantic-add-system-include dir))))))
