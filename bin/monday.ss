#!/usr/bin/env gsi

(define (sh command-line)
  (display (string-append "# " command-line))
  (newline)
  (let ((result (shell-command command-line #t)))
    (if (not (= 0 (car result)))
      (begin
        (display (cdr result))
        (newline)
        (error (string-append "error-exit " (number->string (car result)) " from shell command: " command-line))))))

(define (update-branch branch)
  (sh "git fetch upstream")
  (sh (string-append "git checkout " branch))
  (sh (string-append "git reset --hard upstream/" branch))
  (sh (string-append "git push origin " branch)))

(define (update-nixpkgs)
  (parameterize ((current-directory "~/src/dotfiles/nixpkgs"))
    (update-branch "master"))
  (parameterize ((current-directory "~/src/dotfiles"))
    (sh "nixos-rebuild build")
    (sh "git add nixpkgs")
    (sh "git commit -m 'Bump nixpkgs' nixpkgs")))

(update-nixpkgs)
