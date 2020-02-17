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

(define (update-kakoune)
  (parameterize ((current-directory "~/src/kakoune"))
    (update-branch "master")
    ;; Update dogfood
    (sh "git checkout dogfood")
    (sh "git merge --no-edit master")
    (sh "git push origin dogfood")
    (sh "dogfood")
    (sh "nixos-rebuild build")
    (sh "rm -f result"))
  (parameterize ((current-directory "~/src/dotfiles"))
    (sh "git add dogfood/kakoune.nix")
    (sh "git commit -m 'Bump kakoune' dogfood/kakoune.nix")))

(define (update-nixpkgs)
  (parameterize ((current-directory "~/src/dotfiles/nixpkgs"))
    (update-branch "master"))
  (parameterize ((current-directory "~/src/dotfiles"))
    (sh "nixos-rebuild build")
    (sh "git add nixpkgs")
    (sh "git commit -m 'Bump nixpkgs' nixpkgs")))

;(update-kakoune)
(update-nixpkgs)
