#!/usr/bin/env gsi

(define (sh command-line)
  (let ((result (shell-command command-line #t)))
    (if (not (= 0 (car result)))
      (begin
        (display (cdr result))
        (newline)
        (error (string-append "error-exit " (number->string (car result)) " from shell command: " command-line))))))

(define (update-kakoune)
  (parameterize ((current-directory "~/src/kakoune"))
    ;; Update master
    (sh "git fetch upstream")
    (sh "git checkout master")
    (sh "git reset --hard upstream/master")
    (sh "git push origin master")
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

(update-kakoune)
