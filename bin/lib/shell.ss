(import :std/misc/process)
(export darwin? sh)

(def (sh . command-line)
  (display "#")
  (for-each (lambda (word)
              (display " ")
              (display word))
            command-line)
  (newline)
  (run-process command-line stdout-redirection: #f stderr-redirection: #f))

(def darwin?
  (let (uname (delay (run-process ["uname"])))
    (lambda ()
      (string=? "Darwin\n" (force uname)))))
