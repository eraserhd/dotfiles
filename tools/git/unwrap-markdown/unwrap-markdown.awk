# Joins the lines of each paragraph and list item, since GitHub shows every
# newline in a pull request or issue body as a line break.  Code, quotes,
# tables, headings, HTML and lines ending in a hard break are left be.

function flush() {
    if (have)
        print buf
    have = 0
}

function starts_block(line) {
    return line ~ /^ {0,3}#{1,6}( |$)/ ||
        line ~ /^ *([-*+]|[0-9]+[.)])( |$)/ ||
        line ~ /^ {0,3}>/ ||
        line ~ /^ *\|/ ||
        line ~ /^ {0,3}</ ||
        line ~ /^ {0,3}(=+|-+) *$/ ||
        line ~ /^ {0,3}((\* *){3,}|(_ *){3,})$/
}

function ends_paragraph(line) {
    return line ~ /^ {0,3}#/ || line ~ /^ *\|/ || line ~ /^ {0,3}</ ||
        line ~ /-->$/ || line ~ /(  |\\)$/
}

{
    line = $0
    if (fence != "") {
        print line
        if (line ~ ("^ {0,3}" fence))
            fence = ""
        next
    }
    if (match(line, /^ {0,3}(```|~~~)/)) {
        flush()
        fence = substr(line, RSTART + RLENGTH - 3, 3)
        print line
        previous = "fence"
        next
    }
    if (line ~ /^[ \t]*$/) {
        flush()
        print line
        previous = "blank"
        next
    }
    if ((previous == "blank" || previous == "code") && line ~ /^(    |\t)/) {
        flush()
        print line
        previous = "code"
        next
    }
    if (have && joinable && !starts_block(line)) {
        sub(/^[ \t]+/, "", line)
        buf = buf " " line
        joinable = !ends_paragraph(line)
        next
    }
    flush()
    buf = line
    have = 1
    joinable = !ends_paragraph(line)
    previous = "text"
}

END {
    flush()
}
