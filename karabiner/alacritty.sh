#!/bin/bash

modifiers=( left_option right_option )
key_codes=( a b c d e f g h i j k l m n o p q r s t u v w x y z 
            0 1 2 3 4 5 6 7 8 9 
            backslash
            close_bracket
            comma
            down_arrow
            equal_sign
            grave_accent_and_tilde
            hyphen
            left_arrow
            open_bracket
            period
            right_arrow
            semicolon
            slash
            spacebar
            tab
            up_arrow )

conditions() {
    printf ' "conditions": [{'
    printf '   "type": "frontmost_application_if",'
    printf '   "bundle_identifiers": ['
    printf '	  "^io\\\\.alacritty$"'
    printf '   ]'
    printf ' }]'
}

make_modifier_lazy() {
    local modifier="$1"
    printf '{'
    printf ' "type": "basic",\n'
    printf ' "from": { "key_code": "%s" },\n' "$modifier"
    printf ' "to": [{ "key_code": "%s", "lazy": true }],\n' "$modifier"
    conditions
    printf '}'
}

soft_meta_key() {
    local modifier="$1" key_code="$2"
    printf '{"type": "basic",\n'
    printf ' "from": {\n'
    printf '   "key_code": "%s",' "$key_code"
    printf '   "modifiers": {\n'
    printf '     "mandatory": [ "%s" ],\n' "$modifier"
    printf '     "optional": [ "shift" ]\n'
    printf '   }\n'
    printf ' },\n'
    printf ' "to": [\n'
    printf '   {"key_code": "escape", "modifiers": []},\n'
    printf '   {"key_code": "%s"}\n' "$key_code"
    printf ' ],\n'
    conditions
    printf '}\n'
}

dir="karabiner/config/karabiner.symlink/assets/complex_modifications"
mkdir -p "$dir"

(
    printf '{ "title": "Map Option to soft-meta for Alacritty", '
    printf '  "rules": ['
    comma=''
    for modifier in left_option right_option; do
        echo $comma
        comma=','
        printf ' {"description": "Map %s to soft-meta for Alacritty",' "$modifier"
        printf '  "manipulators": ['
        make_modifier_lazy "$modifier"
        for key_code in "${key_codes[@]}"; do
            printf ' ,\n'
            soft_meta_key "$modifier" "$key_code"
        done
        printf ' ]}'
    done
    printf '  ]'
    printf '}'
) |python -mjson.tool > "${dir}/alacritty.json"
