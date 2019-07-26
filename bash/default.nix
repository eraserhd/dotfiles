{ ... }:

{
  config = {
    programs.bash.interactiveShellInit = ''
      :r() {
        if command -v darwin-rebuild >/dev/null; then
          pushd ~/src/dotfiles >/dev/null
          darwin-rebuild build || return $?
          if [[ $(readlink /run/current-system) != $(readlink ./result) ]]; then
            darwin-rebuild switch || return $?
          fi
          popd >/dev/null
        fi
        unset __NIX_DARWIN_SET_ENVIRONMENT_DONE __ETC_BASHRC_SOURCED
        exec $SHELL -l
      }

      source ${toString ../bin/private.sh}
    '';
  };
}
