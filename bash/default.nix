{ config, lib, ... }:

with lib;
{
  options = {
    local.systemDisplayName = mkOption {
      type = types.string;
      description = ''
        System name to display in prompts (can be different from hostname).
      '';
    };
  };

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

      export PS1='\[\e[1;32m\][\u@${config.local.systemDisplayName} \W$(__git_ps1 "(%s)")]\$\[\e[0m\] '
    '';
  };
}
