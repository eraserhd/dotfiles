{ config, lib, pkgs, ... }:

with lib;
{
  options = {
    local.systemDisplayName = mkOption {
      type = types.str;
      description = ''
        System name to display in prompts (can be different from hostname).
      '';
    };
  };

  config = {
    programs.bash.enableCompletion = true;
    programs.bash.interactiveShellInit = ''
      source_if_exists() {
        if [[ -f "$1" ]]
        then
          source "$1"
        fi
      }

      man() {
          if (( $# == 2 )); then
              kak -e "man $2($1)"
              return $?
          else
              kak -e "man $*"
              return $?
          fi
      }

      ssh() {
          if [[ "$TERM" = "tmux-256color" ]]; then
              TERM=screen command ssh "$@"
              return $?
          else
              command ssh "$@"
              return $?
          fi
      }

      source_if_exists ~/.nix-profile/etc/profile.d/nix.sh
      source ${toString ../bin/private.sh}

      if ! command -v __git_ps1 >/dev/null
      then
        __git_ps1() { :; }
      fi

      bashPromptCommand() {
        local exitCode=$? exitColor='\[\e[1;32m\]'
        if (( exitCode != 0 )); then
           exitColor='\[\e[1;31m\]'
        fi
        PS1='\n'"$exitColor"'\t \u@${config.local.systemDisplayName} \W $(__git_ps1 "(%s)")\n\$\[\e[0m\] '
      }
      export PROMPT_COMMAND=bashPromptCommand

      eval "$(${pkgs.direnv}/bin/direnv hook bash)"
    '';

    nixpkgs.overlays = [
      (import ./add-missing/overlay.nix)
    ];

    environment.systemPackages = with pkgs; [
      add-missing
      ag
      direnv
      file
      jq
      killall
      posix_man_pages
      wget
    ];

    environment.variables = {
      CLICOLOR = "1";
      GIT_PS1_SHOWDIRTYSTATE = "1";
      GIT_PS1_SHOWUNTRACKEDFILES = "1";
      GIT_PS1_SHOWUPSTREAM = "auto";
    };
  };
}
