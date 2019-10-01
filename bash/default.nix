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

      :r() {
        if command -v darwin-rebuild >/dev/null; then
          pushd ~/src/dotfiles >/dev/null
          darwin-rebuild build || return $?
          if [[ $(readlink /run/current-system) != $(readlink ./result) ]]; then
            darwin-rebuild switch || return $?
          fi
          popd >/dev/null
        elif command -v nixos-rebuild >/dev/null; then
          pushd ~/src/dotfiles >/dev/null
          nixos-rebuild build || return $?
          if [[ $(readlink /run/current-system) != $(readlink ./result) ]]; then
            sudo nixos-rebuild switch || return $?
          fi
          popd >/dev/null
        fi
        unset __NIX_DARWIN_SET_ENVIRONMENT_DONE __NIXOS_SET_ENVIRONMENT_DONE
        unset __ETC_BASHRC_SOURCED __ETC_PROFILE_SOURCED __ETC_PROFILE_DONE
        exec $SHELL -l
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
