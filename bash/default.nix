{ config, lib, pkgs, ... }:

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

      initPlan9() {
          if ! command -v 9 >/dev/null 2>&1
          then
              printf 'no plan9 services.\n' >&2
              return 1
          fi

          if [[ -z $NAMESPACE ]];
          then
              export NAMESPACE=$XDG_RUNTIME_DIR/plan9/srv
          fi
          mkdir -p $NAMESPACE $XDG_RUNTIME_DIR/plan9/log
      }

      postService() {
          local srvname="$1"
          local logfile="$XDG_RUNTIME_DIR/plan9/log/$srvname"
          shift
          local ok=no
          if [[ -S $NAMESPACE/$srvname ]]
          then
              if 9 9p stat $srvname/. 1>/dev/null 2>&1
              then
                  ok=yes
              fi
          fi
          if [[ $ok = no ]]
          then
              rm -f $NAMESPACE/$srvname
              "$@" >"$logfile" 2>&1
          fi
      }

      startPlan9() {
          if [[ $(hostname) = crunch ]]; then
              postService plumb 9 plumber -p ~/.config/plan9/plumbing
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

      if [[ -z $XDG_RUNTIME_DIR ]]
      then
          export XDG_RUNTIME_DIR=~/.run
          mkdir -p ~/.run
      fi

      initPlan9

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
