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
    programs.bash.interactiveShellInit = ''
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

      source ${toString ../bin/private.sh}

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
      rlwrap
      wget
    ];

    environment.variables = {
      GIT_PS1_SHOWDIRTYSTATE = "1";
      GIT_PS1_SHOWUNTRACKEDFILES = "1";
      GIT_PS1_SHOWUPSTREAM = "auto";
    };
  };
}
