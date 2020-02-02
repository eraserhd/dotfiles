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
