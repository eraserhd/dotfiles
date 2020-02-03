{ config, pkgs, ... }:

let
  shellPackage = config.local.loginShell.package;
in {
  config = {
    local.loginShell.package = pkgs.zsh;
    programs.zsh.enable = true;
    programs.zsh.enableCompletion = true;
    programs.zsh.interactiveShellInit = ''
      setopt auto_cd
      cdpath=(~/src)

      eval "$(${pkgs.direnv}/bin/direnv hook zsh)"
    '';
    programs.zsh.promptInit = ''
      export PROMPT='
      %(?.%F{green}.%F{red}[%?] )%n@%m %1d
      $%f '
    '';
  };
}
