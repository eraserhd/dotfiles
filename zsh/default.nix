{ pkgs, ... }:

{
  config = {
    local.loginShell.package = pkgs.zsh;
    programs.zsh.enable = true;
    programs.zsh.enableCompletion = true;
    programs.zsh.interactiveShellInit = ''
      setopt auto_cd
      cdpath=(~/src)

      setopt nonomatch

      eval "$(${pkgs.direnv}/bin/direnv hook zsh)"
    '';
    programs.zsh.promptInit = ''
      autoload -Uz vcs_info
      precmd() {
        vcs_info
      }
      zstyle ':vcs_info:*' formats '(%b)'
      zstyle ':vcs_info:*' actionformats '(%b[%a])'
      setopt prompt_subst
      PROMPT='
      %(?.%F{green}.%F{red}[%?] )%n@%m %1d ''${vcs_info_msg_0_}
      $%f '
      RPS1=""
    '';
    environment.shells = [ pkgs.zsh ];
  };
}
