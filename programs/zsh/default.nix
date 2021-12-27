{ pkgs, ... }:

{
  config = {
    local.loginShell.package = pkgs.zsh;
    programs.zsh.enable = true;
    programs.zsh.enableCompletion = true;
    programs.zsh.interactiveShellInit = ''
      autoload -U edit-command-line
      zle -N edit-command-line
      bindkey '^xe' edit-command-line
      bindkey '^x^e' edit-command-line
      bindkey '\ef' emacs-forward-word
      bindkey '\eb' emacs-backward-word

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
      %(?.%F{green}.%F{red})%* %? %n@%m %1d ''${vcs_info_msg_0_}
      ᐅ%f '
      RPS1=""
    '';
    environment.shells = [ pkgs.zsh ];

    home-manager.users.jfelice = { pkgs, ... }: {
      home.file.".zshrc".text = ''
        # empty file to prevent zsh from trying to initialize
      '';
    };
  };
}
