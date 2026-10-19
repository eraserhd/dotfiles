{ pkgs, ... }:

{
  config = {
    local.loginShell.package = pkgs.zsh;
    environment.shells = [ pkgs.zsh ];

    # The system keeps /etc/zshenv (the environment) and completion, but ~/.zshrc
    # from home-manager does the rest, so nothing is set up for the prompt here.
    programs.zsh.enable = true;
    programs.zsh.promptInit = "";

    home-manager.users.jfelice = { pkgs, ... }: {
      programs.zsh = {
        enable = true;
        # /etc/zshrc has already run compinit.
        enableCompletion = false;
        initContent = ''
          autoload -U edit-command-line
          zle -N edit-command-line
          bindkey '^xe' edit-command-line
          bindkey '^x^e' edit-command-line
          bindkey '\ef' emacs-forward-word
          bindkey '\eb' emacs-backward-word

          setopt auto_cd
          cdpath=(~/src ~/src/*.*/*(N/))

          setopt nonomatch

          eval "$(${pkgs.direnv}/bin/direnv hook zsh)"

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
      };
    };
  };
}
