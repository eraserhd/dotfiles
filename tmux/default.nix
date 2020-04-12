{ lib, pkgs, config, options, ... }:

with lib;
let
  sessionName = config.local.systemDisplayName;
  shellPackage = config.local.loginShell.package;

  defaultCommand = if pkgs.stdenv.isDarwin
    then "exec ${pkgs.reattach-to-user-namespace}/bin/reattach-to-user-namespace -l ${shellPackage}${shellPackage.shellPath}"
    else "exec ${shellPackage}${shellPackage.shellPath}";

  tmuxConfig = ''
    #### Use C-a as prefix ####
    set-option -g prefix C-a
    unbind C-b
    bind C-a send-prefix
    bind a send-prefix

    set-option -g default-command '${defaultCommand}'
    set-option -g update-environment '''

    ##### Basic Usage #####


    ##### Scrollback Navigation #####


    # Reduce the command delay time to something a bit shorter
    set -sg escape-time 25
    setw -g xterm-keys on
    set -sa terminal-overrides ",xterm*:kLFT5=\eOD:kRIT5=\eOC:kUP5=\eOA:kDN5=\eOB:smkx@:rmkx@" # Shift+Arrow
    set -sa terminal-overrides ',xterm*:sitm=\E[3m' # italics

    ##### Window/Pane Management #####

    set -g status off

    # Support true color in Alacritty
    set -sa terminal-overrides ",xterm-256color:Tc"
    set -g default-terminal "tmux-256color"

    # Pane borders
    set -g pane-border-status top
    set -g pane-border-style "fg=#181a1f,bg=#292d3e"
    set -g pane-active-border-style "fg=#181a1f,bg=#292d3e"
    set -g pane-border-format "#[bg=#{?pane_active,#bfc7d5,#292d3e},fg=#292d3e]#[fg=#{?@ctrlw_active,#ff5370,#{?pane_active,#292d3e,#697098}}] #P #[fg=#{?pane_active,#bfc7d5,#292d3e},bg=#697098]#[fg=#{?pane_active,#bfc7d5,#292d3e}] #T #[bg=#292d3e,fg=#697098]"

    # Command line
    set -g message-style "fg=#bfc7d5,bg=#292d3e,bright"

    # Status bar
    set -g status-style "fg=white,bg=#292d3e"
    set -g status-position top
    set -g status-justify left
    set -g status-left-length 40
    set -g status-left "#[fg=#292d3e,bg=#697098] #S #[fg=#697098,bg=#292d3e] "
    set -g status-right "#[bg=#292d3e,fg=#697098] %d %b %R "

    # Window list
    setw -g window-status-format "#I #W"
    setw -g window-status-separator "#[fg=#697098]  "
    setw -g window-status-style "fg=#697098,bg=#292d3e"
    setw -g window-status-current-format "#I #W"
    setw -g window-status-current-style "fg=#697098,italics"
    setw -g window-status-activity-style "fg=#697098"

    # Monitor windows for activity
    setw -g monitor-activity on
    set -g visual-activity on

    #### Plugins ####

    run-shell ${pkgs.tmuxPlugins.sensible}/share/tmux-plugins/sensible/sensible.tmux
    run-shell ${pkgs.tmuxPlugins.ctrlw}/share/tmux-plugins/ctrlw/ctrlw.tmux
    run-shell ${pkgs.tmuxPlugins.plumb}/share/tmux-plugins/plumb/plumb.tmux

    # Is mode-keys-vi necessary?
    setw -g mode-keys vi
    set -g '@copy_mode_kakoune_after_yank' 'tmux show-buffer |${pkgs.plan9port-wrapper}/bin/9 9p write snarf/snarf'
    run-shell ${pkgs.tmuxPlugins.copy-mode-kakoune}/share/tmux-plugins/copy-mode-kakoune/copy-mode-kakoune.tmux

    ### Default Session ###

    new-session -c ~/src -n 'main' -s '${sessionName}'
    split-window -h -c ~/src
    split-window -h -c ~/src
    split-window -h -c ~/src
    select-layout -E
    split-window -v -c ~/src
    split-window -v -c ~/src
    select-layout -E
    send-keys -t %1 kak Enter
    send-keys -t %0 '${config.local.tmux.paneZeroCommand}' Enter
    send-keys -t %2 'sleep 2; kak' Enter
    send-keys -t %3 'sleep 2; kak -e "edit *debug*"' Enter
  '';
in
{
  options = {
    local.tmux.paneZeroCommand = mkOption {
      type = types.str;
      description = ''
        Command to run in leftmost pane on start.
      '';
      default = "sleep 2; kak";
    };
  };
  config = {
    programs.tmux = {
      enable = true;
      extraConfig = tmuxConfig;
    };
  };
}
