{ pkgs, config, options, ... }:

let
  sessionName = config.local.systemDisplayName;

  tmuxConfig = ''
    #### Use C-a as prefix ####
    set-option -g prefix C-a
    unbind C-b
    bind C-a send-prefix
    bind a send-prefix

    if-shell 'command -v reattach-to-user-namespace' 'set-option -g default-command "reattach-to-user-namespace -l bash"'
    set-option -g update-environment '''

    ##### Basic Usage #####

    # Reduce the command delay time to something a bit shorter
    set -sg escape-time 25

    ##### Scrollback Navigation #####

    # Use vi-style navigation in Copy mode (which is also scrollback mode)
    setw -g mode-keys vi
    bind -Tcopy-mode-vi Escape send-keys -X cancel
    bind -Tcopy-mode-vi i send-keys -X cancel
    bind -Tcopy-mode-vi v send-keys -X begin-selection
    bind -Tcopy-mode-vi y send-keys -X copy-selection-and-cancel

    setw -g xterm-keys on
    set -sa terminal-overrides ",xterm*:kLFT5=\eOD:kRIT5=\eOC:kUP5=\eOA:kDN5=\eOB:smkx@:rmkx@" # Shift+Arrow
    set -sa terminal-overrides ',xterm*:sitm=\E[3m' # italics

    ##### Window/Pane Management #####

    # Support true color in Alacritty
    set -sa terminal-overrides ",xterm-256color:Tc"
    set -g default-terminal "tmux-256color"

    # Pane borders
    set -g pane-border-status bottom
    set -g pane-border-style "fg=#181a1f,bg=#292d3e"
    set -g pane-active-border-style "fg=#181a1f,bg=#292d3e"
    set -g pane-border-format "#[bg=#{?pane_active,#bfc7d5,#292d3e},fg=#292d3e]#[fg=#{?pane_active,#292d3e,#697098}] #P #[fg=#{?pane_active,#bfc7d5,#292d3e},bg=#697098]#[fg=#{?pane_active,#bfc7d5,#292d3e}] #T #[bg=#292d3e,fg=#697098]"

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

    ### Default Session ###

    new-session -c ~/src -n 'main' -s '${sessionName}'
    split-window -h -l 25% -c ~/src
    split-window -h -l 25% -c ~/src
    split-window -h -l 25% -c ~/src
    split-window -v -l 33% -c ~/src
    split-window -v -l 33% -c ~/src
    send-keys -t %1 kak Enter
    send-keys -t %0 weechat Enter
    run-shell 'sleep 2'
    send-keys -t %2 kak Enter
    send-keys -t %3 'kak -e "edit *debug*"' Enter
  '';
in
{
  config = {
    nixpkgs.overlays = [
      (self: super: {
        tmux = super.tmux.overrideAttrs (old: {
          buildInputs = old.buildInputs ++ [ self.bison3 ];
          src = super.fetchFromGitHub {
            owner = "tmux";
            repo = "tmux";
            rev = "fb7ce5b5d509db244111e130224f366ced28b228";
            sha256 = "0h4m3qsbnw1xl3vf3n08fjfm5m979fh1l9nbl44rr64k8ks0nfnr";
          };
        });
      })
    ];

    programs.tmux = {
      enable = true;
    }
    // (if (builtins.hasAttr "tmuxConfig" options.programs.tmux) then {
      tmuxConfig = tmuxConfig;
    } else {
      extraTmuxConf = tmuxConfig;
    });
  };
}
