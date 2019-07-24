{ ... }:

{
  config = {
    programs.tmux.enable = true;
    programs.tmux.tmuxConfig = ''
      #### Use C-a as prefix ####
      set-option -g prefix C-a
      unbind C-b
      bind C-a send-prefix
      bind a send-prefix

      if-shell 'command -v reattach-to-user-namespace' 'set-option -g default-command "reattach-to-user-namespace -l bash"'
      set-option -g update-environment ${"''"}

      ##### Basic Usage #####

      # Reduce the command delay time to something a bit shorter
      set -sg escape-time 1
      # Extend the repeat interval for repeatable commands (e.g., resize-pane)
      set -sg repeat-time 1000

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

      # Split windows more intuitively (except for the fact that tmux doesn't
      # understand that a horizontal split means the pane should be split down the
      # middle horizontally, and likewise for a vertical split).
      bind | split-window -h -c '#{pane_current_path}' # horizontal columns
      bind - split-window -v -c '#{pane_current_path}' # vertical rows

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

      set -g @plugin 'tmux-plugins/tmux-sensible'
      set -g @plugin 'eraserhd/tmux-ctrlw'
      set -g @plugin 'eraserhd/tmux-editor-copy-mode'

      run -b '~/.tmux/plugins/tpm/tpm'
    '';
  };
}
