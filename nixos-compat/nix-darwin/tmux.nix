{ lib, config, options, ... }:

with lib;
let
  cfg = config.programs.tmux;
in {
  options.programs.tmux.extraConfig = options.programs.tmux.tmuxConfig;
  config = {
    programs.tmux.tmuxConfig = cfg.extraConfig;
  };
}
