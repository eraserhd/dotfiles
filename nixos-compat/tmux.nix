{ lib, options, ... }:

with lib;
{
  options.programs.tmux.extraConfig = options.programs.tmux.tmuxConfig;
}
