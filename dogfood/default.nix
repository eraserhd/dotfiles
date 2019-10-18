{ pkgs, ... }:

{
  config = {
    nixpkgs.overlays = [
      (self: super: {
        tmuxPlugins = super.tmuxPlugins // {
          plumb = pkgs.fetchFromGitHub (import ./tmux-plumb.nix);
        };
      })
    ];
  };
}
