{ pkgs, ... }:

{
  config = {
    nixpkgs.overlays = [
      (self: super: {
        kakoune-unwrapped = super.kakoune-unwrapped.overrideAttrs (oldAttrs: {
          src = super.pkgs.fetchFromGitHub (import ./kakoune.nix);
        });
        tmuxPlugins = super.tmuxPlugins // {
          plumb = pkgs.fetchFromGitHub (import ./tmux-plumb.nix);
        };
      })
    ];
  };
}
