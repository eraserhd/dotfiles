{ pkgs, lib, ... }:

let
  dogfood = super: file: overrides: let
    source = super.fetchFromGitHub (import file);
  in
    super.callPackage "${source}/derivation.nix" ({ fetchFromGitHub = _: source; } // overrides);
in
{
  config = {
    nixpkgs.overlays = [
      (self: super: rec {
        kakoune-unwrapped = super.kakoune-unwrapped.overrideAttrs (oldAttrs: {
          src = super.pkgs.fetchFromGitHub (import ./kakoune.nix);
          enableParallelBuilding = true;
        });

        kakounePlugins = super.kakounePlugins // {
          kak-fzf = super.kakounePlugins.kak-fzf.overrideAttrs (oldAttrs: {
            src = super.pkgs.fetchFromGitHub (import ./fzf.kak.nix);
          });
          kak-jira = dogfood super ./kak-jira.nix {};
        };

        # Fixes bug with nats request --raw
        natscli = super.callPackage ./natscli.nix {};

        parinfer-rust = dogfood super ./parinfer-rust.nix {};
        rep = dogfood super ./rep.nix {};

        sqltools = dogfood super ./sqltools.nix {};

        tableize = dogfood super ./tableize.nix {};
      })
    ];
  };
}
