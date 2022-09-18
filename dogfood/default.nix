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
          kak-plumb = dogfood super ./kak-plumb.nix {
            plan9port = self.nats-plumber;
          };
        };

	# Fixes bug with nats request --raw
        natscli = super.callPackage ./natscli.nix {};

        nats-plumber = dogfood super ./nats-plumber.nix {};

        # lxml / beautifulsoup4 fails to build on Darwin
        # https://github.com/NixOS/nixpkgs/issues/137678
        python39 = super.python39.override {
          packageOverrides = self: super: {
            beautifulsoup4 = super.beautifulsoup4.overrideAttrs (old: {
              propagatedBuildInputs = lib.remove super.lxml old.propagatedBuildInputs;
            });
          };
        };
        python39Packages = python39.pkgs;

        parinfer-rust = dogfood super ./parinfer-rust.nix {};
        rep = dogfood super ./rep.nix {};

        sqltools = dogfood super ./sqltools.nix {};

        tableize = dogfood super ./tableize.nix {};
      })
    ];
  };
}
