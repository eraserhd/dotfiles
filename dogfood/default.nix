{ pkgs, ... }:

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
        gerbil = super.gerbil.overrideAttrs (oldAttrs: {
          src = super.pkgs.fetchFromGitHub (import ./gerbil.nix);
        });

        add-missing = dogfood super ./add-missing.nix {};

        gerbilPackages = {
          clojerbil = dogfood super ./clojerbil.nix {};
          gerbil-bencode = dogfood super ./gerbil-bencode.nix {};
        };

        gitAndTools = super.gitAndTools // {
          gitout = dogfood super ./gitout.nix { inherit gerbilPackages; };
        };

        kakoune-unwrapped = super.kakoune-unwrapped.overrideAttrs (oldAttrs: {
          src = super.pkgs.fetchFromGitHub (import ./kakoune.nix);
          enableParallelBuilding = true;
        });

        kakounePlugins = super.kakounePlugins // {
          kak-ansi = dogfood super ./kak-ansi.nix {};
          kak-fzf = super.kakounePlugins.kak-fzf.overrideAttrs (oldAttrs: {
            src = super.pkgs.fetchFromGitHub (import ./fzf.kak.nix);
          });
          kak-jira = dogfood super ./kak-jira.nix {};
          kak-plumb = dogfood super ./kak-plumb.nix {
            plan9port = pkgs.plan9port-wrapper;
          };
        };

        # Use old plan9port for Darwin, because plan9port removed support
        # for Mac OS 10.12, and Nixpkgs requires it, so the package is marked
        # broken.
        plan9port = super.callPackage ./plan9port {};

        parinfer-rust = dogfood super ./parinfer-rust.nix {};
        rep = dogfood super ./rep.nix {};
      })
    ];
  };
}
