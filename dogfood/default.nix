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

        # For new module system.  Remove when >4.9.3 is released
        gambit = super.callPackage ../nixpkgs/pkgs/development/compilers/gambit/build.nix rec {
          version = (import ./gambit.nix).rev;
          git-version = version;
          src = super.pkgs.fetchFromGitHub (import ./gambit.nix);
        };

        gerbilPackages = {
          clojerbil = dogfood super ./clojerbil.nix {};
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

        parinfer-rust = super.parinfer-rust.overrideAttrs (oldAttrs: {
          src = super.pkgs.fetchFromGitHub (import ./parinfer-rust.nix);
        });

        rep = dogfood super ./rep.nix {};

        yabai = super.yabai.overrideAttrs (oldAttrs: {
          src = super.pkgs.fetchFromGitHub (import ./yabai.nix);
        });
      })
    ];
  };
}
