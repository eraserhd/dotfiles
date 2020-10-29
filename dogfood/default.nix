{ pkgs, ... }:

let
  dogfoodFromGitHub = super: file: overrides: let
    source = super.fetchFromGitHub (import file);
  in
    super.callPackage "${source}/derivation.nix" ({ fetchFromGitHub = _: source; } // overrides);
in
{
  config = {
    nixpkgs.overlays = [
      (self: super: let
        gerbilPackages = {
          clojerbil = self.callPackage "${super.fetchFromGitHub (import ./clojerbil.nix)}/derivation.nix" {};
        };
      in {
        gerbil = super.gerbil.overrideAttrs (oldAttrs: {
          src = super.pkgs.fetchFromGitHub (import ./gerbil.nix);
        });

        add-missing = dogfoodFromGitHub super ./add-missing.nix {};

        # For new module system.  Remove when >4.9.3 is released
        gambit = super.callPackage ../nixpkgs/pkgs/development/compilers/gambit/build.nix rec {
          version = (import ./gambit.nix).rev;
          git-version = version;
          src = super.pkgs.fetchFromGitHub (import ./gambit.nix);
        };

        inherit gerbilPackages;

        gitAndTools = super.gitAndTools // {
          gitout = super.callPackage "${super.fetchFromGitHub (import ./gitout.nix)}/derivation.nix" {
            inherit gerbilPackages;
          };
        };

        kakoune-unwrapped = super.kakoune-unwrapped.overrideAttrs (oldAttrs: {
          src = super.pkgs.fetchFromGitHub (import ./kakoune.nix);
          enableParallelBuilding = true;
        });

        kakounePlugins = super.kakounePlugins // {
          kak-ansi = dogfoodFromGitHub super ./kak-ansi.nix {};
          kak-fzf = super.kakounePlugins.kak-fzf.overrideAttrs (oldAttrs: {
            src = super.pkgs.fetchFromGitHub (import ./fzf.kak.nix);
          });
          kak-jira = dogfoodFromGitHub super ./kak-jira.nix {};
          kak-plumb = dogfoodFromGitHub super ./kak-plumb.nix {
            plan9port = pkgs.plan9port-wrapper;
          };
          quickscope-kak = super.callPackage ../kakoune/quickscope-kak.nix {};
        };

        parinfer-rust = super.parinfer-rust.overrideAttrs (oldAttrs: {
          src = super.pkgs.fetchFromGitHub (import ./parinfer-rust.nix);
        });

        rep = dogfoodFromGitHub super ./rep.nix {};

        yabai = super.yabai.overrideAttrs (oldAttrs: {
          src = super.pkgs.fetchFromGitHub (import ./yabai.nix);
        });
      })
    ];
  };
}
