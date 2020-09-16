{ pkgs, ... }:

{
  config = {
    nixpkgs.overlays = [
      (self: super: let
         rep = self.callPackage "${super.fetchFromGitHub (import ./rep.nix)}/derivation.nix" {};

         gerbilPackages = {
          clojerbil = self.callPackage "${super.fetchFromGitHub (import ./clojerbil.nix)}/derivation.nix" {};
         };
       in {
        gerbil = super.gerbil.overrideAttrs (oldAttrs: {
          src = super.pkgs.fetchFromGitHub (import ./gerbil.nix);
        });

        add-missing = self.callPackage "${super.fetchFromGitHub (import ./add-missing.nix)}/derivation.nix" {};

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

        kakoune = super.kakoune.overrideAttrs (oldAttrs: {
          src = super.pkgs.fetchFromGitHub (import ./kakoune.nix);
          enableParallelBuilding = true;
        });

        kakounePlugins = super.kakounePlugins // {
          kak-ansi = super.kakounePlugins.kak-ansi.overrideAttrs (oldAttrs: {
            src = super.pkgs.fetchFromGitHub (import ./kak-ansi.nix);
          });
          kak-fzf = super.kakounePlugins.kak-fzf.overrideAttrs (oldAttrs: {
            src = super.pkgs.fetchFromGitHub (import ./fzf.kak.nix);
          });
          kak-plumb = super.callPackage "${super.fetchFromGitHub (import ./kak-plumb.nix)}/derivation.nix" {
            plan9port = pkgs.plan9port-wrapper;
          };
          rep = rep;
        };

        # With patch that supports $WINDOWID
        kitty = super.kitty.overridePythonAttrs (oldAttrs: {
          src = super.pkgs.fetchFromGitHub (import ./kitty.nix);
          doCheck = false;
        });

        parinfer-rust = super.parinfer-rust.overrideAttrs (oldAttrs: {
          src = super.pkgs.fetchFromGitHub (import ./parinfer-rust.nix);
        });

        rep = rep;

        yabai = super.yabai.overrideAttrs (oldAttrs: {
          src = super.pkgs.fetchFromGitHub (import ./yabai.nix);
        });
      })
    ];
  };
}
