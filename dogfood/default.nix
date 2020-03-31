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
        # fop doesn't like my jdk11 override
        fop = (super.fop.override {
          jdk = self.jdk8;
        });

        add-missing = self.callPackage "${super.fetchFromGitHub (import ./add-missing.nix)}/derivation.nix" {};

        gerbil = super.gerbil.overrideAttrs (oldAttrs: {
          src = super.pkgs.fetchFromGitHub (import ./gerbil.nix);
        });

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
          case-kak = super.callPackage ../kakoune/case.kak.nix {};
          kak-ansi = super.kakounePlugins.kak-ansi.overrideAttrs (oldAttrs: {
            src = super.pkgs.fetchFromGitHub (import ./kak-ansi.nix);
          });
          kak-fzf = super.kakounePlugins.kak-fzf.overrideAttrs (oldAttrs: {
            src = super.pkgs.fetchFromGitHub (import ./fzf.kak.nix);
          });
          kak-plumb = super.kakounePlugins.kak-plumb.overrideAttrs (oldAttrs: {
            src = super.pkgs.fetchFromGitHub (import ./kak-plumb.nix);
          });
          rep = rep;
        };

        rep = rep;

        tmux = super.tmux.overrideAttrs (old: {
          buildInputs = old.buildInputs ++ [ self.bison ];
          src = super.fetchFromGitHub (import ./tmux.nix);
        });

        tmuxPlugins = super.tmuxPlugins // {
          copy-mode-kakoune = super.tmuxPlugins.mkDerivation {
            pluginName = "copy-mode-kakoune";
            src = super.fetchFromGitHub (import ./tmux-copy-mode-kakoune.nix);
          };
        };
      })
    ];
  };
}
