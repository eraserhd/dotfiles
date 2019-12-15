{ pkgs, ... }:

{
  config = {
    nixpkgs.overlays = [
      (self: super: let
         rep = self.callPackage ("${super.fetchFromGitHub (import ./rep.nix)}/derivation.nix") {};
       in {
        # fop doesn't like my jdk11 override
        fop = (super.fop.override {
          jdk = self.jdk8;
        });

        kakoune-unwrapped = super.kakoune-unwrapped.overrideAttrs (oldAttrs: {
          src = super.pkgs.fetchFromGitHub (import ./kakoune.nix);
        });

        kakounePlugins = super.kakounePlugins // {
          kak-ansi = super.kakounePlugins.kak-ansi.overrideAttrs (oldAttrs: {
            src = super.pkgs.fetchFromGitHub (import ./kak-ansi.nix);
          });
          kak-plumb = super.kakounePlugins.kak-plumb.overrideAttrs (oldAttrs: {
            src = super.pkgs.fetchFromGitHub (import ./kak-plumb.nix);
          });
          rep = rep;
        };

        lilypond = super.lilypond.overrideAttrs (oldAttrs: {
          meta = builtins.removeAttrs oldAttrs.meta [ "broken" ];
        });

        parinfer-rust = super.parinfer-rust.overrideAttrs (oldAttrs: rec {
          # Remove once we get 0.4.x merged to nixpkgs
          buildInputs = [ self.llvmPackages.libclang self.llvmPackages.clang ];
          LIBCLANG_PATH = "${self.llvmPackages.libclang}/lib";
          src = super.pkgs.fetchFromGitHub (import ./parinfer-rust.nix);
          cargoDeps = oldAttrs.cargoDeps.overrideAttrs (_: {
            inherit src;
            outputHash = "0i5wy15w985nxwl4b6rzb06hchzjwph6ygzjkkmigm9diw9jcycn";
          });
        });

        rep = rep;

        tmux = super.tmux.overrideAttrs (old: {
          buildInputs = old.buildInputs ++ [ self.bison3 ];
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
