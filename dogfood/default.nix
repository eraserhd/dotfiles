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

        add-missing = self.callPackage ("${super.fetchFromGitHub (import ./add-missing.nix)}/derivation.nix") {};

        kakoune-unwrapped = super.kakoune-unwrapped.overrideAttrs (oldAttrs: {
          src = super.pkgs.fetchFromGitHub (import ./kakoune.nix);
          enableParallelBuilding = true;
        });

        kakounePlugins = super.kakounePlugins // {
          kak-ansi = super.kakounePlugins.kak-ansi.overrideAttrs (oldAttrs: {
            src = super.pkgs.fetchFromGitHub (import ./kak-ansi.nix);
          });
          kak-plumb = super.kakounePlugins.kak-plumb.overrideAttrs (oldAttrs: {
            src = super.pkgs.fetchFromGitHub (import ./kak-plumb.nix);
          });
          kak-proof = super.callPackage ("${super.fetchFromGitHub (import ./kak-proof.nix)}/derivation.nix") {};
          rep = rep;
        };

        lilypond = super.lilypond.overrideAttrs (oldAttrs: {
          meta = builtins.removeAttrs oldAttrs.meta [ "broken" ];
        });

        parinfer-rust = super.parinfer-rust.overrideAttrs (oldAttrs: {
          src = pkgs.fetchFromGitHub (import ./parinfer-rust.nix);
        });

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
