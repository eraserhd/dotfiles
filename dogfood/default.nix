{ pkgs, ... }:

{
  config = {
    nixpkgs.overlays = [
      (self: super: {
        kakoune-unwrapped = super.kakoune-unwrapped.overrideAttrs (oldAttrs: {
          src = super.pkgs.fetchFromGitHub (import ./kakoune.nix);
        });

        kakounePlugins = super.kakounePlugins // {
          kak-plumb = super.kakounePlugins.kak-plumb.overrideAttrs (oldAttrs: rec {
            src = super.pkgs.fetchFromGitHub (import ./kak-plumb.nix);
          });
        };

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

        tmuxPlugins = super.tmuxPlugins // {
          plumb = pkgs.fetchFromGitHub (import ./tmux-plumb.nix);
        };
      })
    ];
  };
}
