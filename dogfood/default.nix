{ pkgs, ... }:

{
  config = {
    nixpkgs.overlays = [
      (self: super: {
        # fop doesn't like my jdk11 override
        fop = super.fop.override {
          jdk = self.jdk8;
        };

        kakoune-unwrapped = super.kakoune-unwrapped.overrideAttrs (oldAttrs: {
          src = super.pkgs.fetchFromGitHub (import ./kakoune.nix);
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

        rep = self.callPackage ("${super.fetchFromGitHub {
          owner = "eraserhd";
          repo = "rep";
          rev = "1951df780fdd2781644f934dfc36ee394460effb";
          sha256 = "0x7872ia59m6wxksv8c5b41yz2crl10ikgapk2m2q91gkh8fagr4";
        }}/derivation.nix") {};

        tmux = super.tmux.overrideAttrs (old: {
          buildInputs = old.buildInputs ++ [ self.bison3 ];
          src = super.fetchFromGitHub (import ./tmux.nix);
        });

        tmuxPlugins = super.tmuxPlugins // {
          copy-mode-kakoune = super.tmuxPlugins.mkDerivation {
            pluginName = "copy-mode-kakoune";
            src = super.fetchFromGitHub (import ./tmux-copy-mode-kakoune.nix);
          };

          ctrlw = super.tmuxPlugins.ctrlw.overrideAttrs (oldAttrs: {
            src = super.fetchFromGitHub (import ./tmux-ctrlw.nix);
          });
        };
      })
    ];
  };
}
