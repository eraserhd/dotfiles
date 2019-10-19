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
            # Remove once 0.2.x merged to nixpkgs
            installPhase = ''
              mkdir -p $out/bin $out/share/kak/autoload/plugins/
              substitute rc/plumb.kak $out/share/kak/autoload/plugins/plumb.kak \
                --replace '9 plumb' '${self.plan9port}/bin/9 plumb'
              substitute edit-client $out/bin/edit-client \
                --replace '9 9p' '${self.plan9port}/bin/9 9p' \
                --replace 'kak -p' '${self.kakoune-unwrapped}/bin/kak -p'
              chmod +x $out/bin/edit-client
            '';
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

        tmux = super.tmux.overrideAttrs (old: {
          buildInputs = old.buildInputs ++ [ self.bison3 ];
          src = super.fetchFromGitHub (import ./tmux.nix);
        });

        tmuxPlugins = super.tmuxPlugins // {
          # Simplify once we get merged to nixpkgs
          plumb = super.tmuxPlugins.mkDerivation {
            pluginName = "plumb";
            postInstall = ''
              sed -i -e 's,9 plumb,${self.plan9port}/bin/9 plumb,' $target/scripts/plumb
            '';
            src = super.fetchFromGitHub (import ./tmux-plumb.nix);
          };
        };
      })
    ];
  };
}
