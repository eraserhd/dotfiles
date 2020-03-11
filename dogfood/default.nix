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

        elvish = super.buildGoModule rec {
          pname = "elvish";
          version = "0.13";

          goPackagePath = "github.com/elves/elvish";
          excludedPackages = [ "website" ];
          buildFlagsArray = ''
            -ldflags=
              -X ${goPackagePath}/buildinfo.Version=${version}
          '';

          src = super.fetchFromGitHub (import ./elvish.nix);

          modSha256 = "00njpsmhga3fl56qg3qpjksfxfbsk435w750qybsby2i51mkn09x";

          meta = with super.stdenv.lib; {
            description = "A friendly and expressive command shell";
            longDescription = ''
              Elvish is a friendly interactive shell and an expressive programming
              language. It runs on Linux, BSDs, macOS and Windows. Despite its pre-1.0
              status, it is already suitable for most daily interactive use.
            '';
            homepage = https://elv.sh/;
            license = licenses.bsd2;
            maintainers = with maintainers; [ vrthra AndersonTorres ];
            platforms = with platforms; linux ++ darwin;
          };

          passthru = {
            shellPath = "/bin/elvish";
          };
        };

        gitAndTools = super.gitAndTools // {
          git-browse-link = super.callPackage ("${super.fetchFromGitHub (import ./git-browse-link.nix)}/derivation.nix") {};
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
          kak-plumb = super.kakounePlugins.kak-plumb.overrideAttrs (oldAttrs: {
            src = super.pkgs.fetchFromGitHub (import ./kak-plumb.nix);
          });
          kak-proof = super.callPackage ("${super.fetchFromGitHub (import ./kak-proof.nix)}/derivation.nix") {};
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
