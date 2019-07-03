import (fetchGit {
  url = "git@github.com:eraserhd/nixpkgs.git";
  ref = "eraserhd";
  rev = "a8e16f35a3dfaa4724542fd643fbafb18c1f6dbf";
})
{
  config = {
    packageOverrides = pkgs: rec {
      kakoune-unwrapped = let ver = builtins.fromJSON (builtins.readFile ./kakoune-version.json);
      in pkgs.kakoune-unwrapped.overrideAttrs (old: rec {
        version = ver.rev;
        src = pkgs.fetchFromGitHub ver;
      });
      kakoune = pkgs.kakoune.override {
        configure = {
          plugins = with pkgs.kakounePlugins; [
            kak-ansi
            parinfer-rust
          ];
        };
      };
      kakouneWrapper = pkgs.callPackage ./kakoune-wrapper {};
      my-packages = pkgs.callPackage ./my-packages.nix {};
      weechat = (pkgs.weechat.override {
        configure = {availablePlugins, ...}: {
          scripts = with pkgs.weechatScripts; [ wee-slack ];
          plugins = with availablePlugins; [ python ];
        };
      });
    };
  };
}
