import (fetchGit {
  url = "git@github.com:eraserhd/nixpkgs.git";
  ref = "eraserhd";
  rev = "665e7de81ed378b5420fc2005017268df7290c7d";
})
{
  config = {
    packageOverrides = pkgs: rec {
      kakoune-unwrapped = let ver = builtins.fromJSON (builtins.readFile ./kakoune-version.json);
      in pkgs.kakoune-unwrapped.overrideAttrs (old: rec {
        version = ver.rev;
        src = pkgs.fetchFromGitHub ver;
        patches = [ ./kakoune/revert-uncaught-exceptions.patch ];
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
