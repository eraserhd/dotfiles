import (fetchGit {
  url = "git@github.com:eraserhd/nixpkgs.git";
  ref = "eraserhd";
  rev = "c05c72b0041165b60fa5df5d4f50c251dd4bcb86";
})
{
  config = {
    packageOverrides = pkgs: rec {
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
