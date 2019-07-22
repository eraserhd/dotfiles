import (fetchGit {
  url = "git@github.com:eraserhd/nixpkgs.git";
  ref = "eraserhd";
  rev = "c05c72b0041165b60fa5df5d4f50c251dd4bcb86";
})
{
  overlays = [ (self: super: { 
    kakoune = super.kakoune.override {
      configure = {
        plugins = with self.kakounePlugins; [
          kak-ansi
          parinfer-rust
        ];
      };
    };
    kakouneWrapper = super.callPackage ./kakoune-wrapper {};
    my-packages = super.callPackage ./my-packages.nix {};
    weechat = (super.weechat.override {
      configure = {availablePlugins, ...}: {
        scripts = with self.weechatScripts; [ wee-slack ];
        plugins = with availablePlugins; [ python ];
      };
    });
  }) ];
}
