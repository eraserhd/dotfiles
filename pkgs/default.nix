self: super:

{
  kakoune-unwrapped = super.kakoune-unwrapped.overrideAttrs (oldAttrs: {
    src = super.pkgs.fetchFromGitHub (builtins.fromJSON (builtins.readFile ./kakoune-version.json));
  });
  kakoune = super.wrapKakoune self.kakoune-unwrapped {
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
}
