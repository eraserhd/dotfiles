{ config, pkgs, ... }:

{
  config = {
    nixpkgs.overlays = [
      (self: super: {
        weechat = (super.weechat.override {
          configure = {availablePlugins, ...}: {
            scripts = with self.weechatScripts; [ wee-slack ];
            plugins = with availablePlugins; [ python ];
          };
        });
      })
    ];
    environment.systemPackages = with pkgs; [
      weechat
    ];
  };
}
