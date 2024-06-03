{ lib, config, pkgs, ... }:

with lib;
{
  config = {
    nixpkgs.overlays = [
      (self: super: {
        weechat = (super.weechat.override {
          configure = {availablePlugins, ...}: {
            scripts = with self.weechatScripts; [
              edit
              url_hint
              wee-slack
              weechat-autosort
            ];
            plugins = with availablePlugins; [ python ];
          };
        });
      })
    ];

    environment.systemPackages = with pkgs; [
      weechat
    ];

    home-manager.users.jfelice = { pkgs, ... }: {
      home.file.".weechat/alias.conf".source = ./config/alias.conf;
      home.file.".weechat/aspell.conf".source = ./config/aspell.conf;
      home.file.".weechat/buflist.conf".source = ./config/buflist.conf;
      home.file.".weechat/charset.conf".source = ./config/charset.conf;
      home.file.".weechat/exec.conf".source = ./config/exec.conf;
      home.file.".weechat/fifo.conf".source = ./config/fifo.conf;
      home.file.".weechat/fset.conf".source = ./config/fset.conf;
      home.file.".weechat/irc.conf".source = ./config/irc.conf;
      home.file.".weechat/logger.conf".source = ./config/logger.conf;
      home.file.".weechat/plugins.conf".source = ./config/plugins.conf;
      home.file.".weechat/python.conf".source = ./config/python.conf;
      home.file.".weechat/relay.conf".source = ./config/relay.conf;
      home.file.".weechat/script.conf".source = ./config/script.conf;
      home.file.".weechat/sec.conf".source = ./config/sec.conf;
      home.file.".weechat/spell.conf".source = ./config/spell.conf;
      home.file.".weechat/tcl.conf".source = ./config/tcl.conf;
      home.file.".weechat/trigger.conf".source = ./config/trigger.conf;
      home.file.".weechat/weechat.conf".source = ./config/weechat.conf;
      home.file.".weechat/weemoji.json".source = ./config/weemoji.json;
      home.file.".weechat/xfer.conf".source = ./config/xfer.conf;
    };
  };
}
