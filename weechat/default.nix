{ lib, config, pkgs, ... }:

with lib;
let
  edit-weechat = pkgs.stdenv.mkDerivation {
    name = "edit-weechat";
    src = pkgs.fetchFromGitHub {
      owner = "keith";
      repo = "edit-weechat";
      rev = "c1f6966d32f8c54a480fa60b20eb9e82c4a16a33";
      sha256 = "1s42r0l0xkhlp6rbc23cm4vlda91il6cg53w33hqfhd2wz91s66w";
    };
    buildPhase = ''
      :
    '';
    installPhase = ''
      mkdir -p $out
      cp edit.py $out/
    '';
  };

in {
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
      home.file.".weechat/python/autoload/url_hint.py".source = ./config/python/autoload/url_hint.py;
      home.file.".weechat/python/autoload/edit.py".source = "${edit-weechat}/edit.py";
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
