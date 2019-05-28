with import (fetchGit {
  url = "git@github.com:eraserhd/nixpkgs.git";
  ref = "eraserhd";
  rev = "5809c85fbf0a9c6abbc33872875c9d34eaee97ed";
}) {
  config = {
    packageOverrides = pkgs: rec {
      kakoune = (pkgs.kakoune.withPlugins (with kakounePlugins; [
        kak-ansi
        parinfer-rust
      ])).overrideAttrs (old: rec {
        version = "478edc0b97b4acefdf228754f26da490bcafd9ba";
        src = pkgs.fetchFromGitHub {
          repo = "kakoune";
          owner = "eraserhd";
          rev = "478edc0b97b4acefdf228754f26da490bcafd9ba";
          sha256 = "01yfw77q8ppw7m8bscyvf1d5iki8gq3rgiljffx0hxv2b7dvv147";
        };
      });
      kakounePlugins = pkgs.kakounePlugins // {
      };
      kakouneWrapper = pkgs.callPackage ./kakoune-wrapper {};
      weechat = (pkgs.weechat.override {
        configure = {availablePlugins, ...}: {
          scripts = with pkgs.weechatScripts; [ wee-slack ];
          plugins = with availablePlugins; [ python ];
        };
      });
    };
  };
};

pkgs.buildEnv {
  name = "my-packages";
  paths = let osPackages = if stdenv.isDarwin
                           then [ reattach-to-user-namespace ]
                           else [ texlive.combined.scheme-tetex ];
  in [
    ag
    asciidoc
    autossh
    awscli
    bashInteractive
    bash-completion
    chez
    clojure
    docker-compose
    graphviz
    gitAndTools.hub
    gtypist
    ii
    jq
    kakouneWrapper
    killall
    leiningen
    nodejs
    parinfer-rust
    plan9port
    rlwrap
    sassc
    terraform
    tmate
    tmux
    vault
    weechat
    wget
  ] ++ osPackages;
}
