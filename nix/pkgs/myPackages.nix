with import (fetchGit {
  url = "git@github.com:eraserhd/nixpkgs.git";
  ref = "eraserhd";
  rev = "88965be47d3149e7f3559bb5c37f053d05c43790";
}) {
  config = {
    packageOverrides = pkgs: rec {
      kakoune = (pkgs.kakoune.withPlugins (with kakounePlugins; [
        kak-ansi
        parinfer-rust
      ])).overrideAttrs (old: rec {
        version = "92972bed4fb4ff6dffd32169bc437de34acac6a9";
        src = pkgs.fetchFromGitHub {
          repo = "kakoune";
          owner = "mawww";
          rev = "92972bed4fb4ff6dffd32169bc437de34acac6a9";
          sha256 = "1cn32qyp0zki392200zxzp0mjikalrc92g1anav3xwplh1zlv1ks";
        };
      });
      kakounePlugins = pkgs.kakounePlugins // {
      };
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
    kakoune
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
