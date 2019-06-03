with import (fetchGit {
  url = "git@github.com:eraserhd/nixpkgs.git";
  ref = "eraserhd";
  rev = "5809c85fbf0a9c6abbc33872875c9d34eaee97ed";
}) {
  config = {
    packageOverrides = pkgs: rec {
      kakoune = (pkgs.kakoune.withPlugins (with kakounePlugins; [
        parinfer-rust
      ])).overrideAttrs (old: rec {
        version = "a0d264ef07c4b5bc5a9d7ff3629eb45e099e96bf";
        src = pkgs.fetchFromGitHub {
          repo = "kakoune";
          owner = "eraserhd";
          rev = "a0d264ef07c4b5bc5a9d7ff3629eb45e099e96bf";
          sha256 = "0jxy86p54s940lqwialbh0r29ljdchfaww7pcxwfyfk06ibjwxf6";
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
    file
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
