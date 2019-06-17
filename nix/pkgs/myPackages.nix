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
        version = "5888e23e020ac3fd56a93c75682a216bd7085593";
        src = pkgs.fetchFromGitHub {
          repo = "kakoune";
          owner = "mawww";
          rev = "5888e23e020ac3fd56a93c75682a216bd7085593";
          sha256 = "1m0g7y7565zayfjrd44v9lwjs8ni70n57h9cs6ygjvjw4cmcj268";
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
