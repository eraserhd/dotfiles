with import <nixpkgs> {
  config = {
    packageOverrides = pkgs: {
      kakoune = pkgs.kakoune.overrideAttrs (old: rec {
        version = "92972bed4fb4ff6dffd32169bc437de34acac6a9";
        src = pkgs.fetchFromGitHub {
          repo = "kakoune";
          owner = "mawww";
          rev = "92972bed4fb4ff6dffd32169bc437de34acac6a9";
          sha256 = "1cn32qyp0zki392200zxzp0mjikalrc92g1anav3xwplh1zlv1ks";
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
    bash
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
    leiningen
    nodejs
    plan9port
    rlwrap
    sassc
    terraform
    tmate
    tmux
    vault
    (weechat.override {
      configure = {availablePlugins, ...}: {
        scripts = with pkgs.weechatScripts; [
          wee-slack
        ];
        plugins = with availablePlugins; [
          (python.withPackages (ps: with ps; [ websocket_client ]))
        ];
      };
    })
    wget
  ] ++ osPackages;
}
