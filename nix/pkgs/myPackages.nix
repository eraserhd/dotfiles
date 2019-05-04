with import <nixpkgs> {};

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
