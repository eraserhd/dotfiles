{ stdenv, pkgs, ... }:

pkgs.buildEnv {
  name = "my-packages";
  paths = let osPackages = if stdenv.isDarwin
                           then [ pkgs.reattach-to-user-namespace ]
                           else [ pkgs.texlive.combined.scheme-tetex ];
  in with pkgs; [
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
    jbake
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
    weechat
    wget
  ] ++ osPackages;
}
