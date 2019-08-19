{ stdenv, pkgs, ... }:

pkgs.buildEnv {
  name = "my-packages";
  paths = let
    darwinPackages = with pkgs; [
      alacritty
      reattach-to-user-namespace
    ];
    linuxPackages = with pkgs; [
      texlive.combined.scheme-tetex
    ];
    osPackages = if stdenv.isDarwin
                 then darwinPackages
                 else linuxPackages;
  in with pkgs; [
    autossh
    awscli
    chez
    clojure
    docker-compose
    gtypist
    jq
    kakouneWrapper
    killall
    leiningen
    nodejs
    parinfer-rust
    plan9port
    terraform
    weechat
  ] ++ osPackages;
}
