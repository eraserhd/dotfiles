with import (fetchGit {
  url = "git@github.com:eraserhd/nixpkgs.git";
  ref = "eraserhd";
  rev = "9123e8762a93a99b88c7f863d2b9496c4edf21a3";
})
{
  config = {
    packageOverrides = pkgs: rec {
      kakoune-unwrapped = let ver = builtins.fromJSON (builtins.readFile ./kakoune-version.json);
      in pkgs.kakoune-unwrapped.overrideAttrs (old: rec {
        version = ver.rev;
        src = pkgs.fetchFromGitHub ver;
      });
      kakoune = pkgs.kakoune.override {
        configure = {
          plugins = with pkgs.kakounePlugins; [ parinfer-rust ];
        };
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
