with import <nixpkgs> {};

pkgs.buildEnv {
  name = "my-packages";
  paths = let darwinPackages = if stdenv.isDarwin
                               then [ reattach-to-user-namespace ]
                               else [];
  in [
    ag
    asciidoc
    autossh
    awscli
    bash-completion
    chez
    clojure
    graphviz
    gtypist
    jq
    kakoune
    leiningen
    nodejs
    plan9port
    rlwrap
    terraform
    tmate
    tmux
    wget
  ] ++ darwinPackages;
}
