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
    clojure
    jq
    kakoune
    leiningen
    nodejs
    plan9port
    rlwrap
    tmux
    wget
  ];
}
