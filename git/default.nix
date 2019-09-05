{ pkgs, ... }:

let
  localGitScripts = pkgs.stdenv.mkDerivation {
    name = "local-git-scripts-2019.09.05";
    src = ./.;
    installPhase = ''
      mkdir -p $out/bin
      cp git-cleanup git-fork git-l $out/bin/
    '';
  };
in {
  config = {
    environment.systemPackages = with pkgs; [
      gitFull
      gitAndTools.hub
      localGitScripts
    ];

    home-manager.users.jfelice = { pkgs, ... }: {
      home.file.".config/hub".source = ./hub;
      home.file.".gitconfig".source = ./gitconfig;
      home.file.".gitignore_global".source = ./gitignore_global;
    };
  };
}
