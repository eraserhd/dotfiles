{ pkgs, ... }:

let
  compiledMagicFile = pkgs.runCommand "compiled-magic" {} ''
    mkdir -p $out
    (
      cd $out
      ${pkgs.file}/bin/file -C -m ${./magic}
      mv *-magic.mgc magic.mgc
    )
  '';
in {
  config = {
    environment.systemPackages = [ pkgs.file ];
    home-manager.users.jfelice = { pkgs, ... }: {
      home.file.".magic.mgc".source = "${compiledMagicFile}/magic.mgc";
    };
  };
}
