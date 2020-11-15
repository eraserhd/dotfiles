{ pkgs, options, ... }:

let
  openscad-cli = pkgs.stdenv.mkDerivation {
    pname = "openscad-cli";
    version = "0.0";

    src = pkgs.fetchFromGitHub {
      owner = "onealexharms";
      repo = "openscad-cli";
      rev = "d4a02e92dea6a0f08b7a67313b13b043d0a9f4ec";
      sha256 = "1l0drqpjy2p0g6i9mcrqbcap64wnq1yymrwic1m7i6cgfnrdcyn2";
    };

    installPhase = ''
      mkdir -p $out/bin
      cp openscad-cli $out/bin/
      ln -s $out/bin/openscad-cli $out/bin/osc
    '';
  };
in {
  config = (if (builtins.hasAttr "homebrew" options)
  then {
    homebrew.casks = [
      "openscad"
      "ultimaker-cura"
    ];
    environment.systemPackages = [ openscad-cli ];
  }
  else {
  });
}
