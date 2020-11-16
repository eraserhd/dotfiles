{ pkgs, options, ... }:

let
  openscad-cli = pkgs.stdenv.mkDerivation {
    pname = "openscad-cli";
    version = "0.0";

    src = pkgs.fetchFromGitHub {
      owner = "onealexharms";
      repo = "openscad-cli";
      rev = "41de5cc63baa9f48b9755456a326d6c2ecf0f090";
      sha256 = "13lphyh54vqv4l9kv1l47cd5aimdxkjzk01gpyk1kfp6259gkbdy";
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
