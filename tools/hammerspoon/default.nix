{ pkgs, options, lib, ... }:

with lib;
let
  checkedConfig = pkgs.stdenv.mkDerivation {
    name = "hammerspoon-config";
    src = ./.;
    buildInputs = with pkgs; [ lua ];
    buildPhase = ''
      luac -p init.lua
    '';
    installPhase = ''
      mkdir $out
      cp init.lua $out/
    '';
  };

  spoons = {
    WindowSigils = pkgs.fetchFromGitHub {
      owner = "eraserhd";
      repo = "Spoons";
      rev = "18394dbce6f8e4b0a5733e5b8426256f236163bf";
      sha256 = "JL1Vg+r787EUrPArUmcfFsSmz+7G0KCAzylAUF5EBO0=";
    };

    MouseFollowsFocus = pkgs.fetchFromGitHub {
      owner = "eraserhd";
      repo = "Spoons";
      rev = "16d3ac1c013ec4d835e62f71d09348c5e716afb7";
      sha256 = "J5RJRhU2k/CqsKzd1zThWOM7ziEOG2zXWDY49IuyIho=";
    };
  };

in {
  config = (if (builtins.hasAttr "hammerspoon" options.services)
  then {
    services.hammerspoon = {
      enable = true;
      enableCommandLine = true;
    };

    home-manager.users.jfelice = _: {
      home.file = {
        ".hammerspoon/init.lua" = { source = "${checkedConfig}/init.lua"; };
      } // (mapAttrs' (name: value: nameValuePair ".hammerspoon/Spoons/${name}.spoon" { source = "${value}/Source/${name}.spoon"; }) spoons);
    };
  }
  else {
  });
}
