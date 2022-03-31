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
      rev = "24ad70e7137269b3eee6c6e01b360cf14c7bd8cc";
      sha256 = "xKBp3Sw6F96526PDJ75x85eBeoI4a/afZXnGOMS4fQY=";
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
