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
      rev = "d545fdec86b56f9c35fffe36316ad9ce332aa610";
      sha256 = "sha256-029TxRcUpMV1BFTvjP85zhBMX7Rb/lWmpUEI4ylZOjQ=";
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
