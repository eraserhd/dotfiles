{ pkgs, ... }:

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
      rev = "91759d8a0c5214649a5af565bbb28ac950d04028";
      sha256 = "OVDEGdUAuQqdVN0fcfJsKVoTPKUZlvZO6puGnEiN1eI=";
    };

    MouseFollowsFocus = pkgs.fetchFromGitHub {
      owner = "eraserhd";
      repo = "Spoons";
      rev = "16d3ac1c013ec4d835e62f71d09348c5e716afb7";
      sha256 = "J5RJRhU2k/CqsKzd1zThWOM7ziEOG2zXWDY49IuyIho=";
    };
  };

in {
  config = {
    services.hammerspoon.enable = true;

    home-manager.users.jfelice = _: {
      home.file = {
        ".hammerspoon/init.lua" = { source = "${checkedConfig}/init.lua"; };
      } // {
        ".hammerspoon/Spoons/WindowSigils.spoon" = { source = "${spoons.WindowSigils}/Source/WindowSigils.spoon"; };
        ".hammerspoon/Spoons/MouseFollowsFocus.spoon" = { source = "${spoons.MouseFollowsFocus}/Source/MouseFollowsFocus.spoon"; };
      };
    };
  };
}
