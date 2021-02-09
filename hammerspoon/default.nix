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
in {
  config = {
    services.hammerspoon.enable = true;

    home-manager.users.jfelice = { pkgs, ... }: {
      home.file.".hammerspoon/init.lua".source = "${checkedConfig}/init.lua";
      home.file.".hammerspoon/Spoons/WindowSigils.spoon".source = ./WindowSigils.spoon;
      home.file.".hammerspoon/Spoons/Bubbles.spoon".source = ./Bubbles.spoon;
      home.file.".hammerspoon/Spoons/MouseFollowsFocus.spoon".source = ./MouseFollowsFocus.spoon;
    };
  };
}
