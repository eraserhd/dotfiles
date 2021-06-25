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

  WindowSigils-src = pkgs.fetchFromGitHub {
    owner = "eraserhd";
    repo = "Spoons";
    rev = "91759d8a0c5214649a5af565bbb28ac950d04028";
    sha256 = "OVDEGdUAuQqdVN0fcfJsKVoTPKUZlvZO6puGnEiN1eI=";
  };

in {
  config = {
    services.hammerspoon.enable = true;

    home-manager.users.jfelice = { pkgs, ... }: {
      home.file.".hammerspoon/init.lua".source = "${checkedConfig}/init.lua";
      home.file.".hammerspoon/Spoons/WindowSigils.spoon".source = "${WindowSigils-src}/Source/WindowSigils.spoon";
      home.file.".hammerspoon/Spoons/Bubbles.spoon".source = ./Bubbles.spoon;
      home.file.".hammerspoon/Spoons/MouseFollowsFocus.spoon".source = ./MouseFollowsFocus.spoon;
    };
  };
}
