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
  homebrew.casks = [
    "hammerspoon"
  ];

  home-manager.users.jfelice = { pkgs, ... }: {
    home.file.".hammerspoon/init.lua".source = "${checkedConfig}/init.lua";
  };
}
