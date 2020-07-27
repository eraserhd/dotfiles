{ options, pkgs, ... }:

let
  fonts = with pkgs; [ mononoki ];
in {
  config = {
    fonts.enableFontDir = true;
    environment.systemPackages = fonts;
  } // (if (options ? launchd)
  then {
    fonts.fonts = fonts;
  }
  else {
  });
}
