{ pkgs, ... }:

{
  config = {
    fonts.enableFontDir = true;
    fonts.fonts = with pkgs; [ mononoki ];
  };
}
