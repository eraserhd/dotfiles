{ pkgs, ... }:

{
  config = {
    fonts.enableFontDir = true;
    fonts.fonts = with pkgs; [
      julia-mono
      mononoki
    ];
  };
}
