{ pkgs, ... }:

{
  config = {
    fonts.enableFontDir = true;
    fonts.fonts = with pkgs; [
      (input-fonts.overrideAttrs (oldAttrs: {
        src = ./Input-Font.zip;
      }))
      mononoki
    ];
  };
}
