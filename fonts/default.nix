{ pkgs, ... }:

{
  config = {
    fonts.enableFontDir = true;
    fonts.fonts = with pkgs; [
      (input-fonts.overrideAttrs (oldAttrs: {
        src = ./Input-Font.zip;
        outputHash = "14bfyjnhsbxv9v76ngn0v7fb6f7xahnmny38251nm9dhai66zqx8";
      }))
      mononoki
    ];
  };
}
