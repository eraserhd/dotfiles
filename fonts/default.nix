{ options, lib, pkgs, ... }:

# Input-Font.zip.age is a licensed copy of Input (https://input.fontbureau.com),
# kept here as an archive only.  It used to be unpacked into a font package,
# but that needs the zip in the store at evaluation time, which agenix can't
# do.  To get it back:
#
#     agenix -d fonts/Input-Font.zip.age > Input-Font.zip
#
{
  config = {
    nixpkgs.config.allowUnfree = true;
    fonts = {
      packages = with pkgs; [
        mononoki
        fira-sans
      ];
    };
  };
}
