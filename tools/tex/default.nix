{ pkgs, ... }:

{
  config = {
    environment.systemPackages = [
      pkgs.texlive.combined.scheme-full
      #(pkgs.texlive.combine {
      #  inherit (pkgs.texlive)
      #    scheme-tetex
      #    collection-fontsextra
      #    enumitem;
      #})
    ];
  };
}
