{ pkgs, ... }:

{
  config = {
    environment.systemPackages = [
      ( pkgs.callPackage ./tools {} )
    ];
  };
}
