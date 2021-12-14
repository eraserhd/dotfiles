{ pkgs, ... }:

{
  config = {
    nixpkgs.overlays = [
      (self: super: {
        nexus-tools = super.callPackage ./tools {};
      })
    ];

    environment.systemPackages = [ pkgs.nexus-tools ];
  };
}
