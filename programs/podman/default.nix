{ pkgs, ... }:
{
  config = {
    nixpkgs.overlays = [
      (self: super: {
        podman = super.callPackage ./podman.nix {};
      })
    ];

    environment.systemPackages = with pkgs; [
      podman

      # dependencies
      qemu
      xz # xzcat, during podman machine init
    ];
  };
}

