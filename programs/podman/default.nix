{ pkgs, ... }:
{
  config = {
    environment.systemPackages = with pkgs; [
      podman

      # dependencies
      qemu
      xz # xzcat, during podman machine init
    ];
  };
}

