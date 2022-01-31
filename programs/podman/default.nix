{ pkgs, ... }:
{
  config = {
    environment.systemPackages = with pkgs; [
      podman
      podman-compose

      # dependencies
      qemu
      xz # xzcat, during podman machine init
    ];
  };
}

