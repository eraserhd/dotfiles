{ pkgs, lib, ... }:

{
  config = {
    environment.systemPackages = [
      pkgs.plan9port
    ] ++ lib.optional pkgs.stdenv.isDarwin pkgs.osxfuse;

    home-manager.users.jfelice = { pkgs, ... }: {
      home.file.".config/plan9/plumbing".source = ./plumbing;
    };
  };
}
