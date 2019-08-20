{ pkgs, lib, ... }:

{
  config = {
    environment.systemPackages = [
      pkgs.plan9port
    ] ++ lib.optional pkgs.stdenv.isDarwin pkgs.osxfuse;
  };
}
