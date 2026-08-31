{ pkgs, ... }:

{
  config = {
    environment.systemPackages = [
      pkgs.texliveFull
    ];
  };
}
