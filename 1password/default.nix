{ pkgs, ... }:

{
  config = {
    nixpkgs.config.allowUnfree = true;
    environment.systemPackages = [ pkgs._1password ];
  };
}
