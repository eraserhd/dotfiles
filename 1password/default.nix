{ pkgs, ... }:

{
  config = {
    nixpkgs.config.allowUnfree = true;
    # killed: 9 :(
    #environment.systemPackages = [ pkgs._1password ];
  };
}
