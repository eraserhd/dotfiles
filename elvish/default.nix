{ pkgs, ... }:

{
  config = {
    environment.systemPackages = [ pkgs.elvish ];
  };
}
