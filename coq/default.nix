{ pkgs, ... }:

{
  config = {
    environment.systemPackages = [ pkgs.coq ];
  };
}
