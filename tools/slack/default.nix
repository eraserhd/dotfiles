{ pkgs, ... }:

{
  config = {
    environment.systemPackages = [
      pkgs.slack
    ];
  };
}
