{ pkgs, ... }:

{
  config = {
    environment.systemPackages = with pkgs; [ k9s ];
  };
}
