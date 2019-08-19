{ pkgs, ... }:

{
  config = {
    environment.systemPackages = with pkgs; [
      awscli
    ];
  };
}
