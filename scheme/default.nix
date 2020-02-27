{ pkgs, ... }:

{
  config = {
    environment.systemPackages = with pkgs; [
      gambit
      gerbil
    ];
  };
}
