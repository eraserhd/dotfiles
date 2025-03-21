{ pkgs, ... }:

{
  config = {
    environment.systemPackages = with pkgs; [
      R
      rstudio
    ];
  };
}
