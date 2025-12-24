{ pkgs, lib, config, ... }:

let
  myRPackages = with pkgs.rPackages; [
    tidyverse
    ggplot2
    knitr
    rmarkdown
  ];

  myR = pkgs.rWrapper.override { packages = myRPackages; };
  myRStudio = pkgs.rstudioWrapper.override { packages = myRPackages; };

in {
  config = {
    environment.systemPackages = [
      myR
    ] ++ lib.optional config.local.kits.develop.enable myRStudio
    ;
  };
}
