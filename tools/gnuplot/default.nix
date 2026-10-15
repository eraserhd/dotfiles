{ pkgs, ... }:

{
  config = {
    home-manager.users.jfelice = { ... }: {
      home.packages = [ pkgs.gnuplot ];
      home.file.".gnuplot".source = ./gnuplotrc;
    };
  };
}
