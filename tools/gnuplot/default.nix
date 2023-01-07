{ pkgs, ... }:

{
  config = {
    environment.systemPackages = with pkgs; [ gnuplot ];

    home-manager.users.jfelice = { ... }: {
      home.file.".gnuplot".source = ./gnuplotrc;
    };
  };
}
