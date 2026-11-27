{ ... }:

{
  config = {
    home-manager.users.jfelice = { pkgs, ... }: {
      home.packages = [ pkgs.beamPackages.expert ];
    };
  };
}
