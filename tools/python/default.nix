{ ... }:

{
  config = {
    home-manager.users.jfelice = { pkgs, ... }: {
      home.packages = [ pkgs.basedpyright ];
    };
  };
}
