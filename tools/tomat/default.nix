{ pkgs, ... }:

{
  config = {
    environment.systemPackages = [ pkgs.tomat ];

    home-manager.users.jfelice = { pkgs, ... }: {
      services.tomat = {
        enable = true;

        settings = {
          timer = {
            work = 45;
            break = 15;
          };
        };
      };
    };
  };
}
