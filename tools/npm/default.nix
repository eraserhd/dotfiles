{ pkgs, ... }:

{
  config = {
    home-manager.users.jfelice = { pkgs, config, ... }: {
      #age.secrets."npmrc" = {
      #  file = ./npmrc.age;
      #  path = "${config.home.homeDirectory}/.npmrc";
      #};
    };
  };
}
