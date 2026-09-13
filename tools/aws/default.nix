{ pkgs, ... }:

{
  config = {
    environment.systemPackages = [
      pkgs.awscli2
    ];

    home-manager.users.jfelice = { pkgs, config, ... }: {
      home.file.".aws/config".source = ./config;
      age.secrets."boto" = {
        file = ./boto.age;
        path = "${config.home.homeDirectory}/.boto";
      };
      age.secrets."aws-credentials" = {
        file = ./credentials.age;
        path = "${config.home.homeDirectory}/.aws/credentials";
      };
    };
  };
}
