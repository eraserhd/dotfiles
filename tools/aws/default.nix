{ pkgs, ... }:

{
  config = {
    environment.systemPackages = [
      pkgs.awscli2
    ];

    home-manager.users.jfelice = { pkgs, ... }: {
      home.file.".boto".source = ./boto;
      home.file.".aws/config".source = ./config;
      home.file.".aws/credentials".source = ./credentials;
    };
  };
}
