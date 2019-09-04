{ pkgs, ... }:

{
  config = {
    environment.systemPackages = with pkgs; [
      awscli
    ];

    home-manager.users.jfelice = { pkgs, ... }: {
      home.file.".boto".source = ./boto;
      home.file.".aws/config".source = ./config;
      home.file.".aws/credentials".source = ./credentials;
    };
  };
}
