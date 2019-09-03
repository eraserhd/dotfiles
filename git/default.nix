{ pkgs, ... }:

{
  config = {
    environment.systemPackages = with pkgs; [
      gitFull
      gitAndTools.hub
    ];

    home-manager.users.jfelice = { pkgs, ... }: {
      home.file.".gitconfig".source = ./gitconfig;
    };
  };
}
