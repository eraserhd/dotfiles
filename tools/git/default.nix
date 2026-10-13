{ config, pkgs, ... }:

{
  config = {
    environment.systemPackages = with pkgs; [
      gitFull
      gh
      gh-dash
      local.git-scripts
      git-revise
    ];

    home-manager.users.jfelice = { pkgs, ... }: {
      home.file.".gitconfig".source = ./gitconfig;
      home.file.".config/git/ignore".text = ''
        .DS_Store
        .*.swp
        tags
        build-deps
        TAGS
      '';
    };
  };
}
