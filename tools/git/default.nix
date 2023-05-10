{ config, pkgs, ... }:

{
  config = {
    environment.systemPackages = with pkgs; [
      gitFull
      gitAndTools.gh
      #gitAndTools.gitout -- no scheme atm
      local.git-scripts
      git-crypt
      git-revise
    ];

    # Needed for `hub browse`
    environment.variables.BROWSER = config.local.browser.command;

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
