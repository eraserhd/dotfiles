{ config, pkgs, ... }:

{
  config = {
    environment.systemPackages = with pkgs; [
      gitFull
      gitAndTools.hub
      gitAndTools.git-browse-link
      local.git-scripts
    ];

    # Needed for `hub browse`
    environment.variables.BROWSER = config.local.browserCommand;

    nixpkgs.overlays = [ (self: super: {
      local = (super.local or {}) // {
        git-scripts = super.callPackage ./git-scripts {};
      };
    }) ];

    home-manager.users.jfelice = { pkgs, ... }: {
      home.file.".config/hub".source = ./hub;
      home.file.".gitconfig".source = ./gitconfig;
      home.file.".gitignore_global".source = ./gitignore_global;
    };
  };
}
