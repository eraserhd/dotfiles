{ pkgs, ... }:

{
  config = {
    # Until nixpkgs is past NixOS/nixpkgs#562409.
    nixpkgs.overlays = [
      (self: super: {
        pythonPackagesExtensions = super.pythonPackagesExtensions ++ [
          (pySelf: pySuper: {
            anki-mac-helper = pySuper.anki-mac-helper.overridePythonAttrs {
              version = "0.1.1";
            };
          })
        ];
      })
    ];

    home-manager.users.jfelice = { config, ... }: {
      age.secrets."anki-sync-key" = {
        file = ./sync-key.age;
        path = "${config.home.homeDirectory}/.local/share/anki/sync-key";
      };

      programs.anki = {
        enable = true;
        addons = [ pkgs.ankiAddons.anki-connect ];
        profiles."User 1" = {
          default = true;
          sync = {
            username = "jason.m.felice@gmail.com";
            keyFile = config.age.secrets."anki-sync-key".path;
          };
        };
      };
    };
  };
}
