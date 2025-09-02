{ pkgs, ... }:

{
  config = {
    services.xserver.windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
    };

    environment.systemPackages = with pkgs; [
      dmenu
    ];

    home-manager.users.jfelice = { pkgs, ... }: {
      home.file.".config/xmonad/xmonad.hs".text = builtins.readFile ./config/xmonad.hs;
    };

  };
}
