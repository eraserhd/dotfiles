{ pkgs, ... }:

{
  config = {
    nix.package = pkgs.nixFlakes;
    home-manager.users.jfelice = { pkgs, ... }: {
      home.file.".config/nix/nix.conf".text = ''
        experimental-features = nix-command flakes
      '';
    };
  };
}
