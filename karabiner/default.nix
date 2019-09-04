{ pkgs, ... }:

{
  config = {
    home-manager.users.jfelice = { pkgs, ... }: {
      home.file.".config/karabiner/assets/complex_modifications/alacritty.json".source = ./alacritty.json;
      home.file.".config/karabiner/karabiner.json".source = ./karabiner.json;
    };
  };
}
