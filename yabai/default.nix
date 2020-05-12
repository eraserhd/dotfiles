{ ... }:

{
  config = {
    home-manager.users.jfelice = { pkgs, ... }: {
      home.file.".yabairc" = {
        executable = true;
        text = ''
          exec ${pkgs.zsh}/bin/zsh -l -c 'yabai-config'
        '';
      };
    };
  };
}
