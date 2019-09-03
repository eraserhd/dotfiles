{ pkgs, ... }:

{
  config = {
    environment.systemPackages = [ pkgs.rlwrap ];

    home-manager.users.jfelice = { pkgs, ... }: {
      home.file.".inputrc".text = ''
        "\C-x\C-e": rlwrap-call-editor
      '';
    };
  };
}
