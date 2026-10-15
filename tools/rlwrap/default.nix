{ pkgs, ... }:

{
  config = {
    home-manager.users.jfelice = { pkgs, ... }: {
      home.packages = [ pkgs.rlwrap ];
      home.file.".inputrc".text = ''
        "\C-x\C-e": rlwrap-call-editor
      '';
    };
  };
}
