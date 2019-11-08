{ pkgs, ... }:

{
  config = {
    environment.systemPackages = with pkgs; [
      tasksh
      taskwarrior
    ];

    home-manager.users.jfelice = { pkgs, ... }: {
      home.file.".taskrc".text = ''
        data.location=~/src/data/tasks

        include /nix/store/ik7xac808ha8v904m1h7y8pr1h16bw8d-taskwarrior-2.5.1/share/doc/task/rc/dark-16.theme
      '';

    };
  };
}
