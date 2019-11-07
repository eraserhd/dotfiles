{ pkgs, ... }:

{
  config = {
    environment.systemPackages = with pkgs; [
      taskwarrior
    ];

    home-manager.users.jfelice = { pkgs, ... }: {
      home.file.".taskrc".text = ''
        data.location=~/src/data/tasks

        # Color theme (uncomment one to use)
        #include /nix/store/ik7xac808ha8v904m1h7y8pr1h16bw8d-taskwarrior-2.5.1/share/doc/task/rc/light-16.theme
        #include /nix/store/ik7xac808ha8v904m1h7y8pr1h16bw8d-taskwarrior-2.5.1/share/doc/task/rc/light-256.theme
        #include /nix/store/ik7xac808ha8v904m1h7y8pr1h16bw8d-taskwarrior-2.5.1/share/doc/task/rc/dark-16.theme
        include /nix/store/ik7xac808ha8v904m1h7y8pr1h16bw8d-taskwarrior-2.5.1/share/doc/task/rc/dark-256.theme
        #include /nix/store/ik7xac808ha8v904m1h7y8pr1h16bw8d-taskwarrior-2.5.1/share/doc/task/rc/dark-red-256.theme
        #include /nix/store/ik7xac808ha8v904m1h7y8pr1h16bw8d-taskwarrior-2.5.1/share/doc/task/rc/dark-green-256.theme
        #include /nix/store/ik7xac808ha8v904m1h7y8pr1h16bw8d-taskwarrior-2.5.1/share/doc/task/rc/dark-blue-256.theme
        #include /nix/store/ik7xac808ha8v904m1h7y8pr1h16bw8d-taskwarrior-2.5.1/share/doc/task/rc/dark-violets-256.theme
        #include /nix/store/ik7xac808ha8v904m1h7y8pr1h16bw8d-taskwarrior-2.5.1/share/doc/task/rc/dark-yellow-green.theme
        #include /nix/store/ik7xac808ha8v904m1h7y8pr1h16bw8d-taskwarrior-2.5.1/share/doc/task/rc/dark-gray-256.theme
        #include /nix/store/ik7xac808ha8v904m1h7y8pr1h16bw8d-taskwarrior-2.5.1/share/doc/task/rc/dark-gray-blue-256.theme
        #include /nix/store/ik7xac808ha8v904m1h7y8pr1h16bw8d-taskwarrior-2.5.1/share/doc/task/rc/solarized-dark-256.theme
        #include /nix/store/ik7xac808ha8v904m1h7y8pr1h16bw8d-taskwarrior-2.5.1/share/doc/task/rc/solarized-light-256.theme
        #include /nix/store/ik7xac808ha8v904m1h7y8pr1h16bw8d-taskwarrior-2.5.1/share/doc/task/rc/no-color.theme
      '';

    };
  };
}
