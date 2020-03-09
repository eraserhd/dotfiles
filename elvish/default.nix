{ pkgs, ... }:

{
  config = {
    local.loginShell.package = pkgs.elvish;
    environment.systemPackages = [ pkgs.elvish ];
    environment.shells = [ pkgs.elvish ];

    home-manager.users.jfelice = { pkgs, ... }: {
      home.file.".elvish/rc.elv".text = ''
        use direnv
      '';
      home.file.".elvish/lib/direnv.elv".text = ''
        ## hook for direnv
        @edit:before-readline = $@edit:before-readline {
          try {
            m = [("${pkgs.direnv}/bin/direnv" export elvish | from-json)]
            if (> (count $m) 0) {
              m = (explode $m)
              keys $m | each [k]{
                if $m[$k] {
                  set-env $k $m[$k]
                } else {
                  unset-env $k
                }
              }
            }
          } except e {
            echo $e
          }
        }
      '';
    };
  };
}
