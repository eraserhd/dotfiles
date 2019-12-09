{ options, pkgs, ... }:

{
  config = (if (builtins.hasAttr "gnome3" options.services)
  then {
    programs.dconf.profiles.user =
    let
      customDconf = pkgs.writeTextFile {
        name = "user-dconf";
        destination = "/dconf/user";
        text = ''
          [org/freedesktop/Tracker/Miner/Files]
          index-on-battery=false
          index-recursive-directories=[ '~/src' ]
        '';
      };

      customDconfDb = pkgs.stdenv.mkDerivation {
        name = "user-dconf-db";
        buildCommand = ''
          ${pkgs.dconf}/bin/dconf compile $out ${customDconf}/dconf
        '';
      };
    in pkgs.writeTextFile {
      name = "dconf-user-profile";
      text = ''
        file-db:${customDconfDb}
      '';
    };
    services.gnome3.tracker.enable = true;
    services.gnome3.tracker-miners.enable = true;
    systemd.user.targets.default.wants = [
      "tracker-miner-fs.service"
    ];
  } else {
  });
}
