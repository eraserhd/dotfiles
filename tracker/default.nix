{ options, ... }:

{
  config = (if (builtins.hasAttr "gnome3" options.services)
  then {
    services.gnome3.tracker.enable = true;
    services.gnome3.tracker-miners.enable = true;
    systemd.user.targets.default.wants = [
      "tracker-miner-fs.service"
    ];
  } else {
  });
}
