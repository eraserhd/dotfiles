{ config, options, lib, pkgs, ... }:

with lib;
let
  # natscli reads $NATS_TOKEN, which is read out of the agenix file at start-up
  # rather than coming from plugbench.token (which would put it in the store).
  tokenFile = config.local.nats.clientTokenFile;

  listenCommand = schema: ''
    NATS_TOKEN="$(${pkgs.coreutils}/bin/cat ${escapeShellArg tokenFile})" \
      nats reply cmd.show.url.${schema} --command "/bin/sh -c '${config.local.browser.command} \"\$NATS_REQUEST_BODY\"'"
  '';

  launchdConfig = schema: {
    launchd.user.agents."open-${schema}-in-browser" = {
      path = with pkgs; [ natscli ];
      script = listenCommand schema;
      serviceConfig = {
        KeepAlive = true;
      };
    };
  };

  systemdConfig = schema: {
    systemd.user.services."open-${schema}-in-browser" = {
      wantedBy = ["default.target"];
      path = with pkgs; [ natscli ];
      script = listenCommand schema;
    };
  };

  schemaLaunchConfig = schema: if (builtins.hasAttr "launchd" options)
                               then launchdConfig schema
                               else systemdConfig schema;

  firefox = if (builtins.hasAttr "launchd" options)
            then "/Applications/Firefox.app/Contents/MacOS/firefox"
            else "${pkgs.firefox}/bin/firefox";
in {
  options.local.browser = {
    command = mkOption {
      description = "Command to open a URL in a browser";
      type = types.str;
      default = "${firefox} --new-tab --url";
    };
  };

  config = lib.mkMerge [
    (schemaLaunchConfig "http")
    (schemaLaunchConfig "https")
  ];
}
