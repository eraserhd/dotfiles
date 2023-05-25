{ config, options, lib, pkgs, ... }:

with lib;
let
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

  config = if (builtins.hasAttr "launchd" options)
           then {
             launchd.user.agents.open-https-in-firefox = {
               path = with pkgs; [ natscli ];
               script = ''
                 nats reply cmd.show.url.https --command "/bin/sh -c '${config.local.browser.command} \"\$NATS_REQUEST_BODY\"'"
               '';
               serviceConfig = {
                 KeepAlive = true;
               };
             };
             launchd.user.agents.open-http-in-firefox = {
               path = with pkgs; [ natscli ];
               script = ''
                 nats reply cmd.show.url.http --command "/bin/sh -c '${config.local.browser.command} \"\$NATS_REQUEST_BODY\"'"
               '';
               serviceConfig = {
                 KeepAlive = true;
               };
             };
           }
           else {
             systemd.user.services.open-https-in-firefox = {
               wantedBy = ["default.target"];
               path = with pkgs; [ natscli ];
               script = ''
                 nats reply cmd.show.url.https --command "/bin/sh -c '${config.local.browser.command} \"\$NATS_REQUEST_BODY\"'"
               '';
             };
             systemd.user.services.open-http-in-firefox = {
               wantedBy = ["default.target"];
               path = with pkgs; [ natscli ];
               script = ''
                 nats reply cmd.show.url.http --command "/bin/sh -c '${config.local.browser.command} \"\$NATS_REQUEST_BODY\"'"
               '';
             };
           };
}
