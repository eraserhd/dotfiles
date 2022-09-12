{ config, options, lib, pkgs, ... }:

with lib;
{
  options = {
    local.browserCommand = mkOption {
      description = "Command to open a URL in a browser";
      type = types.str;
      default = "open";
    };

    local.openURLsInChrome = mkEnableOption "Open URLs in Chrome";
  };

  config = mkIf config.local.openURLsInChrome
    (if (builtins.hasAttr "launchd" options)
     then {
       launchd.user.agents.open-in-chrome-tab = {
         path = with pkgs; [
           natscli
           open-in-chrome-tab
         ];
         script = ''
           export PATH="$PATH":/usr/bin
           nats reply browser.open --command "/bin/sh -c 'open-in-chrome-tab \"$NATS_REQUEST_BODY\"'"
         '';
         serviceConfig = {
           KeepAlive = true;
         };
       };
       environment.systemPackages = [ pkgs.open-in-chrome-tab ];
     }
     else {
       assertions = [{
         assertion = false;
         message = "local.openURLsInChrome is not available on NixOS yet";
       }];
     });
}
