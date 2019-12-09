{ lib, config, options, ... }:

with lib;
let
  cfg = config.local.sendOutgoingMailWithSES;
in {
  options = {
    local.sendOutgoingMailWithSES.enable = mkEnableOption "SES";
  };

  config = mkIf cfg.enable
  (if (builtins.hasAttr "ssmtp" options.services)
  then {
    services.ssmtp = {
      enable = true;
      useTLS = true;
      useSTARTTLS = true;
      hostName = "email-smtp.us-west-2.amazonaws.com:587";
      domain = "${config.networking.hostName}.eraserhead.net";
      root = "jason.m.felice@gmail.com";
    } // import ./ses-user.nix;
  }
  else {
  });
}
