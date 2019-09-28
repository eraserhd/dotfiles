{ lib, config, ... }:

with lib;
{
  options.local.sendOutgoingMailWithSES = mkEnableOption "SES";

  config = mkIf config.local.sendOutgoingMailWithSES {
    networking.defaultMailServer = {
      hostName = "email-smtp.us-west-2.amazonaws.com:587";
      directDelivery = true;
      domain = "${config.networking.hostName}.eraserhead.net";
      useTLS = true;
      useSTARTTLS = true;
      root = "jason.m.felice@gmail.com";
    } // import ./ses-user.nix;
  };
}
