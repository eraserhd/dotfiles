{ lib, config, options, ... }:

with lib;
let
  cfg = config.local.sendOutgoingMailWithSES;
in {
  options = {
    local.sendOutgoingMailWithSES.enable = mkEnableOption "SES";
  };

  config = mkIf cfg.enable
  (if (builtins.hasAttr "msmtp" options.services)
  then {
    services.msmtp = {
      enable = true;
      accounts.default = {
        auth = true;
        user = "AKIATJ6VYKJDVEPD7C75";
        password = builtins.readFile ./password;
        host = "email-smtp.us-west-2.amazonaws.com:587";
        domain = "${config.networking.hostName}.${config.networking.domain}";
        port = 587;
        tls = true;
        tls_starttls = true;
        from = "jason.m.felice@gmail.com";
      };
    };
  }
  else {
  });
}
