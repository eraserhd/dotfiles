{ lib, config, options, pkgs, ... }:

with lib;
let
  cfg = config.local.sendOutgoingMailWithSES;
in {
  options = {
    local.sendOutgoingMailWithSES.enable = mkEnableOption "SES";
  };

  config = mkIf cfg.enable
  (if (builtins.hasAttr "msmtp" options.programs)
  then {
    age.secrets."ses-smtp-password".file = ./password.age;

    programs.msmtp = {
      enable = true;
      accounts.default = {
        auth = true;
        user = "AKIATJ6VYKJDVEPD7C75";
        # msmtp runs this each time it sends, so the password stays out of
        # both the store and /etc/msmtprc.
        passwordeval = "${pkgs.coreutils}/bin/cat ${config.age.secrets."ses-smtp-password".path}";
        host = "email-smtp.us-west-2.amazonaws.com";
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
