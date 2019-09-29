{ lib, pkgs, config, ... }:

with lib;
let
  cfg = config.local.nagiosServer;

  objects = pkgs.writeText "objects.cfg" ''
    ${cfg.objects}
  '';
in {
  options = {
    local.nagiosServer.enable = mkEnableOption "Nagios";

    local.nagiosServer.objects = mkOption {
      type = types.lines;
      default = "";
      example = ''
        define host {
          host_name foo
          # ...
        }
      '';
      description = "Nagios object configuration";
    };
  };

  config = mkIf cfg.enable {
    services.nagios = {
      enable = true;
      objectDefs = [ (toString objects) ];
    };

    local.nagiosServer.objects = ''
      define command {
          command_name host-notify-by-email
          command_line printf "%b" "Subject: $NOTIFICATIONTYPE$ host $HOSTNAME$ is $HOSTSTATE$\n\nInfo: $HOSTOUTPUT\nDate/Time: $LONGDATETIME$\n" |sendmail -vt $CONTACTEMAIL$
      }
      define command {
          command_name service-notify-by-email
          command_line printf "%b" "Subject: $NOTIFICATIONTYPE$ service $HOSTALIAS$/$SERVICEDESC$ is $SERVICESTATE$\n\nDate/Time: $LONGDATETIME$\n\n$SERVICEOUTPUT$\n" |sendmail -vt $CONTACTEMAIL$
      }
      define timeperiod {
          timeperiod_name 24x7
          alias           All the time
          sunday          0:00-24:00
          monday          0:00-24:00
          tuesday         0:00-24:00
          wednesday       0:00-24:00
          thursday        0:00-24:00
          friday          0:00-24:00
          saturday        0:00-24:00
      }
      define contact {
          contact_name                  jfelice
          host_notifications_enabled    1
          service_notifications_enabled 1
          service_notification_period   24x7
          host_notification_period      24x7
          service_notification_options  w,u,c,r,f
          host_notification_options     d,u,r,f
          service_notification_commands service-notify-by-email
          host_notification_commands    host-notify-by-email
          email                         jason.m.felice@gmail.com
          can_submit_commands           1
      }
      define command {
          command_name check_ssh
          command_line check_ssh -H $HOSTADDRESS$
      }
      define host {
          host_name             crunch
          max_check_attempts    5
          check_period          24x7
          contacts              jfelice
          notification_interval 30
          notification_period   24x7
      }
      define service {
          host_name             crunch
          service_description   ssh-443
          check_period          24x7
          check_command         check_ssh
          check_interval        5
          retry_interval        3
          max_check_attempts    5
          notification_period   24x7
          notification_interval 30
          contacts              jfelice
      }
    '';
  };
}
