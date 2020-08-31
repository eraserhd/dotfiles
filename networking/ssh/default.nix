{ lib, ... }:

with lib;
{
  options = {
    local.authorizedKeys = mkOption {
      description = "List of public SSH keys to allow for users";
      type = types.attrsOf (types.listOf types.str);
    };
  };

  config = {
    local.authorizedKeys = {
      alex = [
        (builtins.readFile ./files/id_rsa.pub-alex)
      ];
      jfelice = [
        (builtins.readFile ./files/id_rsa.pub)
        (builtins.readFile ./files/id_dsa.pub)
        (builtins.readFile ./files/id_rsa-backup.pub)
        (builtins.readFile ./files/id_rsa-workingcopy.pub)
        (builtins.readFile ./files/id_rsa-terminus-iphone.pub)
        ("COMMAND=\"#{pkgs.coreutils}/bin/false\" " + (builtins.readFile ./files/id_rsa-macbook.pub))
      ];
    };

    home-manager.users.jfelice = { pkgs, ... }: {
      # .profile is sourced by Xsession, I'm told
      home.file.".profile".text = ''
        isSshAgentAlive() {
          if [ -z "$SSH_AGENT_PID" ]; then
            return 1
          fi
          ps -p "$SSH_AGENT_PID" |grep -q ssh-agent
        }

        ensureSshAgent() {
          if ! isSshAgentAlive; then
            eval "$(ssh-agent)"
          fi
        }

        case "$0" in
        *Xsession) ensureSshAgent;;
        esac
      '';
    };
  } // (if (builtins.hasAttr "launchd" options)
  then {
    system.activationScripts.extraUserActivation.text = ''
      mkdir -p ~/.ssh
      chmod 700 ~/.ssh

      cp -ap ${toString ./files}/* ~/.ssh/
      chmod 600 ~/.ssh/id_*
    '';
  }
  else {
    # FIXME: NixOS activation as above
  });
}
