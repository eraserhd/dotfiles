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
  };
}
