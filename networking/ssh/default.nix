{ lib, ... }:

with lib;
{
  options = {
    local.authorizedKeys = mkOption {
      description = "List of public SSH keys to allow for me";
      type = types.listOf types.str;
    };
  };

  config = {
    local.authorizedKeys = [
      (builtins.readFile ./files/id_rsa.pub)
      (builtins.readFile ./files/id_dsa.pub)
      (builtins.readFile ./files/id_rsa-workingcopy.pub)
      (builtins.readFile ./files/id_rsa-terminus-iphone.pub)
      ("COMMAND=\"#{pkgs.coreutils}/bin/false\" " + (builtins.readFile ./files/id_rsa-macbook.pub))
    ];
  };
}
