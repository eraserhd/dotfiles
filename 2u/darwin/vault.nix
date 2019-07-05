{ lib, pkgs, config, ... }:

with lib;
let
  cfg = config.services."2u".vault;
  opsTerraformSource = fetchGit {
    url = ssh://git@github.com/2uinc/ops-terraform;
    rev = "37584aa8acfaff1ea3620f83cbdc79152672a13b";
  };
in {
  options = {
    services."2u".vault.enable = mkEnableOption "2U Vault";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [ pkgs.vault ];
    environment.variables = {
      VAULT_ADDR   = "https://vault.prod.2u.com:8200/";
      VAULT_CACERT = "${opsTerraformSource}/consul-vpc/policies/vault-ca.pem";
    };
  };
}
