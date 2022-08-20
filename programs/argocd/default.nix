{ pkgs, ... }:

{
  config = {
    environment.systemPackages = with pkgs; [
      argocd
    ];
  };
}
