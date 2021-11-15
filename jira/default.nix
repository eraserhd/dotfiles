{ pkgs, ... }:

{
  config = {
    nixpkgs.overlays = [
      (self: super: {
        jira-link = super.callPackage ./jira-link {};
      })
    ];

    environment.systemPackages = [ pkgs.jira-link ];
  };
}
