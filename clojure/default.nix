{ config, pkgs, ... }:

{
  config = {
    nixpkgs.overlays = [
      (self: super: {
        jdk = super.jdk11;
      })
    ];

    environment.systemPackages = with pkgs; [
      clojure
      leiningen
      parinfer-rust
    ];
  };
}
