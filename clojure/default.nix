{ config, lib, pkgs, ... }:

with lib;
{
  config = {
    nixpkgs.overlays = [
      (self: super: {
        jdk = super.jdk11;

        # fop doesn't like my jdk11 override
        fop = (super.fop.override {
          jdk = self.jdk8;
        });
      })
    ];

    environment.systemPackages = with pkgs; [
      clojure
      leiningen
      parinfer-rust
      rep
    ];

    home-manager.users.jfelice = { pkgs, ... }: {
      home.file.".clojure/deps.edn".source = ./deps.edn;
      home.file.".lein/profiles.clj".source = ./profiles.clj;
    };
  };
}
