{ config, lib, pkgs, ... }:

with lib;
{
  config = mkMerge [
    {
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
      ];

      home-manager.users.jfelice = { pkgs, ... }: {
        home.file.".clojure/deps.edn".source = ./deps.edn;
        home.file.".lein/profiles.clj".source = ./profiles.clj;
      };
    }
    #(mkIf (!pkgs.stdenv.isDarwin) {
    #  environment.systemPackages = with pkgs; [
    #    clj-kondo
    #    rep
    #  ];
    #})
  ];
}
