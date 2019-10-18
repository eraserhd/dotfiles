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

    home-manager.users.jfelice = { pkgs, ... }: {
      home.file.".clojure/deps.edn".source = ./deps.edn;
      home.file.".lein/profiles.clj".source = ./profiles.clj;
    };
  };
}
