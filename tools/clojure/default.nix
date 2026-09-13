{ config, lib, pkgs, ... }:

with lib;
{
  config = {
    environment.systemPackages = with pkgs; [
      clojure
      leiningen
      parinfer-rust
      rep
    ];

    home-manager.users.jfelice = { pkgs, config, ... }: {
      home.file.".clojure/deps.edn".source = ./deps.edn;
      age.secrets."lein-profiles.clj" = {
        file = ./profiles.clj.age;
        path = "${config.home.homeDirectory}/.lein/profiles.clj";
      };
    };
  };
}
