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

    home-manager.users.jfelice = { pkgs, ... }: {
      home.file.".clojure/deps.edn".source = ./deps.edn;
      home.file.".lein/profiles.clj".source = ./profiles.clj;
    };
  };
}
