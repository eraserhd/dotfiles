{ config, options, pkgs, lib, ... }:

with lib;
{
  config = {
    nixpkgs.overlays = [
      (self: super: {
        kakoune = super.callPackage ./wrapper {
          kakoune = super.kakoune.override {
            plugins = with pkgs.kakounePlugins; [
              case-kak
              kak-ansi
              kak-babashka
              kak-jira
              kakoune-lsp
              openscad-kak
              parinfer-rust
              rep
            ];
          };
        };
      })
    ];

    environment.systemPackages = with pkgs; [
      python310Packages.editorconfig
      kakoune
      kakoune-lsp
    ];

    environment.variables.EDITOR = "${pkgs.kakoune}/bin/kak";
    environment.interactiveShellInit = ''
      man() {
          if (( $# == 2 )); then
              kak -e "man $2($1)"
              return $?
          else
              kak -e "man $*"
              return $?
          fi
      }
    '';

    home-manager.users.jfelice = { pkgs, ... }: {
      home.file.".config/kak/kakrc".source = ./kakrc;
    };
  };
}
