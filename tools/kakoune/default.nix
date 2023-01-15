{ config, options, pkgs, lib, ... }:

with lib;
{
  config = {
    nixpkgs.overlays = [
      (self: super: {
        kakoune = super.kakoune.override {
          plugins = with pkgs.kakounePlugins; [
            case-kak
            kak-ansi
            kak-fzf
            kak-jira
            kak-lsp
            openscad-kak
            parinfer-rust
            quickscope-kak
            rep
          ];
        };
        kakouneWrapper = super.callPackage ./wrapper {};
      })
    ];

    environment.systemPackages = with pkgs; [
      python310Packages.editorconfig
      kakouneWrapper
      kak-lsp
    ];

    environment.variables.EDITOR = "${pkgs.kakouneWrapper}/bin/kak";
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
