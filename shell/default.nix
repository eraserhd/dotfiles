{ options, config, lib, pkgs, ... }:

with lib;
let
  cfg = config.local.loginShell;

  homeDirectory = config.users.users.jfelice.home;

  privateShPath = config.home-manager.users.jfelice.age.secrets."private.sh".path;
in {
  options = {
    local.loginShell.package = mkOption {
      type = types.package;
      default = pkgs.bashInteractive;
      description = ''
        Package containing user shell.
      '';
      example = "pkgs.bashInteractive";
    };

    local.dotfilesPath = mkOption {
      type = types.str;
      default = "${homeDirectory}/src/github.com/eraserhd/dotfiles";
      description = ''
        Path to the flake that `:r` builds.
      '';
    };
  };

  config = {
    home-manager.users.jfelice = { pkgs, config, ... }: {
      age.identityPaths = [ "${config.home.homeDirectory}/.ssh/id_ed25519" ];
      age.secrets."private.sh" = {
        file = ./private.sh.age;
      };
    };
    environment.interactiveShellInit = ''
      :r() {
        local readlink_bin="${pkgs.coreutils}/bin/readlink"
        if command -v darwin-rebuild >/dev/null; then
          pushd ${config.local.dotfilesPath} >/dev/null
          TERM=xterm darwin-rebuild build --flake ${config.local.dotfilesPath} --show-trace || return $?
          if [ "$($readlink_bin -f /run/current-system)" != "$($readlink_bin -f ./result)" ] || [ "$1" = "--force" ]; then
            TERM=xterm VERBOSE=1 sudo darwin-rebuild switch --flake ${config.local.dotfilesPath} || return $?
          fi
          popd >/dev/null
        elif command -v nixos-rebuild >/dev/null; then
          pushd ${config.local.dotfilesPath} >/dev/null
          nixos-rebuild build --flake ${config.local.dotfilesPath} || return $?
          if [ "$($readlink_bin -f /run/current-system)" != "$($readlink_bin -f ./result)" ] || [ "$1" = "--force" ]; then
            nixos-rebuild switch --elevate=sudo --flake ${config.local.dotfilesPath} || return $?
          fi
          popd >/dev/null
        fi
        unset __NIX_DARWIN_SET_ENVIRONMENT_DONE __NIXOS_SET_ENVIRONMENT_DONE
        unset __ETC_BASHRC_SOURCED __ETC_PROFILE_SOURCED __ETC_PROFILE_DONE
        unset __ETC_ZSHENV_SOURCED __ETC_ZSHRC_SOURCED
        exec '${pkgs.zsh}${pkgs.zsh.shellPath}' -l
      }

      source_if_exists() {
        if [ -f "$1" ]
        then
          source "$1"
        fi
      }

      source_if_exists ~/.nix-profile/etc/profile.d/nix.sh
      source_if_exists ${privateShPath}

      # So nix doesn't hit GitHub's rate limit for anonymous requests.
      if [ -n "''${GITHUB_TOKEN:-}" ]; then
        export NIX_CONFIG="''${NIX_CONFIG:+$NIX_CONFIG
      }access-tokens = github.com=$GITHUB_TOKEN"
      fi

      alias k=kubectl
    '';

    environment.variables = {
      CLICOLOR = "1";
    };

    environment.systemPackages = with pkgs; [
      add-missing
      bat
      direnv
      entr
      file
      fzf
      jq
      killall
      man-pages-posix
      nix-prefetch-github
      shellcheck
      tableize
      tree
      unzip
      wget
      zip
    ];
  } // (if (builtins.hasAttr "defaultUserShell" options.users)
  then {
    users.defaultUserShell = cfg.package;
    documentation.man.cache.enable = true;
  }
  else {
  });
}
