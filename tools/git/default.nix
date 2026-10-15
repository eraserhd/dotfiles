{ config, pkgs, ... }:

let
  # gh pipes the diff (in colour) to this in gh-dash's full-screen view.
  kakDiffPager = pkgs.writeShellApplication {
    name = "kak-diff-pager";
    runtimeInputs = [ pkgs.coreutils pkgs.gnused pkgs.git ];
    text = ''
      diff=$(mktemp -t gh-diff.XXXXXX)
      trap 'rm -f "$diff"' EXIT
      sed 's/\x1b\[[0-9;]*m//g' >"$diff"

      # So that plumbing a file in the diff finds it.
      GH_DIFF_ROOT=$(git rev-parse --show-toplevel 2>/dev/null || pwd)
      export GH_DIFF_ROOT
      kak -e 'set-option buffer filetype diff; set-option buffer plumb_wdir %env{GH_DIFF_ROOT}' "$diff"
    '';
  };
in {
  config = {
    environment.systemPackages = with pkgs; [
      gitFull
      gh
      local.git-scripts
      git-revise
    ];

    home-manager.users.jfelice = { pkgs, ... }: {
      programs.gh-dash = {
        enable = true;
        settings.pager.diff = "${kakDiffPager}/bin/kak-diff-pager";
      };
      # Replaces the config which gh-dash wrote for itself.
      xdg.configFile."gh-dash/config.yml".force = true;

      home.file.".gitconfig".source = ./gitconfig;
      home.file.".config/git/ignore".text = ''
        .DS_Store
        .*.swp
        tags
        build-deps
        TAGS
      '';
    };
  };
}
