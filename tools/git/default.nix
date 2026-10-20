{ config, pkgs, ... }:

let
  privateShPath = config.home-manager.users.jfelice.age.secrets."private.sh".path;

  # Only shells which have sourced private.sh have GITHUB_TOKEN, so this gives
  # github.com the personal token from it everywhere else, unless something
  # says otherwise.
  gh = pkgs.symlinkJoin {
    name = "gh-with-personal-token";
    paths = [ pkgs.gh ];
    postBuild = ''
      rm $out/bin/gh
      cat >$out/bin/gh <<'EOF'
      #!${pkgs.runtimeShell}
      if [ -z "''${GH_TOKEN:-}''${GITHUB_TOKEN:-}" ] && [ -r "${privateShPath}" ]; then
        GITHUB_TOKEN=$(. "${privateShPath}" && printf '%s' "''${GITHUB_TOKEN:-}")
        export GITHUB_TOKEN
      fi
      exec ${pkgs.gh}/bin/gh "$@"
      EOF
      chmod +x $out/bin/gh
    '';
  };

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
