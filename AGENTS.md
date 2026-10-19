# Jason's public dotfiles Nix flake

- `common.nix` - includes everything.
- `tools/` - individual programs w/config.
- `kits/` - bundles of tools for a topic (e.g. CNC, 3D printing).
- `shell/` - utils, functions, and shell config.
- `hosts/` - public and personal hosts.
- `~/src/git.drwholdings.com/jfelice/dotfiles/` - on my Mac work laptop
  (`chlmp-jfelice1`), is private dotfiles for work, which uses this flake.
  - Other systems build this flake directly; the work dotfiles point
    `local.dotfilesPath` at themselves, so the `:r` shell function can build and
    reload them.  You might not be able to use the
    shell function, but you can emulate it.  It is defined in `shell/default.nix`.
  - `:r` builds the work dotfiles, whose `public-dotfiles` input is
    `github:eraserhd/dotfiles` pinned in its `flake.lock`, so local edits here
    are invisible to it.  To build uncommitted work, override the input:

        cd ~/src/git.drwholdings.com/jfelice/dotfiles && darwin-rebuild build --flake . --show-trace \
          --override-input public-dotfiles path:$HOME/src/github.com/eraserhd/dotfiles

  - Stop at `build`.  The switch needs `sudo` and `:r`'s last line is `exec zsh -l`;
    neither works from a tool call, so leave switching to me.
- Prefer home-manager to nix-darwin/NixOS config where it won't break things, e.g.
  installing packages used for development.
