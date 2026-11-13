# Jason's public dotfiles Nix flake

- `common.nix` - includes everything.
- `tools/` - individual programs w/config.
- `kits/` - bundles of tools for a topic (e.g. CNC, 3D printing).
- `shell/` - utils, functions, and shell config.
- `hosts/` - public and personal hosts.
- `pkg/` - small packages that are meant to go into the path.  Not for scripts
  that can be called through a derivation reference to automate things.
- Prefer home-manager to nix-darwin/NixOS config where it won't break things, e.g.
  installing packages used for development.
- Do not set force = true for home-manager overwrites, or choose mutable config
  changes merely to make the next switch work.  I'd rather deal with the one-time
  effort.
- Bash and babashka (bb, Clojure) are preferred for scripting.

## Machine $ROOT Flake

- `~/src/git.drwholdings.com/jfelice/dotfiles/` on my Mac work laptop
  (`chlmp-jfelice1`), is private dotfiles for work, which uses this flake.
- Other systems build this flake directly.

## Building and Updating Local Machine Config

Build before switching.  Only switch when user approves or is AFK (it can
move windows and steal focus).

### Mac OS

    TERM=xterm darwin-rebuild build --flake $ROOT --show-trace     # build
    TERM=xterm VERBOSE=1 sudo darwin-rebuild switch --flake $ROOT  # switch

### NixOS

    nixos-rebuild build --flake $ROOT                              # build
    nixos-rebuild switch --elevate=sudo --flake $ROOT              # switch

## Displaying Machine Config Status

Display machine config status after accomplishing any task involving modifying
the local system's machine config or any dependent flakes or repos.

Show an ASCII-art tree starting from $ROOT with just enough depth to show
what needs to be pushed, pulled/updated, repinned, is dirty, or needs a PR.
Show which have local overrides.
Repos under `github.com/eraserhd` or `git.drwholdings.com/jfelice` never need
PRs since we push directly to the default branch.
