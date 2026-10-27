# Jason's public dotfiles Nix flake

- `common.nix` - includes everything.
- `tools/` - individual programs w/config.
- `kits/` - bundles of tools for a topic (e.g. CNC, 3D printing).
- `shell/` - utils, functions, and shell config.
- `hosts/` - public and personal hosts.
- `pkg/` - small packages that are meant to go into the path.  Not for scripts
  that can be called through a derivation reference to automate things.
- `~/src/git.drwholdings.com/jfelice/dotfiles/` - on my Mac work laptop
  (`chlmp-jfelice1`), is private dotfiles for work, which uses this flake.
  - Other systems build this flake directly; the work dotfiles point
    `local.dotfilesPath` at themselves, so the `:r` shell function can build and
    reload them.  You might not be able to use the
    shell function, but you can emulate it.  It is defined in `shell/default.nix`.
  - `:r` builds the work dotfiles, whose `public-dotfiles` input is
    `github:eraserhd/dotfiles` pinned in its `flake.lock`.
  - Stop at `build`.  The switch needs `sudo` and `:r`'s last line is `exec zsh -l`;
    neither works from a tool call, so leave switching to me.
- Prefer home-manager to nix-darwin/NixOS config where it won't break things, e.g.
  installing packages used for development.
- Do not set force = true for home-manager overwrites, or choose mutable config
  changes merely to make the next switch work.  I'd rather deal with the one-time
  effort.
- Bash and babashka (bb, Clojure) are preferred for scripting.

## Building and Updating Local Machine Config

When you commit here, modify `flake.lock` in `~/src/git.drwholdings.com/jfelice/dotfiles`
by commands, so that `:r` gets the commits:

- It is OK to update a dirty worktree or a modified `flake.lock`.
- Run `nix flake lock --override-input <input> <url>` to point to forks or local copies
  for anything not yet accepted.  Prefer forks to local copies when available.
- Check remotes and PRs, and undo any local pins of inputs that are available
  in the remotes.
- Leave `flake.lock` uncommitted.
- Build the config when you are done.

When you are done working, report any local overrides and what we need to do or are waiting for.
e.g. "DO NOT COMMIT AFTER TESTING, we are still waiting for:" and a list or,
"It is OK to commit after testing."
