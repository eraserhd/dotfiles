{ ... }:

{
  config = {
    nixpkgs.overlays = [
      (final: prev: {
        local = (prev.local or {}) // {
          git-scripts = prev.callPackage ./git-scripts {};
          kak-scrollback-pager = prev.callPackage ./kak-scrollback-pager {};
          monitor-follows-mouse = prev.callPackage ./monitor-follows-mouse {};
          wake-watcher = prev.callPackage ./wake-watcher {};
        };
      })
    ];
  };
}
