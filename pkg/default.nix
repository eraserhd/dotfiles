{ ... }:

{
  config = {
    nixpkgs.overlays = [
      (final: prev: {
        local = (prev.local or {}) // {
          git-scripts = prev.callPackage ./git-scripts {};
          kak-scrollback-pager = prev.callPackage ./kak-scrollback-pager {};
          monitor-follows-mouse = prev.callPackage ./monitor-follows-mouse {};
          post-keystroke = prev.callPackage ./post-keystroke {};
          sigils = prev.callPackage ./sigils {};
        };
        bCNC = prev.callPackage ./bCNC {};
      })
    ];
  };
}
