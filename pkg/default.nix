{ ... }:

{
  config = {
    nixpkgs.overlays = [
      (final: prev: {
        local = (prev.local or {}) // {
          git-scripts = prev.callPackage ./git-scripts {};
          kak-scrollback-pager = prev.callPackage ./kak-scrollback-pager {};
          sigils = prev.callPackage ./sigils {};
        };
        bCNC = prev.callPackage ./bCNC {};
      })
    ];
  };
}
