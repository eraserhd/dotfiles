{ ... }:

{
  config = {
    nixpkgs.overlays = [
      (final: prev: {
        local = (prev.local or {}) // {
          git-scripts = prev.callPackage ./git-scripts {};
          kak-scrollback-pager = prev.callPackage ./kak-scrollback-pager {};
          monitor-follows-mouse = prev.callPackage ./monitor-follows-mouse {};
          sigils = prev.callPackage ./sigils {};
          wake-watcher = prev.callPackage ./wake-watcher {};
        };

        # clipboard-pluggo's derivation.nix still asks for libX11, and reaching
        # it through nixpkgs' xorg set warns on every evaluation.  Hand it the
        # renamed package instead.  Drop this once clipboard-pluggo takes
        # libx11.  -- 2026-10-24
        libX11 = final.libx11;
      })
    ];
  };
}
