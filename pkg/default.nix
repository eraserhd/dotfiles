{ ... }:

{
  config = {
    nixpkgs.overlays = [
      (final: prev: {
        local = (prev.local or {}) // {
          git-scripts = prev.callPackage ./git-scripts {};
        };
        open-in-chrome-tab = prev.callPackage ./open-in-chrome-tab {};

        bCNC = prev.callPackage ./bCNC {};
      })
    ];
  };
}
