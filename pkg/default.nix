{ ... }:

{
  config = {
    nixpkgs.overlays = [
      (final: prev: {
        local = (prev.local or {}) // {
          git-scripts = prev.callPackage ./git-scripts {};
        };
        bCNC = prev.callPackage ./bCNC {};
      })
    ];
  };
}
