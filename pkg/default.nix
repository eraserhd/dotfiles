{ ... }:

{
  config = {
    nixpkgs.overlays = [
      (self: super: {
        local = (super.local or {}) // {
          git-scripts = super.callPackage ./git-scripts {};
        };
        nexus-tools = super.callPackage ./nexus-tools {};
        open-in-chrome-tab = super.callPackage ./open-in-chrome-tab {};
      })
    ];
  };
}
