{ ... }:

{
  config = {
    nixpkgs.overlays = [
      (self: super: {
        local = (super.local or {}) // {
          git-scripts = super.callPackage ./git-scripts {};
        };
        open-in-chrome-tab = super.callPackage ./open-in-chrome-tab {};

        k3s = super.callPackage ./k3s {
          inherit (super) k3s;
        };
      })
    ];
  };
}
