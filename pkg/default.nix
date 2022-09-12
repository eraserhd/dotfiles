{ ... }:

{
  config = {
    nixpkgs.overlays = [
      (self: super: {
        local = (super.local or {}) // {
          git-scripts = super.callPackage ./git-scripts {};
        };
        nats-plumber = super.callPackage ./nats-plumber {
          inherit (self.darwin.apple_sdk.frameworks) Cocoa;
        };
        open-in-chrome-tab = super.callPackage ./open-in-chrome-tab {};
      })
    ];
  };
}
