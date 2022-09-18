{ ... }:

{
  config = {
    nixpkgs.overlays = [
      (self: super: {
        local = (super.local or {}) // {
          git-scripts = super.callPackage ./git-scripts {};
        };
        nats-clipboard = super.callPackage ./nats-clipboard {
          inherit (self.darwin.apple_sdk.frameworks) Cocoa;
          inherit (self.xorg) libX11;
        };
        open-in-chrome-tab = super.callPackage ./open-in-chrome-tab {};
      })
    ];
  };
}
