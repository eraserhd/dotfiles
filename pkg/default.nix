{ ... }: {
  config = {
    nixpkgs.overlays = [
      (self: super: {
        open-in-chrome-tab = super.callPackage ./open-in-chrome-tab {};
      })
    ];
  };
}
