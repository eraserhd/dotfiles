{
  description = "eraserhd's machine configs";

  inputs = {
    add-missing.url = "github:eraserhd/add-missing";
    add-missing.inputs.nixpkgs.follows = "nixpkgs";
    bCNC-nix.url = "github:eraserhd/bCNC-nix";
    bCNC-nix.inputs.nixpkgs.follows = "nixpkgs";
    darwin.url = "github:LnL7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = "github:nix-community/home-manager/master";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    kak-ansi.url = "github:eraserhd/kak-ansi";
    kak-ansi.inputs.nixpkgs.follows = "nixpkgs";
    kak-babashka.url = "github:eraserhd/kak-babashka";
    kak-babashka.inputs.nixpkgs.follows = "nixpkgs";
    nixpkgs.url = "github:NixOS/nixpkgs/master";
    parinfer-rust.url = "github:eraserhd/parinfer-rust";
    parinfer-rust.inputs.nixpkgs.follows = "nixpkgs";
    plugbench.url = "github:plugbench/nix-plugbench";
    plugbench.inputs.nixpkgs.follows = "nixpkgs";
    raspberry-pi-nix.url = "github:nix-community/raspberry-pi-nix";
    raspberry-pi-nix.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs =
  { self
  , add-missing
  , bCNC-nix
  , darwin
  , home-manager
  , kak-ansi
  , kak-babashka
  , nixpkgs
  , parinfer-rust
  , plugbench
  , raspberry-pi-nix
  }@inputs:
  let
    overlays = {
      nixpkgs.overlays = [
        add-missing.overlays.default
        bCNC-nix.overlays.default
        kak-ansi.overlays.default
        kak-babashka.overlays.default
        parinfer-rust.overlays.default
      ];
    };
  in {
    darwinConfigurations."V3Q9GYKM9C" = darwin.lib.darwinSystem {
      system = "aarch64-darwin";
      modules = [
        ./os/nix-darwin
        ./hosts/V3Q9GYKM9C
        ./common.nix
        home-manager.darwinModules.home-manager
        overlays
        plugbench.darwinModules.default
      ];
      specialArgs = { inherit inputs; };
    };

    nixosConfigurations.crunch = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        ./os/nixos
        ./hosts/crunch
        ./common.nix
        home-manager.nixosModules.home-manager
        overlays
        plugbench.nixosModules.default
      ];
      specialArgs = { inherit inputs; };
    };

    nixosConfigurations.cnc = nixpkgs.lib.nixosSystem {
      system = "aarch64-linux";
      modules = [
        raspberry-pi-nix.nixosModules.raspberry-pi
        raspberry-pi-nix.nixosModules.sd-image
        ./os/nixos
        ./hosts/cnc
        ./common.nix
        home-manager.nixosModules.home-manager
        overlays
        plugbench.nixosModules.default
      ];
      specialArgs = { inherit inputs; };
    };
  };
}
