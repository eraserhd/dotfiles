{
  description = "eraserhd's machine configs";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/master";
    darwin.url = "github:LnL7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = "github:nix-community/home-manager/master";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    twou.url = "git+ssh://git@github.com/2uinc/nix-2u?ref=main";
    twou.inputs.nixpkgs.follows = "nixpkgs";
    add-missing.url = "github:eraserhd/add-missing";
    add-missing.inputs.nixpkgs.follows = "nixpkgs";
    kak-ansi.url = "github:eraserhd/kak-ansi";
    kak-ansi.inputs.nixpkgs.follows = "nixpkgs";
    plugbench.url = "github:plugbench/nix-plugbench";
    plugbench.inputs.nixpkgs.follows = "nixpkgs";
    raspberry-pi-nix.url = "github:nix-community/raspberry-pi-nix";
    raspberry-pi-nix.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs =
  { self
  , nixpkgs
  , darwin
  , home-manager
  , twou
  , add-missing
  , kak-ansi
  , plugbench
  , raspberry-pi-nix
  }@inputs:
    let
      homeManagerConfig = {
        home-manager.useGlobalPkgs = true;
        home-manager.useUserPackages = true;
      };

    in {
      darwinConfigurations."V3Q9GYKM9C" = darwin.lib.darwinSystem {
        system = "aarch64-darwin";
        modules = [
          ./os/nix-darwin
          ./machines/macbook
          ./common.nix
          home-manager.darwinModules.home-manager
          homeManagerConfig
          {
            nixpkgs.overlays = [
              add-missing.overlays.default
              kak-ansi.overlays.default
            ];
          }
          twou.darwinModules.default
          plugbench.darwinModules.default
        ];
        specialArgs = { inherit inputs; };
      };

      nixosConfigurations.crunch = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./os/nixos
          ./machines/crunch
          ./common.nix
          home-manager.nixosModules.home-manager
          homeManagerConfig
          {
            nixpkgs.overlays = [
              add-missing.overlays.default
              kak-ansi.overlays.default
            ];
          }
          twou.nixosModules.default
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
          ./machines/cnc
          ./common.nix
          home-manager.nixosModules.home-manager
          homeManagerConfig
          {
            nixpkgs.overlays = [
              add-missing.overlays.default
              kak-ansi.overlays.default
            ];
          }
          plugbench.nixosModules.default
        ];
        specialArgs = { inherit inputs; };
      };

    };
}
