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
  };

  outputs  = { self, nixpkgs, darwin, home-manager, twou, add-missing, kak-ansi }:
    let
      homeManagerConfig = {
        home-manager.useGlobalPkgs = true;
        home-manager.useUserPackages = true;
      };
    in {
      darwinConfigurations."C02CW0J5ML87" = darwin.lib.darwinSystem {
        system = "x86_64-darwin";
        modules = [
          ./os/nix-darwin
          ./machines/macbook
          ./common.nix
          home-manager.darwinModules.home-manager
          homeManagerConfig
          {
            nix.nixPath = {
              inherit nixpkgs darwin;
            };
          }
          {
            nixpkgs.overlays = [
              add-missing.overlays.default
              kak-ansi.overlays.default
            ];
          }
          twou.darwinModules.default
        ];
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
            nix.nixPath = [
              "nixpkgs=${nixpkgs}"
            ];
          }
          {
            nixpkgs.overlays = [
              add-missing.overlays.default
              kak-ansi.overlays.default
            ];
          }
          twou.nixosModules.default
        ];
      };
    };
}
