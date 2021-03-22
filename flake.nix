{
  description = "eraserhd's machine configs";

  inputs = {
    nixpkgs.url = "github:eraserhd/nixpkgs/dogfood";
    darwin.url = "github:lnl7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    twou.url = "git+ssh://git@github.com/2uinc/nix-2u?ref=develop";
    twou.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs  = { self, nixpkgs, darwin, home-manager, twou }: {
    darwinConfigurations."C02CW0J5ML87" = darwin.lib.darwinSystem {
      modules = [
        ./machines/macbook
        ./modules/nix-darwin
        home-manager.darwinModules.home-manager
        {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
        }
        {
          nix.nixPath = {
            inherit nixpkgs darwin;
          };
        }
        twou.darwinModules
      ];
    };

    nixosConfigurations.crunch = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        ./machines/crunch
        ./modules/nixos
        home-manager.nixosModules.home-manager
        {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
        }
        twou.nixosModules
      ];
    };
  };
}
