{
  description = "eraserhd's machine configs";

  inputs = {
    nixpkgs.url = "github:eraserhd/nixpkgs/podman-gvproxy-path";
    darwin.url = "github:LnL7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = "github:nix-community/home-manager/master";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    twou.url = "git+ssh://git@github.com/2uinc/nix-2u?ref=develop";
    twou.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs  = { self, nixpkgs, darwin, home-manager, twou }: {
    darwinConfigurations."C02CW0J5ML87" = darwin.lib.darwinSystem {
      system = "x86_64-darwin";
      modules = [
        ./os/nix-darwin
        ./machines/macbook
        ./common.nix
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
        ./os/nixos
        ./machines/crunch
        ./common.nix
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
