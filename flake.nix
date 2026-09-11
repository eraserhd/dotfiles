{
  description = "eraserhd's machine configs";

  inputs = {
    add-missing.url = "github:eraserhd/add-missing";
    add-missing.inputs.nixpkgs.follows = "nixpkgs";
    agenix.url = "github:ryantm/agenix";
    agenix.inputs.nixpkgs.follows = "nixpkgs";
    agenix.inputs.darwin.follows = "darwin";
    agenix.inputs.home-manager.follows = "home-manager";
    bCNC-nix.url = "github:eraserhd/bCNC-nix";
    bCNC-nix.inputs.nixpkgs.follows = "nixpkgs";
    claude-desktop.url = "github:aaddrick/claude-desktop-debian";
    claude-desktop.inputs.nixpkgs.follows = "nixpkgs";
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
  , agenix
  , bCNC-nix
  , claude-desktop
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
        claude-desktop.overlays.default
        kak-ansi.overlays.default
        kak-babashka.overlays.default
        parinfer-rust.overlays.default
      ];
    };

    darwinModules = rec {
      dotfiles = {
        imports = [
          ./os/nix-darwin
          ./common.nix
          home-manager.darwinModules.home-manager
          overlays
          plugbench.darwinModules.default
          agenix.darwinModules.default
        ];
      };
      default = dotfiles;
    };

    nixosModules = rec {
      dotfiles = {
        imports = [
          ./os/nixos
          ./common.nix
          home-manager.nixosModules.home-manager
          overlays
          plugbench.nixosModules.default
          agenix.nixosModules.default
        ];
      };
      default = dotfiles;
    };
  in {
    darwinConfigurations."chlmp-jfelice1" = darwin.lib.darwinSystem rec {
      system = "aarch64-darwin";
      modules = [
        darwinModules.dotfiles
        ./hosts/chlmp-jfelice1
        { environment.systemPackages = [ agenix.packages.${system}.default ]; }
      ];
      specialArgs = { inherit inputs; };
    };

    nixosConfigurations.crunch = nixpkgs.lib.nixosSystem rec {
      system = "x86_64-linux";
      modules = [
        nixosModules.dotfiles
        ./hosts/crunch
        { environment.systemPackages = [ agenix.packages.${system}.default ]; }
      ];
      specialArgs = { inherit inputs; };
    };

    nixosConfigurations.cnc = nixpkgs.lib.nixosSystem rec {
      system = "aarch64-linux";
      modules = [
        raspberry-pi-nix.nixosModules.raspberry-pi
        raspberry-pi-nix.nixosModules.sd-image
        nixosModules.dotfiles
        ./hosts/cnc
        { environment.systemPackages = [ agenix.packages.${system}.default ]; }
      ];
      specialArgs = { inherit inputs; };
    };

    inherit darwinModules nixosModules;
  };
}
