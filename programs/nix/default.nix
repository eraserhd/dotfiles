{ pkgs, ... }:

{
  config = {
    nix.package = pkgs.nixFlakes;
    nix.extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };
}
