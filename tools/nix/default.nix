{ pkgs, ... }:

{
  config = {
    nix.extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };
}
