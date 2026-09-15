{ pkgs, ... }:

{
  environment.systemPackages = [ pkgs.beam27Packages.elixir-ls ];
}
