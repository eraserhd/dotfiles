{ pkgs, ... }:

{
  environment.systemPackages = [ pkgs.elixir-ls ];
}
