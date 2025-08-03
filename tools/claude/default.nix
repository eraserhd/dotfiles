{ pkgs, ... }:

{
  config = {
    environment.systemPackages = [ pkgs.claude-code ];
  };
}
