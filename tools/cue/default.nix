{ pkgs, ... }:

{
  environment.systemPackages = [ pkgs.cue ];
}
