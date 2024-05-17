{ lib, pkgs, ... }:

with lib;
{
  config = {
    environment.systemPackages = with pkgs.llvmPackages_18; [
      clang
      #libclang
      #libcxxClang
      #libstdcxxClang
    ];
  };
}
