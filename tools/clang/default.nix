{ pkgs, ... }:

{
  config = {
    environment.systemPackages = [
      pkgs.llvmPackages_18.clang
      pkgs.llvmPackages_18.clang-tools
    ];
  };
}
