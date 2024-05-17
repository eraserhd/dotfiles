{ pkgs, ... }:

{
  config = {
    environment.systemPackages = [
      pkgs.llvmPackages_18.clang
      pkgs.clang-tools_18
    ];
  };
}
