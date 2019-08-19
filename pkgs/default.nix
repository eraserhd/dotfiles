self: super:

{
  my-packages = super.callPackage ./my-packages.nix {};
}
