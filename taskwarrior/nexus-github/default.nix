{ buildGoModule }:
buildGoModule {
  pname = "nexus-github";
  version = "0.1.0";

  src = ./.;

  vendorSha256 = "pQpattmS9VmO3ZIQUFn66az8GSmB4IvYhTTCFn6SUmo=";
}
