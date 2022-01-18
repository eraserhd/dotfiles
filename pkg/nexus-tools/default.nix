{ buildGoModule }:
buildGoModule {
  pname = "nexus-tools";
  version = "0.1.0";

  src = ./.;

  vendorSha256 = "26VUr0T4JH8/elBGYYRVjWAH98jX6E+wLEDQafLK7fc=";
}
