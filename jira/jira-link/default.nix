{ lib, buildGoModule }:

buildGoModule rec {
  pname = "jira-link";
  version = "0.1.0";

  src = ./.;

  vendorSha256 = "pQpattmS9VmO3ZIQUFn66az8GSmB4IvYhTTCFn6SUmo=";

  meta = with lib; {
    platforms = platforms.all;
  };
}
