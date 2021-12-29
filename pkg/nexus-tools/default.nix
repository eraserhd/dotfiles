{ buildGoModule }:
buildGoModule {
  pname = "nexus-tools";
  version = "0.1.0";

  src = ./.;

  vendorSha256 = "ymRNXBux1cwVZQZbtZJtZLCGR1ANmUEfGtSG6uZeaCs=";
}
