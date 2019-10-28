{ stdenv, bash, plan9port, ... }:

stdenv.mkDerivation {
  name = "plan9port-namespace-wrapper";
  version = plan9port.version;
  src = ./.;
  installPhase = ''
    mkdir -p $out/bin
    substitute 9.sh $out/bin/9 \
      --subst-var-by bash '${bash}' \
      --subst-var-by plan9port '${plan9port}'
    chmod +x $out/bin/9
  '';
}
