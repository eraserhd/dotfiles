{ stdenv, fetchurl, unzip }:
stdenv.mkDerivation rec {
  version = "2.8.2";
  name = "1password-${version}";
  src = fetchurl {
    url = "https://downloads.slack-edge.com/mac_releases/Slack-${version}-macOS.zip";
    sha256 = "2fef59b83e6a8d4f3ac99f44f4e8e9bbeca6d9c7788c640d854c8e6daf1799e9";
  };
  buildInputs = [ unzip ];
  builder = builtins.toFile "builder.sh" ''
    source $stdenv/setup
    unzip $src
    mkdir -p $out/Applications
    mv Slack.app $out/Applications/
  '';

  meta = {
    license = stdenv.lib.licenses.unfree;
    platforms = stdenv.lib.platforms.darwin;
  };
}
