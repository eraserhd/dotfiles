{ stdenv, fetchurl, unzip }:
stdenv.mkDerivation rec {
  version = "6.8.4";
  name = "1password-${version}";
  src = fetchurl {
    url = "https://d13itkw33a7sus.cloudfront.net/dist/1P/mac4/1Password-${version}.zip";
    sha256 = "6d86d1d7c75e1f2245af955c9c94ce49d5293c1cde7be98ca3268c0d3106b342";
  };
  buildInputs = [ unzip ];
  builder = builtins.toFile "builder.sh" ''
    source $stdenv/setup
    unzip $src
    mkdir -p $out/Applications
    mv 1Password*.app $out/Applications/
  '';

  meta = {
    license = stdenv.lib.licenses.unfree;
    platforms = stdenv.lib.platforms.darwin;
  };
}
