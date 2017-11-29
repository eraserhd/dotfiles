{ stdenv, fetchurl, libiconv, libX11, ncurses }:
stdenv.mkDerivation {
  name = "chez-scheme-9.5";
  src = fetchurl {
    url = https://github.com/cisco/ChezScheme/archive/v9.5.tar.gz;
    sha256 = "a1d9f93bd8a683ea3d8f2f1b4880f85ea40bf9a482ee6b84cb0fe0ab6148a98c";
  };
  nanopass_src = fetchurl {
    url = https://github.com/nanopass/nanopass-framework-scheme/archive/v1.9.tar.gz;
    sha256 = "625b239f9030d0b1e86b1fffd8b69f7249a63e8b8ca85195a00cf22889f7fc86";
  };
  zlib_src = fetchurl {
    url = https://github.com/madler/zlib/archive/v1.2.11.tar.gz;
    sha256 = "629380c90a77b964d896ed37163f5c3a34f6e6d897311f1df2a7016355c45eff";
  };
  stex_src = fetchurl {
    url = https://github.com/dybvig/stex/archive/v1.2.1.tar.gz;
    sha256 = "bf784ca46aaca9b665b7eb0c39f04f6a695aa40e99b11d8a6d4440648c1bf40e";
  };
  builder = builtins.toFile "builder.sh" ''
    source $stdenv/setup
    tar xvfz $src
    cd ChezScheme-*

    # This prevents the configure script from curling these sources.
    rmdir nanopass ; tar xvfz $nanopass_src ; mv nanopass-framework-scheme-* nanopass
    rmdir zlib ; tar xvfz $zlib_src ; mv zlib-* zlib
    rmdir stex ; tar xvfz $stex_src ; mv stex-* stex

    ./configure --installprefix="$out"
    make install
  '';
  buildInputs = [ libiconv libX11 ncurses ];
}
