{ stdenv, rustPlatform, fetchFromGitHub, llvmPackages }:

rustPlatform.buildRustPackage rec {
  name = "parinfer-rust-${version}";
  version = "0.4.0";

  src = fetchFromGitHub {
    owner = "eraserhd";
    repo = "parinfer-rust";
    rev = "6809233baf080e8d592cc4eb370b56a3509c235f";
    sha256 = "0y0lhs8lplhxcv4c6h68f8j6nx44rs4jvdvy52bmxwgwariirgsy";
  };

  cargoSha256 = "0i5wy15w985nxwl4b6rzb06hchzjwph6ygzjkkmigm9diw9jcycn";

  buildInputs = [ llvmPackages.libclang llvmPackages.clang ];
  LIBCLANG_PATH = "${llvmPackages.libclang}/lib";

  postInstall = ''
    mkdir -p $out/share/kak/autoload/plugins
    cp rc/parinfer.kak $out/share/kak/autoload/plugins/
  '';

  meta = with stdenv.lib; {
    description = "Infer parentheses for Clojure, Lisp, and Scheme.";
    homepage = "https://github.com/eraserhd/parinfer-rust";
    license = licenses.isc;
    maintainers = with maintainers; [ eraserhd ];
    platforms = platforms.all;
  };
}
