{ buildGoModule
, Cocoa
, lib
, libX11
, stdenv
}:

buildGoModule {
  pname = "nats-plumber";
  version = "0.1.0";
  src = ./.;
  buildInputs = lib.optional stdenv.isDarwin Cocoa ++ lib.optional (!stdenv.isDarwin) libX11;
  vendorSha256 = "GdG7QpcrQQ7+FZwswFiEsIHXeOvzgjpeKgX0/woTNLU=";
}
