{ buildGoModule
, Cocoa
, lib
, stdenv
}:

buildGoModule {
  pname = "nats-plumber";
  version = "0.1.0";
  src = ./.;
  buildInputs = lib.optional stdenv.isDarwin Cocoa;
  vendorSha256 = "GdG7QpcrQQ7+FZwswFiEsIHXeOvzgjpeKgX0/woTNLU=";
}
