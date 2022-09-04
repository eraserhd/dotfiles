{ k3s
, makeWrapper
, symlinkJoin
}:

symlinkJoin {
  name = "k3s";
  paths = [ k3s ];
  buildInputs = [ makeWrapper ];

  postBuild = ''
    :
  '';
}
