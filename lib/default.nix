{ ... }:

let
  localLib = {

  };
in {
  config = {
    _module.args.localLib = localLib;
  };
}
