{ options, ... }:

{
  config = (if (builtins.hasAttr "homebrew" options)
  then {
    homebrew.casks = [
      "openscad"
      "ultimaker-cura"
    ];
  }
  else {
  });
}
