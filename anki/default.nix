{ options, ... }:

{
  config = (if (builtins.hasAttr "old-homebrew" options)
  then {
    old-homebrew.casks = [ "anki" ];
  }
  else {
  });
}
