{ options, ... }:

{
  config = (if (builtins.hasAttr "homebrew" options)
  then {
    homebrew.casks = [ "alfred" ];
  }
  else {
  });
}
