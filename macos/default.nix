{ options, ... }:

{
  config = (if (builtins.hasAttr "homebrew" options)
  then {
    homebrew.casks = [
      "caffeine"
      "osxfuse"
    ];

    system.defaults.NSGlobalDomain = {
      AppleInterfaceStyle = "Dark";
    };
  }
  else {
  });
}
