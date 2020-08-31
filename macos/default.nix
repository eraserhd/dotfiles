{ options, ... }:

{
  config = (if (builtins.hasAttr "launchd" options)
  then {
    homebrew.casks = [
      "caffeine"
      "osxfuse"
    ];

    system.defaults.finder.AppleShowAllExtensions = true;

    system.defaults.NSGlobalDomain = {
      AppleInterfaceStyle = "Dark";
      AppleKeyboardUIMode = 3;
      InitialKeyRepeat = 20;
      KeyRepeat = 1;
      "com.apple.keyboard.fnState" = true;
    };
  }
  else {
  });
}
