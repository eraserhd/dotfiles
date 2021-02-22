{ options, ... }:

{
  config = (if (builtins.hasAttr "launchd" options)
  then {
    old-homebrew.casks = [
      "caffeine"
      "osxfuse"
    ];

    system.defaults.finder.AppleShowAllExtensions = true;
    system.defaults.finder._FXShowPosixPathInTitle = true;

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
