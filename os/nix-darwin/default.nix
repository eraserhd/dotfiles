{ options, inputs, ... }:

{
  imports = [
    ./modules
  ];

  nix.nixPath = [{
    inherit (inputs) nixpkgs darwin;
  }];

  system.defaults.finder.AppleShowAllExtensions = true;
  system.defaults.finder._FXShowPosixPathInTitle = true;
  system.defaults.magicmouse.MouseButtonMode = "TwoButton";
  system.defaults.NSGlobalDomain = {
    AppleInterfaceStyle = "Dark";
    AppleKeyboardUIMode = 3;
    InitialKeyRepeat = 20;
    KeyRepeat = 1;
    "com.apple.keyboard.fnState" = true;
  };
  nix.configureBuildUsers = true;
}
