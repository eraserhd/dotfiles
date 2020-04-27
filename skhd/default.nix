{ options, pkgs, ... }:

{
  config = if (builtins.hasAttr "skhd" options.services)
  then {
    services.skhd.enable = true;
    services.skhd.skhdConfig = ''
      ctrl - w : gewgaw-yabai-focus-window
    '';

    #  ctrlw < shift - c : \
    #    window_id=$(yabai -m query --windows --window |jq .id) ; \
    #    yabai -m window --space coding ; \
    #    yabai -m window --focus $window_id ; \
    #    skhd -k escape
    #  ctrlw < shift - m : \
    #    window_id=$(yabai -m query --windows --window |jq .id) ; \
    #    yabai -m window --space meeting ; \
    #    yabai -m window --focus $window_id ; \
    #    skhd -k escape
    #  ctrlw < shift - b : \
    #    window_id=$(yabai -m query --windows --window |jq .id) ; \
    #    yabai -m window --space browse ; \
    #    yabai -m window --focus $window_id ; \
    #    yabai -m window --grid 1:1:0:0:1:1 ; \
    #    skhd -k escape
    environment.systemPackages = [ pkgs.skhd pkgs.gewgaw ];
  }
  else {
  };
}
