{ config, lib, pkgs, ... }:

with lib;
let
  environment = user: let
    fixVariableReferences = s:
      let
        result = builtins.match "(.*)\\$([a-zA-Z0-9_]+)(.*)" s;
      in
        if (builtins.isNull result)
        then s
        else (fixVariableReferences "${builtins.elemAt result 0}\${${builtins.elemAt result 1}}${builtins.elemAt result 2}");
    vars = config.environment.variables // {
      HOME = (builtins.getAttr user config.users.users).home;
    } // (if (hasAttr "systemPath" config.environment)
    then {
      PATH = config.environment.systemPath;
    }
    else {
    });

    directives = map (name: "env ${name}=${fixVariableReferences (getAttr name vars)}") (attrNames vars);
  in
    concatStringsSep "\n" directives;

in {
  config = {
    environment.systemPackages = with pkgs; [ kitty ];
    home-manager.users.jfelice = { pkgs, ... }: {
      home.file.".config/kitty/macos-launch-services-cmdline".text = let
        homeDirectory = config.users.users.jfelice.home;
      in ''
        --listen-on=unix:/Users/jfelice/.run/kitty --single-instance --directory=${homeDirectory}/src
      '';

      programs.kitty = {
        enable = true;
        font = {
          name = "Input Mono Narrow Light";
          size = 11;
        };
        settings = {
          bold_font = "Input Mono Narrow Medium";
          italic_font = "Input Mono Narrow Light Italic";
          bold_italic_font = "Input Mono Narrow Medium Italic";

          allow_remote_control = "yes";

          scrollback_lines = "5000";

          window_resize_step_cells = "1";
          window_resize_step_lines = "1";
          window_margin_width = "5.0";

          macos_option_as_alt = "yes";
          macos_thicken_font = "0.5";

          foreground = "#bfc7d5";
          background = "#14161f";

          cursor_text_color = "#c782ea";

          #: black
          color0 = "#292d3e";
          color8 = "#292d3e";

          #: red
          color1 = "#ff5370";
          color9 = "#ff5370";

          #: green
          color2 = "#c3e88d";
          color10 = "#c3e88d";

          #: yellow
          color3 = "#ffcb6b";
          color11 = "#ffcb6b";

          #: blue
          color4 = "#82b1ff";
          color12 = "#82b1ff";

          #: magenta
          color5 = "#c792ea";
          color13 = "#c792ea";

          #: cyan
          color6 = "#89ddff";
          color14 = "#89ddff";

          #: white
          color7 = "#bfc7d5";
          color15 = "#bfc7d5";
        };
        keybindings = {
          "shift+ctrl+h" = "launch --stdin-add-formatting --stdin-source=@screen_scrollback --cwd=current --type=overlay kak-scrollback-pager @scrolled-by @cursor-x @cursor-y @line-count";
        };
        extraConfig = ''
          ${environment "jfelice"}
        '';
      };
    };
  };
}
