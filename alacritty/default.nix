{ pkgs, lib, ... }:

{
  config = lib.mkIf pkgs.stdenv.isDarwin {
    environment.systemPackages = with pkgs; [
      alacritty
      reattach-to-user-namespace
    ];

    users.users.jfelice.name = "jfelice";
    home-manager.users.jfelice = { pkgs, ... }: {
      home.file.".config/alacritty/alacritty.yml".text = ''
        # Configuration for Alacritty, the GPU enhanced terminal emulator

        # Any items in the `env` entry below will be added as
        # environment variables. Some entries may override variables
        # set by alacritty it self.
        env:
          # TERM env customization.
          #
          # If this property is not set, alacritty will set it to xterm-256color.
          #
          # Note that some xterm terminfo databases don't declare support for italics.
          # You can verify this by checking for the presence of `smso` and `sitm` in
          # `infocmp xterm-256color`.
          TERM: xterm-256color

        window:
          # Window dimensions in character columns and lines
          # (changes require restart)
          dimensions:
            columns: 0
            lines: 0

          # Adds this many blank pixels of padding around the window
          # Units are physical pixels; this is not DPI aware.
          # (change requires restart)
          padding:
            x: 2
            y: 2

          # Window decorations
          #
          # Values for `decorations`:
          #     - full: Borders and title bar
          #     - none: Neither borders nor title bar
          #
          # Values for `decorations` (macOS only):
          #     - transparent: Title bar, transparent background and title bar buttons
          #     - buttonless: Title bar, transparent background, but no title bar buttons
          decorations: buttonless

          startup_mode: Maximized

        scrolling:
          # Maximum number of lines in the scrollback buffer.
          # Specifying '0' will disable scrolling.
          history: 10000

          # Number of lines the viewport will move for every line scrolled when
          # scrollback is enabled (history > 0).
          multiplier: 3

          # Faux Scrolling
          #
          # The `faux_multiplier` setting controls the number of lines the terminal
          # should scroll when the alternate screen buffer is active. This is used
          # to allow mouse scrolling for applications like `man`.
          #
          # Specifying `0` will disable faux scrolling.
          faux_multiplier: 3

          # Scroll to the bottom when new text is written to the terminal.
          auto_scroll: false

        # Spaces per Tab (changes require restart)
        #
        # This setting defines the width of a tab in cells.
        #
        # Some applications, like Emacs, rely on knowing about the width of a tab.
        # To prevent unexpected behavior in these applications, it's also required to
        # change the `it` value in terminfo when altering this setting.
        tabspaces: 8

        # Font configuration (changes require restart)
        font:
          # The normal (roman) font face to use.
          normal:
            family: mononoki
            # Style can be specified to pick a specific face.
            # style: Regular

          # The bold font face
          bold:
            family: mononoki
            # Style can be specified to pick a specific face.
            # style: Bold

          # The italic font face
          italic:
            family: mononoki
            # Style can be specified to pick a specific face.
            # style: Italic

          # Point size of the font
          size: 13.0

          # Offset is the extra space around each character. offset.y can be thought of
          # as modifying the linespacing, and offset.x as modifying the letter spacing.
          offset:
            x: 0
            y: 0

          # Glyph offset determines the locations of the glyphs within their cells with
          # the default being at the bottom. Increase the x offset to move the glyph to
          # the right, increase the y offset to move the glyph upward.
          glyph_offset:
            x: 0
            y: 0

          # OS X only: use thin stroke font rendering. Thin strokes are suitable
          # for retina displays, but for non-retina you probably want this set to
          # false.
          use_thin_strokes: false

        # When true, bold text is drawn using the bright variant of colors.
        draw_bold_text_with_bright_colors: true

        # Colors (Tomorrow Night Bright)
        colors:
          # Default colors
          primary:
            background: '0x292d3e'
            foreground: '0xbfc7d5'

            # (Optional) Bright and Dim foreground colors
            #
            # The dimmed foreground color is calculated automatically if it is not present.
            # If the bright foreground color is not set, or `draw_bold_text_with_bright_colors`
            # is `false`, the normal foreground color will be used.
            #
            # dim_foreground: '0x9a9a9a'
            # bright_foreground: '0xffffff'

          # Cursor colors
          #
          # Colors which should be used to draw the terminal cursor. If these are unset,
          # the cursor color will be the inverse of the cell color.
          cursor:
            text: '0x292d3e'
            cursor: '0xc782ea'

          # Normal colors
          normal:
            black:   '0x292d3e'
            red:     '0xff5370'
            green:   '0xc3e88d'
            yellow:  '0xffcb6b'
            blue:    '0x82b1ff'
            magenta: '0xc792ea'
            cyan:    '0x89ddff'
            white:   '0xbfc7d5'

          # Bright colors
          bright:
            black:   '0x292d3e'
            red:     '0xff5370'
            green:   '0xc3e88d'
            yellow:  '0xffcb6b'
            blue:    '0x82b1ff'
            magenta: '0xc792ea'
            cyan:    '0x89ddff'
            white:   '0xbfc7d5'

        # Visual Bell
        #
        # Any time the BEL code is received, Alacritty "rings" the visual bell. Once
        # rung, the terminal background will be set to white and transition back to the
        # default background color. You can control the rate of this transition by
        # setting the `duration` property (represented in milliseconds). You can also
        # configure the transition function by setting the `animation` property.
        #
        # Possible values for `animation`
        # `Ease`
        # `EaseOut`
        # `EaseOutSine`
        # `EaseOutQuad`
        # `EaseOutCubic`
        # `EaseOutQuart`
        # `EaseOutQuint`
        # `EaseOutExpo`
        # `EaseOutCirc`
        # `Linear`
        #
        # To completely disable the visual bell, set its duration to 0.
        #
        visual_bell:
          animation: EaseOutExpo
          duration: 0

        # Background opacity
        background_opacity: 1.0

        # Mouse bindings
        #
        # Currently doesn't support modifiers. Both the `mouse` and `action` fields must
        # be specified.
        #
        # Values for `mouse`:
        # - Middle
        # - Left
        # - Right
        # - Numeric identifier such as `5`
        #
        # Values for `action`:
        # - Paste
        # - PasteSelection
        # - Copy (TODO)
        mouse_bindings:
          - { mouse: Middle, action: PasteSelection }

        mouse:
          # Click settings
          #
          # The `double_click` and `triple_click` settings control the time
          # alacritty should wait for accepting multiple clicks as one double
          # or triple click.
          double_click: { threshold: 300 }
          triple_click: { threshold: 300 }

          # Faux Scrollback
          #
          # The `faux_scrollback_lines` setting controls the number
          # of lines the terminal should scroll when the alternate
          # screen buffer is active. This is used to allow mouse
          # scrolling for applications like `man`.
          #
          # To disable this completely, set `faux_scrollback_lines` to 0.
          faux_scrolling_lines: 1

          # If this is `true`, the cursor is temporarily hidden when typing.
          hide_when_typing: false

        selection:
          semantic_escape_chars: ",│`|:\"' ()[]{}<>"

        # When set to `true`, selected text will be copied to the primary clipboard.
          save_to_clipboard: false

        dynamic_title: true

        cursor:
          # Cursor style
          #
          # Values for `style`:
          #   - ▇ Block
          #   - _ Underline
          #   - | Beam
          style: Block

          # If this is `true`, the cursor will be rendered as a hollow box when the
          # window is not focused.
          unfocused_hollow: true

        # Live config reload (changes require restart)
        live_config_reload: true

        # Shell
        #
        # You can set shell.program to the path of your favorite shell, e.g. /bin/fish.
        # Entries in shell.args are passed unmodified as arguments to the shell.
        #
        shell:
          program: /run/current-system/sw/bin/bash
          args:
            - --login

        # Send ESC (\x1b) before characters when alt is pressed.
        alt_send_esc: true

        debug:
          # Keep the log file after quitting Alacritty.
          persistent_logging: false


        # Key bindings
        #
        # Each binding is defined as an object with some properties. Most of the
        # properties are optional. All of the alphabetical keys should have a letter for
        # the `key` value such as `V`. Function keys are probably what you would expect
        # as well (F1, F2, ..). The number keys above the main keyboard are encoded as
        # `Key1`, `Key2`, etc. Keys on the number pad are encoded `Number1`, `Number2`,
        # etc.  These all match the glutin::VirtualKeyCode variants.
        #
        # A list with all available `key` names can be found here:
        # https://docs.rs/glutin/*/glutin/enum.VirtualKeyCode.html#variants
        #
        # Possible values for `mods`
        # `Command`, `Super` refer to the super/command/windows key
        # `Control` for the control key
        # `Shift` for the Shift key
        # `Alt` and `Option` refer to alt/option
        #
        # mods may be combined with a `|`. For example, requiring control and shift
        # looks like:
        #
        # mods: Control|Shift
        #
        # The parser is currently quite sensitive to whitespace and capitalization -
        # capitalization must match exactly, and piped items must not have whitespace
        # around them.
        #
        # Either an `action`, `chars`, or `command` field must be present.
        #   `action` must be one of `Paste`, `PasteSelection`, `Copy`, or `Quit`.
        #   `chars` writes the specified string every time that binding is activated.
        #     These should generally be escape sequences, but they can be configured to
        #     send arbitrary strings of bytes.
        #   `command` must be a map containing a `program` string, and `args` array of
        #     strings. For example:
        #     - { ... , command: { program: "alacritty", args: ["-e", "vttest"] } }
        key_bindings:
          - { key: V,        mods: Command, action: Paste                        }
          - { key: C,        mods: Command, action: Copy                         }
          - { key: Paste,                   action: Paste                        }
          - { key: Copy,                    action: Copy                         }
          - { key: H,        mods: Command, action: Hide                         }
          - { key: Q,        mods: Command, action: Quit                         }
          - { key: W,        mods: Command, action: Quit                         }
          - { key: Home,                    chars: "\x1bOH",   mode: AppCursor   }
          - { key: Home,                    chars: "\x1b[H",   mode: ~AppCursor  }
          - { key: End,                     chars: "\x1bOF",   mode: AppCursor   }
          - { key: End,                     chars: "\x1b[F",   mode: ~AppCursor  }
          - { key: Key0,     mods: Command, action: ResetFontSize                }
          - { key: Equals,   mods: Command, action: IncreaseFontSize             }
          - { key: Minus,    mods: Command, action: DecreaseFontSize             }
          - { key: PageUp,   mods: Shift,   chars: "\x1b[5;2~"                   }
          - { key: PageUp,   mods: Control, chars: "\x1b[5;5~"                   }
          - { key: PageUp,                  chars: "\x1b[5~"                     }
          - { key: PageDown, mods: Shift,   chars: "\x1b[6;2~"                   }
          - { key: PageDown, mods: Control, chars: "\x1b[6;5~"                   }
          - { key: PageDown,                chars: "\x1b[6~"                     }
          - { key: Tab,      mods: Shift,   chars: "\x1b[Z"                      }
          - { key: Back,                    chars: "\x7f"                        }
          - { key: Back,     mods: Alt,     chars: "\x1b\x7f"                    }
          - { key: Insert,                  chars: "\x1b[2~"                     }
          - { key: Delete,                  chars: "\x1b[3~"                     }
          - { key: Left,     mods: Shift,   chars: "\x1b[1;2D"                   }
          - { key: Left,     mods: Control, chars: "\x1b[1;5D"                   }
          - { key: Left,     mods: Alt,     chars: "\x1b[1;3D"                   }
          - { key: Left,                    chars: "\x1b[D",   mode: ~AppCursor  }
          - { key: Left,                    chars: "\x1bOD",   mode: AppCursor   }
          - { key: Right,    mods: Shift,   chars: "\x1b[1;2C"                   }
          - { key: Right,    mods: Control, chars: "\x1b[1;5C"                   }
          - { key: Right,    mods: Alt,     chars: "\x1b[1;3C"                   }
          - { key: Right,                   chars: "\x1b[C",   mode: ~AppCursor  }
          - { key: Right,                   chars: "\x1bOC",   mode: AppCursor   }
          - { key: Up,       mods: Shift,   chars: "\x1b[1;2A"                   }
          - { key: Up,       mods: Control, chars: "\x1b[1;5A"                   }
          - { key: Up,       mods: Alt,     chars: "\x1b[1;3A"                   }
          - { key: Up,                      chars: "\x1b[A",   mode: ~AppCursor  }
          - { key: Up,                      chars: "\x1bOA",   mode: AppCursor   }
          - { key: Down,     mods: Shift,   chars: "\x1b[1;2B"                   }
          - { key: Down,     mods: Control, chars: "\x1b[1;5B"                   }
          - { key: Down,     mods: Alt,     chars: "\x1b[1;3B"                   }
          - { key: Down,                    chars: "\x1b[B",   mode: ~AppCursor  }
          - { key: Down,                    chars: "\x1bOB",   mode: AppCursor   }
          - { key: F1,                      chars: "\x1bOP"                      }
          - { key: F2,                      chars: "\x1bOQ"                      }
          - { key: F3,                      chars: "\x1bOR"                      }
          - { key: F4,                      chars: "\x1bOS"                      }
          - { key: F5,                      chars: "\x1b[15~"                    }
          - { key: F6,                      chars: "\x1b[17~"                    }
          - { key: F7,                      chars: "\x1b[18~"                    }
          - { key: F8,                      chars: "\x1b[19~"                    }
          - { key: F9,                      chars: "\x1b[20~"                    }
          - { key: F10,                     chars: "\x1b[21~"                    }
          - { key: F11,                     chars: "\x1b[23~"                    }
          - { key: F12,                     chars: "\x1b[24~"                    }
          - { key: F1,       mods: Shift,   chars: "\x1b[1;2P"                   }
          - { key: F2,       mods: Shift,   chars: "\x1b[1;2Q"                   }
          - { key: F3,       mods: Shift,   chars: "\x1b[1;2R"                   }
          - { key: F4,       mods: Shift,   chars: "\x1b[1;2S"                   }
          - { key: F5,       mods: Shift,   chars: "\x1b[15;2~"                  }
          - { key: F6,       mods: Shift,   chars: "\x1b[17;2~"                  }
          - { key: F7,       mods: Shift,   chars: "\x1b[18;2~"                  }
          - { key: F8,       mods: Shift,   chars: "\x1b[19;2~"                  }
          - { key: F9,       mods: Shift,   chars: "\x1b[20;2~"                  }
          - { key: F10,      mods: Shift,   chars: "\x1b[21;2~"                  }
          - { key: F11,      mods: Shift,   chars: "\x1b[23;2~"                  }
          - { key: F12,      mods: Shift,   chars: "\x1b[24;2~"                  }
          - { key: F1,       mods: Control, chars: "\x1b[1;5P"                   }
          - { key: F2,       mods: Control, chars: "\x1b[1;5Q"                   }
          - { key: F3,       mods: Control, chars: "\x1b[1;5R"                   }
          - { key: F4,       mods: Control, chars: "\x1b[1;5S"                   }
          - { key: F5,       mods: Control, chars: "\x1b[15;5~"                  }
          - { key: F6,       mods: Control, chars: "\x1b[17;5~"                  }
          - { key: F7,       mods: Control, chars: "\x1b[18;5~"                  }
          - { key: F8,       mods: Control, chars: "\x1b[19;5~"                  }
          - { key: F9,       mods: Control, chars: "\x1b[20;5~"                  }
          - { key: F10,      mods: Control, chars: "\x1b[21;5~"                  }
          - { key: F11,      mods: Control, chars: "\x1b[23;5~"                  }
          - { key: F12,      mods: Control, chars: "\x1b[24;5~"                  }
          - { key: F1,       mods: Alt,     chars: "\x1b[1;6P"                   }
          - { key: F2,       mods: Alt,     chars: "\x1b[1;6Q"                   }
          - { key: F3,       mods: Alt,     chars: "\x1b[1;6R"                   }
          - { key: F4,       mods: Alt,     chars: "\x1b[1;6S"                   }
          - { key: F5,       mods: Alt,     chars: "\x1b[15;6~"                  }
          - { key: F6,       mods: Alt,     chars: "\x1b[17;6~"                  }
          - { key: F7,       mods: Alt,     chars: "\x1b[18;6~"                  }
          - { key: F8,       mods: Alt,     chars: "\x1b[19;6~"                  }
          - { key: F9,       mods: Alt,     chars: "\x1b[20;6~"                  }
          - { key: F10,      mods: Alt,     chars: "\x1b[21;6~"                  }
          - { key: F11,      mods: Alt,     chars: "\x1b[23;6~"                  }
          - { key: F12,      mods: Alt,     chars: "\x1b[24;6~"                  }
          - { key: F1,       mods: Command, chars: "\x1b[1;3P"                   }
          - { key: F2,       mods: Command, chars: "\x1b[1;3Q"                   }
          - { key: F3,       mods: Command, chars: "\x1b[1;3R"                   }
          - { key: F4,       mods: Command, chars: "\x1b[1;3S"                   }
          - { key: F5,       mods: Command, chars: "\x1b[15;3~"                  }
          - { key: F6,       mods: Command, chars: "\x1b[17;3~"                  }
          - { key: F7,       mods: Command, chars: "\x1b[18;3~"                  }
          - { key: F8,       mods: Command, chars: "\x1b[19;3~"                  }
          - { key: F9,       mods: Command, chars: "\x1b[20;3~"                  }
          - { key: F10,      mods: Command, chars: "\x1b[21;3~"                  }
          - { key: F11,      mods: Command, chars: "\x1b[23;3~"                  }
          - { key: F12,      mods: Command, chars: "\x1b[24;3~"                  }
          - { key: NumpadEnter,             chars: "\n"                          }
          - { key: Space,     mods: Control,   chars: "\x00"                     } # Ctrl + Space
      '';
    };
  };
}
