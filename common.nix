{ ... }:

{
  config = {
    environment.variables = {
      CDPATH = [
        "."
        "~/src"
        "~/src/dotfiles/kak/config/kak.symlink/autoload"
      ];
    };
    
    environment.systemPath = [ (toString ./bin) ];
  };
}