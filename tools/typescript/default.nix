{ ... }:

{
  config = {
    home-manager.users.jfelice = { pkgs, ... }: {
      home.packages = [ pkgs.typescript-language-server ];
    };
  };
}
