{ pkgs, ... }:

{
  config = {
    environment.systemPackages = with pkgs; [
      nodejs_latest
      typescript-language-server
    ];
  };
}

