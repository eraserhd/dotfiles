{ pkgs, lib, options, ... }:

with lib;
{
  config =
    (if (builtins.hasAttr "launchd" options)
     then {}
     else {
       environment.systemPackages = [ pkgs.tomat ];

       home-manager.users.jfelice = { pkgs, ... }: {
         services.tomat = {
           enable = true;

           settings = {
             timer = {
               work = 45;
               break = 15;
             };
           };
         };
       };
     });
}
