let
  jfelice-age = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIB/xxLZIrNwqwwT1ujgML0kNJ0gThB2mmqCrQKWElCTu";

  # Decrypted by home-manager, after login, with ~/.ssh/id_ed25519.
  user = [ jfelice-age ];

  # Decrypted during system activation, before /home is mounted, so the host
  # has to be a recipient too.  See hosts/keys.nix for how to add one.
  system = user ++ builtins.attrValues (import ./hosts/keys.nix);
in {
  "shell/private.sh.age".publicKeys = user;
  "tools/aws/boto.age".publicKeys = user;
  "tools/aws/credentials.age".publicKeys = user;
  "tools/clojure/profiles.clj.age".publicKeys = user;
  "tools/npm/npmrc.age".publicKeys = user;

  "networking/wifi/secrets.age".publicKeys = system;
  "tools/nats/token.env.age".publicKeys = system;

  # Not deployed anywhere; kept encrypted because it is a licensed copy.
  "fonts/Input-Font.zip.age".publicKeys = user;
}
