let
  jfelice-age = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIB/xxLZIrNwqwwT1ujgML0kNJ0gThB2mmqCrQKWElCTu";
in {
  "shell/private.sh.age".publicKeys = [ jfelice-age ];
  "tools/aws/boto.age".publicKeys = [ jfelice-age ];
  "tools/aws/credentials.age".publicKeys = [ jfelice-age ];
  "tools/clojure/profiles.clj.age".publicKeys = [ jfelice-age ];
  "tools/npm/npmrc.age".publicKeys = [ jfelice-age ];
}
