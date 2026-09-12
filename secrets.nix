let
  jfelice-age = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIB/xxLZIrNwqwwT1ujgML0kNJ0gThB2mmqCrQKWElCTu";
in {
  "shell/private.sh.age".publicKeys = [ jfelice-age ];
}
