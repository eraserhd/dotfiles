# Host public keys, used as agenix recipients for secrets which are decrypted
# during system activation.  On NixOS that happens before /home is mounted, so
# jfelice's key isn't readable yet and the host has to be able to decrypt them
# on its own.
#
# Add a host with:
#
#     ssh <host> cat /etc/ssh/ssh_host_ed25519_key.pub    # paste it below
#     agenix -r                                           # rekey every secret
#
{
  crunch = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHhOJ8QVNaMMw9IhLBMp8eXCCnjVaK8EgU6ASpZ+hnJw";
}
