{ ... }:

{
  config = {
    local.networking.nameservers = [
      "2620:119:35::35"
      "2620:119:53::53"
      "208.67.222.222"
      "208.67.220.220"
    ];
  };
}
