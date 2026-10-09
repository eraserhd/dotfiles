{ ... }:

{
  config = {
    home-manager.users.jfelice = { ... }: {
      programs.broot = {
        enable = true;
        settings.verbs = [
          {
            invocation = "plumb";
            key = "enter";
            apply_to = "file";
            execution = "9 plumb {file}";
            leave_broot = false;
          }
        ];
      };
    };
  };
}
