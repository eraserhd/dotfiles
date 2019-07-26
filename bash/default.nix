{ ... }:

{
  config = {
    programs.bash.interactiveShellInit = ''
      :r() {
        unset __NIX_DARWIN_SET_ENVIRONMENT_DONE __ETC_BASHRC_SOURCED
        exec $SHELL -l
      }
    '';
  };
}
