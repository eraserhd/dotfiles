{ pkgs, ... }:

{
  config = {
    local.loginShell.package = pkgs.zsh;
    programs.zsh.enable = true;
    programs.zsh.enableCompletion = true;
    programs.zsh.interactiveShellInit = ''
      :r() {
        if command -v darwin-rebuild >/dev/null; then
          pushd ~/src/dotfiles >/dev/null
          darwin-rebuild build || return $?
          if [[ $(readlink /run/current-system) != $(readlink ./result) ]]; then
            darwin-rebuild switch || return $?
          fi
          popd >/dev/null
        elif command -v nixos-rebuild >/dev/null; then
          pushd ~/src/dotfiles >/dev/null
          nixos-rebuild build || return $?
          if [[ $(readlink /run/current-system) != $(readlink ./result) ]]; then
            sudo nixos-rebuild switch || return $?
          fi
          popd >/dev/null
        fi
        unset __NIX_DARWIN_SET_ENVIRONMENT_DONE __NIXOS_SET_ENVIRONMENT_DONE
        unset __ETC_BASHRC_SOURCED __ETC_PROFILE_SOURCED __ETC_PROFILE_DONE
        unset __ETC_ZSHENV_SOURCED __ETC_ZSHRC_SOURCED
        exec $SHELL -l
      }
    '';
  };
}
