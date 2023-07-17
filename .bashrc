if command -v fzf-share >/dev/null; then
  source "$(fzf-share)/key-bindings.bash"
  source "$(fzf-share)/completion.bash"
fi

eval "$(direnv hook bash)"

alias dotfiles='/etc/profiles/per-user/arjen/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'
