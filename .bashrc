if command -v fzf-share >/dev/null; then
  source "$(fzf-share)/key-bindings.bash"
  source "$(fzf-share)/completion.bash"
fi

eval "$(direnv hook bash)"

alias dotfiles='/etc/profiles/per-user/arjen/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'
alias createdirenv='nix --extra-experimental-features "nix-command flakes" flake new -t github:nix-community/nix-direnv .'

# Added by LM Studio CLI (lms)
export PATH="$PATH:/home/arjen/.cache/lm-studio/bin"
