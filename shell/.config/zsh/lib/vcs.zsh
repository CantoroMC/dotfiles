autoload -Uz vcs_info
add-zsh-hook precmd vcs_info

# Options
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' get-revision true

# Formats
zstyle ':vcs_info:*' formats \
  "%F{green}%B%{$__BULL[ITALIC_ON]%}%s%%b:%{$__BULL[ITALIC_OFF]%}[%f%F{blue}%B%{$__BULL[ITALIC_ON]%}%b%%b%{$__BULL[ITALIC_OFF]%}%f%F{cyan}%{$__BULL[ITALIC_ON]%} %7.7i %{$__BULL[ITALIC_OFF]%}%f%c%u%F{green}]%f %m"
zstyle ':vcs_info:*' actionformats \
  "(%F{red}%B%{$__BULL[ITALIC_ON]%}%s%%b%{$__BULL[ITALIC_OFF]%}%f)%F{green}[%f%F{blue}%B%{$__BULL[ITALIC_ON]%}%b%%b%{$__BULL[ITALIC_OFF]%}%f%F{cyan}%{$__BULL[ITALIC_ON]%} %7.7i %{$__BULL[ITALIC_OFF]%}%f%c%u%F{green}]%f %m-%F{red}%a%f"
zstyle ':vcs_info:*' stagedstr \
  "%F{green}%{● %2G%}%f"
zstyle ':vcs_info:*' unstagedstr \
  "%F{red}%{ %2G%}%f"

# Vcs_Info Hook
zstyle ':vcs_info:git+set-message:*' hooks git-untracked git-commits_ahead git-commits_behind

# Wrap in a local function instead of exporting the variable directly in
# order to avoid interfering with manually-run git commands by the user.
function __git_prompt_git() {
  GIT_OPTIONAL_LOCKS=0 command git "$@"
}

# Check if there are untracked files
function +vi-git-untracked() {
  if [[ -n $(__git_prompt_git ls-files --exclude-standard --others 2> /dev/null) ]]; then
    hook_com[unstaged]+="%F{blue}%{ %2G%}%f"
  fi
}
# Gets the number of commits ahead from remote
function +vi-git-commits_ahead() {
  local commits="$(__git_prompt_git rev-list --count @{upstream}..HEAD 2>/dev/null)"
  if [[ -n "$commits" && "$commits" != 0 ]]; then
    hook_com[branch]+=" %F{red}$commits%{⇡ %2G%}%f"
  fi
}
# Gets the number of commits behind remote
function +vi-git-commits_behind() {
  local commits="$(__git_prompt_git rev-list --count HEAD..@{upstream} 2>/dev/null)"
  if [[ -n "$commits" && "$commits" != 0 ]]; then
    hook_com[branch]+=" %F{red}$commits%{⇡ %2G%}%f"
  fi
}
