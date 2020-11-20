# Git Commands

### Basics

| __Command__         | __Action__                             |
| :---                | :---                                   |
| git init            | initiates git in the current directory |
| git clone <address> | creates a git repo from given address  |

### Staging

| __Command__        | __Action__                                                |
| :---               | :---                                                      |
| git add file.txt   | adds(stages) file.txt to the git                          |
| git add *          | adds(stages) all new modifications, deletions, creations  |
| git reset file.txt | Removes file.txt from the stage                           |
| git rm file.txt    | removes file.txt both from git and file system            |
| git status         | shows the modifications and stuff that are not staged yet |

### Branching

| __Command__                 | __Action__                           |
| :---                        | :---                                 |
| git branch                  | shows all the branches             |
| git branch my-branch        | creates my-branch                  |
| git branch -d my-branch     | deletes my-branch                  |
| git checkout my-bracnch     | switches to my-branch              |
| git merge my-branch         | merges my-branch to current branch |
| git push origin :my-branch  | delete remote branch               |
| git cherry-pick <commit_id> | merge the specified commit         |

### Remote

| __Command__                        | __Action__                         |
| :---                               | :---                               |
| git remote                         | shows the remotes                  |
| git remote -v                      | shows the remote for pull and push |
| git remote add my-remote <address> | creates a remote                   |

### Commit and Share

| __Command__                  | __Action__                                         |
| :---                         | :---                                               |
| git log                      | shows the log of commits                         |
| git commit -m "msg"          | commit changes with a msg                        |
| git push my-remote my-branch | pushes the commits to the my-remote in my-branch |
| git pull my-remote my-branch | pulls and tries to merge my-branch from my-remote to the current branch

### Tags

| __Command__                       | __Action__                               |
| :---                              | :---                                     |
| git tag                           | shows all the tags                       |
| git tag -a v1.0 -m "msg"          | creates an annotated tag                 |
| git show v1.0                     | shows the description of version-1.0 tag |
| git tag --delete v1.0             | deletes the tag in local directory       |
| git push --delete my-remote v1.0  | deletes the tag in my-remote             |
| git push my-remote my-branch v1.0 | push v1.0 tag to my-remote in my-branch  |
| git fetch --tags                  | pulls the tags from remote               |

### Stashing

| __Command__                          | __Action__                                     |
| :---                                 | :---                                           |
| git stash                            | stashes the staged and unstaged changes        |
| git stash -u                         | stash everything including new untracked files |
| git stash save "msg"                 | stash with a msg                               |
| git stash list                       | list all stashes                               |
| git stash pop                        | delete the recent stash and applies it         |
| git stash stach@{2}                  | delete the {2} stash and applies it            |
| git stash show                       | shows the description of stash                 |
| git stash apply                      | keep the stash and applies it to the git       |
| git stash branch my-branch stash@{1} | creates a branch from your stash               |
| git stash drop stash@{1}             | deletes the {1} stash                          |
| git stash clear                      | clears all the stash                           |

