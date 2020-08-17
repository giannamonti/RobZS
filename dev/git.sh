exit 0

## Status
git status

## Remote 
git remote add origin https://github.com/giannamonti/RobZS.git
git push -u origin master

## Download news from the repository
git fetch

## create a feature-branch
git branch wip-feature-branch
git checkout wip-feature-branch                   # use the new branch
git push --set-upstream origin wip-feature-branch # first time push


## Make a commit 
git add file1 file2 ...
git commit -m "commit message"


## Push the current branch
git push

## Pull the current branch
git pull

## Merge feature branch into master
git status                      
git checkout master                  # move on master (so you can change it)
git merge wip-feature-branch         # add the feature-branch changes to master (merge)
git branch -D wip-feature-branch     # delete the local feature-branch
git push origin :wip-feature-branch  # delete the remote feature-branch





