#git remote add origin https://github.com/kl-higgins/bryoquel-test.git
#git branch -M main

git add .
git commit -m "Initial Shiny application"

On branch master

Initial commit

Untracked files:
  (use "git add <file>..." to include in what will be committed)
.gitignore
AssemblerTablesversDB.R
BRYOQUELInterfaceR.Rproj
app.R
bryoquelTest.duckdb
tables/
  
  nothing added to commit but untracked files present (use "git add" to track)

git push -u origin main

$ git push -u origin main
warning: url has no scheme: //github.com/kl-higgins/bryoquel-test.git/
  fatal: credential url cannot be parsed: //github.com/kl-higgins/bryoquel-test.git/
  fatal: remote helper 'https' aborted session


$ git commit -m "Initial Shiny application v2"
[main (root-commit) df2a900] Initial Shiny application v2
8 files changed, 6159 insertions(+)
create mode 100644 .gitignore
create mode 100644 AssemblerTablesversDB.R
create mode 100644 BRYOQUELInterfaceR.Rproj
create mode 100644 app.R
create mode 100644 bryoquelTest.duckdb
create mode 100644 tables/nomenclature.csv
create mode 100644 tables/occurrences.csv
create mode 100644 tables/taxons.csv

$ git push -u origin main
Enumerating objects: 14, done.
Counting objects: 100% (14/14), done.
Delta compression using up to 12 threads
Compressing objects: 100% (11/11), done.
Writing objects: 100% (12/12), 266.33 KiB | 6.66 MiB/s, done.
Total 12 (delta 1), reused 0 (delta 0), pack-reused 0 (from 0)
remote: Resolving deltas: 100% (1/1), done.
To https://github.com/kl-higgins/bryoquel-test.git
0675e63..3f118d0  main -> main
branch 'main' set up to track 'origin/main'.