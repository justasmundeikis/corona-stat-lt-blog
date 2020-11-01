#!/bin/bash

# Author
# Purpose
# Date_creation
# Date_modification

date=$(date +"%Y-%m-%d %H:%M");
echo "started build $(date)">> /home/pc/Data/Projects/corona-stat-lt-blog/log.txt;

#git pull;
Rscript -e "blogdown::build_dir(dir='/home/pc/Data/Projects/corona-stat-lt-blog', force=TRUE)";

date=$(date +"%Y-%m-%d %H:%M");
echo "finished build $(date)" >> /home/pc/Data/Projects/corona-stat-lt-blog/log.txt;

git add . ;
git commit -m "automatic update $date";
git push;


