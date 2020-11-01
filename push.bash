#!/bin/bash

# Author
# Purpose
# Date_creation
# Date_modification

date=$(date +"%Y-%m-%d %H:%M");
echo "started $(date)">> /home/pc/Data/Projects/corona-stat-lt-blog/log.txt;

#git pull;
Rscript -e "blogdown::build_dir(dir='/home/pc/Data/Projects/corona-stat-lt-blog/', force=TRUE)";
git add . ;
git commit -m "automatic update $date";
git push;

echo "finished $(date)" >> /home/pc/Data/Projects/corona-stat-lt-blog/log.txt
