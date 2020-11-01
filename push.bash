#!/bin/bash

# Author
# Purpose
# Date_creation
# Date_modification

date=$(date +"%Y-%m-%d %H:%M");
echo "started $(date)">> ./log.txt;

#git pull;
Rscript -e "blogdown::build_dir()";
git add .;
git commit -m "automatic update $date";
git push;

echo "finished $(date)">> /home/pi/list_log.txt
