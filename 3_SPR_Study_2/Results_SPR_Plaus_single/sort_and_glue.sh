dir_array=(SPR_o1_l1/ SPR_o1_l2/ SPR_o1_l3/)
files_array=(demog.txt consent.txt survey.txt reading.txt task.txt)

for direc in "${dir_array[@]}"; do  # loop through the array
  cd "$direc"
  pwd
  bash sort_results.sh
  cd ..
done

prefixvar="attic/"
for file in "${files_array[@]}"; do
    pathvar="$prefixvar$file"
    echo "$pathvar"
    cp -v "$file" "$pathvar"
    rm -v "$file"
done

for direc in "${dir_array[@]}"; do
    for file in "${files_array[@]}"; do
        pathvar="$direc$file"
        cat "$pathvar" >> "$file"
    done
done
