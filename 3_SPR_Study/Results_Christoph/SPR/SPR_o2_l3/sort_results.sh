# Expects 'results' as input file
# Sorts results recorded by Ibex into plausibility ratings, demographics, consent
# Removes practice items, as well as commented lines

# clear output files
cp -v demog.txt attic/demog.txt
cp -v consent.txt attic/consent.txt
cp -v survey.txt attic/survey.txt
cp -v reading.txt attic/reading.txt
cp -v task.txt attic/task.txt
rm -v demog.txt consent.txt survey.txt reading.txt task.txt

# Make a working copy of input file
cp -v results.csv results_wc

# remove comments
sed -i '/#/d' results_wc
# remove first information screen
sed -i '/consent,/d' results_wc
# remove practice
sed -i '/practice/d' results_wc
sed -i '/practice_feedback/d' results_wc
# remove instructions
sed -i '/instruction/d' results_wc
# remove block feedback
sed -i '/block1_feedback/d' results_wc
sed -i '/block2_feedback/d' results_wc
sed -i '/block3_feedback/d' results_wc

while read line
do
# echo -e "$line \n"
if [[ $line == *,demographics,* ]]; then 
	echo $line >> demog.txt
elif [[ $line == *,consent2,* ]]; then 
	echo $line >> consent.txt
elif [[ $line == *,postexp_survey,* ]]; then 
	echo $line >> survey.txt
elif [[ $line == *,DashedSentence,* ]]; then 
	echo $line >> reading.txt
else 
	echo $line >> task.txt
fi
done < results_wc
