# Expects 'results' as input file
# Sorts results recorded by Ibex into plausibility ratings, demographics, consent
# Removes practice items, as well as commented lines

# clear output files
cp -v rating.txt attic/rating.txt
cp -v demog.txt attic/demog.txt
cp -v consent.txt attic/consent.txt
rm -v rating.txt demog.txt consent.txt

# Make a working copy of input file
cp -v results_all.csv results_wc

# remove practice
sed -i  '/practice/d' results_wc
# remove comments
sed -i  '/#/d' results_wc


while read line
do
# echo -e "$line \n"
if [[ $line == *,demographics,* ]]; then 
	echo $line >> demog.txt
elif [[ $line == *,Consent,* ]]; then 
	echo $line >> consent.txt
else 
	echo $line >> rating.txt
fi
done < results_wc
