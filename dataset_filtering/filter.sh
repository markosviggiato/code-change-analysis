#!/bin/bash

# set directory where repository will be cloned
cd "C:\Users\markos\Desktop"

url="https://github.com/"
url2=".git"

while IFS='' read -r line || [[ -n "$line" ]]; do
	echo $line
	repo_raw_name="${line//\"}"
	echo $repo_raw_name
	
	# clone repository and change to its directory
	repo=$repo_raw_name
	urlToClone="$url$repo$url2"
	echo $urlToClone
	repoName=${repo##*/}
	echo $repoName

	git clone "$urlToClone"
	cd $repoName
	
	# calculate the number of source lines of code (SLOC)
	total_loc=0
	all_files="$(find . -name "*.java")"
	for entry in $all_files
	do
		echo "$entry"
		
		"C:\Users\markos\Desktop\cloc" "$entry" --fullpath --not-match-f='/[Tt][Ee][Ss][Tt][Ee]?[Ss]?/'  --quiet --csv  > logLOC.log
		
		    {
        read 
        read
        count_files=0
        count_lines=0
		bool="false"
        while IFS=“,”, read f1 f2 f3 f4 f5
        do
			bool="true"
            (( count_lines += f5 ))
		(( total_loc += f5 ))
        echo "$line - $f5"$'\r'
	    echo "$line - $f5"$'\r' >> outLOC.txt
		done }< logLOC.log
		
		if [ "$bool" = "false" ]
			then
				echo "$line - ZERO"$'\r' >> exceptionsLOC.txt
				echo "$line - ZERO"$'\r' >> outLOC.txt
			fi
	done
	echo $total_loc
	
	if [ $total_loc -lt 1000 ]
	then
	
	# delete repository
	cd ..; rm -rf -- $repoName
	
	else
	
	# search for AndroidManifest.xml file
	t="$(find "$(pwd)" -type f -name "*Manifest.xml")"
	siz=${#t}
	echo $siz
	if [ $siz != 0 ]
	then
	echo true
	echo there is manifest
	
	# write loc to a file
	cd ..
	echo "$repo_raw_name,$total_loc"$'\r' >> loc_android.txt
	rm -rf -- $repoName
	else 
	echo false
	echo there is NO manifest
	
	# write loc to a file
	cd ..
	echo "$repo_raw_name,$total_loc"$'\r' >> loc_desktop.txt
	rm -rf -- $repoName	
	fi
	
	fi

done < "repo.txt"