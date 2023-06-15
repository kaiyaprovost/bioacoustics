import pandas as pd
import io
import sys

# for i in /Users/kprovost/Documents/Postdoc_Working/Finished_Models/*_trainlog_*txt; do python3 /Users/kprovost/Documents/GitHub/bioacoustics/tweetynet/tweetynet_trainlog_accuracy.py $i; done;
# for i in /Users/kprovost/Documents/Postdoc_Working/Finished_Models/*_learncurvelog_*txt; do python3 /Users/kprovost/Documents/GitHub/bioacoustics/tweetynet/tweetynet_trainlog_accuracy.py $i; done;
# for i in /Users/kprovost/Documents/Postdoc_Working/Finished_Models/*/*/train*log; do python3 /Users/kprovost/Documents/GitHub/bioacoustics/tweetynet/tweetynet_trainlog_accuracy.py $i; done;
# for i in /Users/kprovost/Documents/Postdoc_Working/Finished_Models/*/*/learncurve*log; do python3 /Users/kprovost/Documents/GitHub/bioacoustics/tweetynet/tweetynet_trainlog_accuracy.py $i; done;

try:
	trainfile = str(sys.argv[1])
	print("\tFile is: ",trainfile)
except:
	print("Filename not given, quitting")
	exit()
	trainfile="/Users/kprovost/Documents/Postdoc_Working/Finished_Models/Melozone.fusca/MF_trainlog_2022-09-08_16-14-37.txt"
	
## extract accuracy from training log
with open(trainfile,"r") as infile:
	lines=infile.readlines()

matching = [i for i in lines if "avg_acc" in i]

with open(trainfile+".ACCURACY.temp","w") as outfile:
	_ = outfile.write("".join(matching))