"""Create histogram of bigrams from txt_file"""
import re
import collections
import os
import string
import sys
import getopt
import matplotlib.pyplot as plt
def bigram(txt_file, threshold = 0):
    """Create histogram of bigrams from txt_file"""
    # read file
    file_connection = open(txt_file, "r") # open
    words = file_connection.read()  # read as single string
    file_connection.close() # close

    # split text into individual words by spaces, slashes, dashes, commas, periods, and semicolons
    words = re.split("[\n\r\t /, ;]+", words)

    # begin list of bigrams as empty
    bigrams = []

    for ndx_i in range(len(words)):
        # remove punctuation that precedes or trails words
        words[ndx_i] = words[ndx_i].strip(string.punctuation)
        words[ndx_i] = re.sub(r"\.+$", "", words[ndx_i])

    words = [m for m in words if m != '']
    # iterate through words to build bigrams
    for ndx_i in range(1, len(words)):
        bigrams.append(" ".join([words[ndx_i-1],words[ndx_i]]))
    
    # Obtain tallies for the unique bigrams 
    bigrams = collections.Counter(bigrams)

    # eliminate bigrams that fall below threshold
    { k:v for k, v in bigrams.items() if threshold <= v }

    # create png file named the same as the txt file
    if not os.path.isdir("gallery"):
        os.makedirs("gallery")
    png_file = "_".join(["gallery/py",re.sub("txt$", "png", txt_file)])

    # create histogram
    plt.hist(bigrams.values(), color="green", density=True)
    plt.title(" ".join(["Histogram of bigrams in", txt_file, "- python"]))
    plt.xlabel("Tallies")
    plt.ylabel("Probability Density")
    plt.savefig(png_file)

    # print name of png file
    print(png_file)

# arguments from command line
try:
    ARGS = list(getopt.getopt(sys.argv, "ho:v"))[1][1]
    bigram(ARGS)
except:
    print("no arguments from command line")

