import csv
import bcubed
import sys


clustering = {}
truth = {}

with open(str(sys.argv[1])) as f:
    f.readline()
    reader = csv.reader(f)
    clustering = dict((rows[0], set(rows[1])) for rows in reader)
    # print clustering

with open('GroundTruthClusters.csv') as f:
    f.readline()
    reader = csv.reader(f)
    truth = dict((rows[0], set(rows[1])) for rows in reader)


precision = bcubed.precision(clustering, truth)
recall = bcubed.recall(clustering, truth)
fscore = bcubed.fscore(precision, recall)

print precision
print recall
print fscore
