import csv
import bcubed
import pprint as pp

clustering = {}
truth = {}

with open('RaboClusters_Var_Pelt.csv') as f:
    f.readline()
    reader = csv.reader(f)
    clustering = dict((rows[0],set(rows[1])) for rows in reader)
    # print clustering

with open('RaboClusters_Truth.csv') as f:
    f.readline()
    reader = csv.reader(f)
    truth = dict((rows[0],set(rows[1])) for rows in reader)
    # print truth

# print clustering
# print truth

precision = bcubed.precision(clustering, truth)
recall = bcubed.recall(clustering, truth)
fscore = bcubed.fscore(precision, recall)

pp.pprint(clustering)
pp.pprint(truth)

print "BCubed Precision: {0}".format(precision)
print "BCUbed Recall: {0}".format(recall)
print "BCubed F-Score: {0}".format(fscore)
