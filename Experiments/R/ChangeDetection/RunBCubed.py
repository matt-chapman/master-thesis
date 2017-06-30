import csv
import bcubed
import sys


def calculate_bcubed():

    with open(str(sys.argv[1])) as predictions, open('GroundTruthClusters.csv') as labels:
        predictions.readline()
        reader = csv.reader(predictions)
        clustering = dict((rows[0], set([rows[1]])) for rows in reader)
        # print clustering

        labels.readline()
        reader = csv.reader(labels)
        truth = dict((rows[0], set([rows[1]])) for rows in reader)

        precision = bcubed.precision(clustering, truth)
        recall = bcubed.recall(clustering, truth)
        fscore = bcubed.fscore(precision, recall)

        print precision
        print recall
        print fscore

if __name__ == '__main__':
    calculate_bcubed()
