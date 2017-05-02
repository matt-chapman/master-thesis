# Evaluation Pipeline Design

## Description of Existing Methods

### Buntain et. al. (A brief comparison of algorithms for detecting change points in data)

Buntain et. al calculate F1 score (precision) for two data sets, one with a change in covariance, one with a change in mean (and constant covariance). They also explore "sensitivity to dimensionality" - though this would not likely be practical for my purposes.

Finally, they assess effectiveness of the algorithms against data sets with multiple change points distributed through the data set.

CUSUM was found to be the most effective and accurate algorithm.

### Downey (A novel change detection algorithm)

A simple 'does it detect the change' test. Acknowledges in conclusions that evaluations on the algorithms performance would need to be carried out.

### Matteson et. al. (A nonparametric approach for multiple change point analysis of multivariate data)

Calculates the 'Rand Index' and 'Morey and Agresti's' Adjusted Rand Index. These show a measure of similarity between two different partitions of the same observations.

The first is for comparing an estimated set of change points to a baseline or known set, the second for comparing two sets of estimated change points.

See:

* Rand, W. M. (1971), “Objective Criteria for the Evaluation of Clustering Methods,” Journal of the American Statistical Association, 66, 846 – 850.
* Morey, L. C., and Agresti, A. (1984), “The Measurement of Classification Agreement: An Ad- justment to the Rand Statistic for Chance Agreement.,” Educational and Psychological Mea- surement, 44, 33 – 37.

### TREC2016 Evaluation

4 generalised measures: Expected Gain, Normalized Cumulative Gain, Gain Minus Pain and Latency.

Important to note that these measures concern (for the most part) the relevance of a given tweet. Not immediately useful to this project, but could be adapted to provide a meaningful scoring mechanism.

Gain calculated as a measure of relevance - non relevance nets a 0.0, relevance nets a 0.5, high relevance nets a 1.0.

## Suggestions

* Adapt TREC2016 scoring - altering gain calculations to swap relevancy of a given tweet to relevancy of a detection. We need some banded threshold values to be able to define "relevance" mathematically. Perhaps some function of spike size?

* Measure latency between actual (human annotated) spike, and detection. Define minimum and maximum values for this and score accordingly. Early effective detections should be rewarded, late detections penalised.

* Can I find algorithm implementations in R? There is a nice binding library in ```rpy2``` and R seems to be the current method in _vogue_ for statistical analysis.

## Baseline

* Implement static mean method
* Implement Z-Score change detection (distance from the sample mean to the population mean in units of the standard error) with windows - see [IPYNB](https://nbviewer.jupyter.org/github/amanahuja/change-detection-tutorial/blob/master/ipynb/section_03_Windows.ipynb)

The two methods above will show:

* Ineffective detections as noise increases (static mean)
* Late (thus, useless) detections (Z-Score & Window)
