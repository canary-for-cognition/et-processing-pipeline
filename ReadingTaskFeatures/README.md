Pipeline to generate eye tracking features for the Reading Task. Each step takes the output of previous step as input:

1. Run ReadingTaskScript.py (Diego's Script):
 This script takes as input the eye tracking data and generates a set of values that will be used in order to generate the eye tracking features for the reading task, as per Fraser's paper

2. Run MultiModalFeaturesSummative.R (Oswald's Script):
This script takes the output of the previous script, and generates all features in Fraser's "Predicting MCI status from multimodal language data using cascaded classifiers (2109)" paper

