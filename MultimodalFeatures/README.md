Pipeline to generate multimodal features. Each step takes the output of previous step as input:

1. Run MultimodalFeatures-Preprocessing.R (Oswald' Script):
This file takes as input the transcripts and raw Eye tracking Data, and generates a set of Synchronized eye-tracking files in which an extra column is added to the Tobii exports. This column indicates, for each eye tracking sample (row) whether an info unit was being mentioned at that time.  

2. Run GenerateMultimodal.py (Diego's script): 
This script generates a single file that includes, for each participant and infounit mentioned, a set of multimodal features. (e..g, latency between mention and gaze on infunit "Boy")

3. Run MultiModalFeaturesSummative.R (Oswald's Script):
This file generates a single file that includes, for each user, a set of features that have been aggregated based on summative statistics of the individual infounit features for each user. (e.g., average latency between mention and gaze for all info units mentioned by a given user)

