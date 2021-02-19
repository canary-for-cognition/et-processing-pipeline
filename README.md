# ET-Processing-Pipeline
This Repository contains several scripts mostly written by Oswald that are used to pre- and post - process eye tracking data for the CANARY project

* GenerateAuxiliaryFiles.R --> Script that takes all tobii exports, participantLog, and CookieTheftAoi template as inputs, and generates the following outputs:
  * Raw eye tracking data where for mislabeled users, the filenames and content have been corrected with StudyID
  * CookieTheft AOI files for EMDAT
  * Segment files for EMDAT
  * A file with the pupil baselines for EMDAT
  * A file that includes the tasks timestamps 
  * A log indicating if there were any errors processing the files, and some stats
  * Converts the participant log into a csv file

* AOIs --> Folder that contain the information for defining AOIs for the Cookie theft and Reading Tasks. Note that GenerateAuxiliaryFiles.R takes the file under CookieTheft > All_AOIs.txt as input in order to generate its output for EMDAT

* MultimodalFeatures --> Scripts related to generating multimodal features for CookieTheft task. Find dedicated readme in subfolder

* ReadingTaskFeatures --> Scripts related to generating eye tracking features for the reading task. Find dedicated readme in subfolder
