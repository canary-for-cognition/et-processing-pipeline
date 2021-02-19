import pandas as pd
import glob
import numpy as np
import geopandas as gp
from shapely.geometry import Point, Polygon
from pandas.core.common import flatten
from functools import reduce
import os

# myPoint takes an integer and converts it into a shapley geometry point
def myPoint(n):
    return Point(n)

# This function will compute hits and latencies or just hits
# ComputVals takes a backwindow and frontwindow integer, a dataframe containing only AOI's, the comple dataframe, the patient name as a string and a mode that can be 'full'
# for hits and latencies or 'hits for just hits
def ComputeVals(backWindowVal, frontWindowVal, dfAOI, df, polys, patient,mode):
    #Define the backwindow and front window not necessary but it helps for clarity
    backWindow = backWindowVal
    frontWindow = frontWindowVal 
    #Create a marker to split into consecutive AOI groups
    dfAOI['marker'] = (dfAOI['InfoUnit'] != dfAOI['InfoUnit'].shift()).cumsum()
    #Add the first and last of every group into our df_master
    df_master = dfAOI.index.to_series().groupby(dfAOI['marker']).agg(['first','last']).reset_index()
    #Add a new column with the patient name on our df_master
    df_master['PATIENT'] = patient
    #Find the  RecordingTimestamp for every first and last index in our df_master and store it in their respective df's
    df_firstIndexTime = df.loc[df_master['first'], ['RecordingTimestamp']]
    df_lastIndexTime = df.loc[df_master['last'], ['RecordingTimestamp']]
    #Add the RecordingTimestamps to our df_master
    df_master= df_master.assign(recordingTimeStampStart=df_firstIndexTime.values,recordingTimeStampEnd=df_lastIndexTime.values)
    #Assign the target for looking back and front by subtracting the back window and adding the front window, then assigning it to our df_master
    df_master=df_master.assign(timeTargetBack=df_master['recordingTimeStampStart'].values-backWindow,timeTargetFront=df_master['recordingTimeStampEnd'].values+frontWindow)
    #Create a copy of the index in the main dataframe to avoid loosing the indices in the future
    df['copy_index'] = df.index
    #Use the function merge_asof to find the back and front closest value of from the target time stored in master_df in comparison to the RecordingTimestamp in the main dataframe
    #this will be stored in their respective new dataframes (we get a dataframe back from this function, this dataframe has only the rows of the main dataframe where we have the closest value to
    # our targets, it is important to note that since we get the row from the main df we have the correct location in the main df after merge because we have the copy of index). **It is basically a 
    # left join  but instead of equal values we match based on nearest.
    df_backward = pd.merge_asof(df_master, df, left_on='timeTargetBack',right_on='RecordingTimestamp',direction='backward')
    df_forward = pd.merge_asof(df_master,df,left_on='timeTargetFront',right_on='RecordingTimestamp',direction='forward')
    #Assign the copy of indices which are the correct ones on our df_master
    df_master=df_master.assign(backWindowIndex=df_backward['copy_index'].values)
    df_master=df_master.assign(frontWindowIndex=df_forward['copy_index'].values)
    #Assign the info unit values for every index in the df_master, this could have been done earlier
    df_master=df_master.assign(InfoUnits=df.loc[df_master['first'],'InfoUnit'].values)
    #Create a list to store the smaller dataframes from the back to front windows
    df_list = []
    #Zip the values to create a point in the main dataframe, will be useful for later when finding if it falls on a polygon
    df['point'] = list(zip(df['FixationPointX..MCSpx.'], df['FixationPointY..MCSpx.']))
    #Append all the smaller dataframes into the list 
    for index, row in df_master.iterrows():
        df_list.append(df.loc[row['backWindowIndex'] : row['frontWindowIndex']].drop_duplicates('FixationIndex'))
    #Iterate through the df_list and create a new dataframe of true and false values based on which polygon every row lands on, append this to a list of hits.
    hitsList = []
    for x in df_list:
        pointList = list(map(myPoint, (x['point'].values)))
        _pnts = pointList
        pnts = gp.GeoDataFrame(geometry=_pnts)
        hitsList.append(pnts.assign(**{key: pnts.within(geom) for key, geom in polys.items()}))
    resList = []
    #We iterate through the true false dataframes and we just keep the truth values as a list
    for data in hitsList:
        res = list(flatten(pd.DataFrame(data.columns.where(data == True).tolist()).values.tolist()))
        ans = [x for x in res if not isinstance(x, float)]
        noDuplicate = pd.Series(ans).drop_duplicates()
        resList.append(list(noDuplicate))
    #Store the hists into the df_master
    hitName = 'HITS' + '_' + str(backWindow)
    df_master[hitName] = resList
    hitToCSV = df_master[['PATIENT', 'InfoUnits', hitName]]
    #If we just call for hits just return the hits dataframe
    if(mode == 'hits'):
        return hitToCSV
    #Else we call full and now we calculate the latencies    
    elif(mode == 'full'):
        df_latency = df_master
        #Drop undefined AOI's
        # df_latency.drop(df_latency.loc[df_latency['InfoUnits']=='KITCHEN'].index, inplace=True)
        # df_latency.drop(df_latency.loc[df_latency['InfoUnits']=='EXTERIOR'].index, inplace=True)
        # df_latency.drop(df_latency.loc[df_latency['InfoUnits']=='CUPBOARD'].index, inplace=True)
        #Call the computeLatency function for back and front
        latencyValuesBack = computeLatency(df,polys,df_latency, 'back') 
        latencyValuesFront = computeLatency(df,polys,df_latency, 'front')
        df_master['BackLatency'] = latencyValuesBack[0]
        df_master['BackIndex'] = latencyValuesBack[1]
        df_master['FrontLatency'] = latencyValuesFront[0]
        df_master['FrontIndex'] = latencyValuesBack[1]
        latencyToCSV = df_master[['PATIENT', 'InfoUnits', 'BackLatency', 'FrontLatency']]
        patientResult = pd.merge(latencyToCSV, hitToCSV, right_index=True, left_index=True)
        patientResult.reset_index()
        resultsToCSV = patientResult.drop(['PATIENT_y','InfoUnits_y'], axis=1)
        resultsToCSV = resultsToCSV.rename(columns={'PATIENT_x': 'Patient', 'InfoUnits_x': 'InfoUnits'})
        return resultsToCSV
#Compute the latencies we take the main dataframe the polygons, the df_latency which is the df_master without unwanted AOI'S
def computeLatency(df,polys,df_latency,direction):
    df_listLatency = []
    df_indices = []
    res_list = []
    #Iterate through every row in df_latency we drop duplicates and we define a subset of the dataframe based on the values given by first and last in df_master = df_latency, so from back is
    #from the mention of the InfoUnit until the first row of the main df and for front it is from last mention of InfoUnit until last row of the main dataframe, without duplicates.
    #**For every row we create a sub df
    #We can potentially generate the sub_df's iteratively to save space in the ComputeVals function like we do in here to save space if needed, instead of storing all sub_df's in one place
    for index, row in df_latency.iterrows():
        if(direction == 'back'):
            sub_df = df.loc[0 : row['first'] - 1].drop_duplicates('FixationIndex')
        elif(direction == 'front'):
            sub_df = df.loc[row['last'] + 1 : len(df)].drop_duplicates('FixationIndex')
        #We create a true false table for every polygon for every row in the subset of the main dataframe = sub_df
        pointList = list(map(myPoint, (sub_df['point'].values)))
        _pnts = pointList
        pnts = gp.GeoDataFrame(geometry=_pnts)
        df_table = pnts.assign(**{key: pnts.within(geom) for key, geom in polys.items()})
        #We find the exact polygon in each for each row of the df_master = df_latency and we store it in exactPoly dataframe
        exactPoly =  df_table[['geometry', row['InfoUnits']]]
        #Finally we just find the last valid and first valid index in the exactPoly dataframe, we make the calculation for RecordingTimestamp and we append it to a list 
        if(direction == 'back'):
            lastHit = exactPoly.where(exactPoly == True).last_valid_index()
            if(lastHit != None):
                res = df.loc[row['first']]['RecordingTimestamp'] - sub_df.iloc[lastHit]['RecordingTimestamp'] 
                df_indices.append(sub_df.iloc[lastHit]['copy_index'])   
            else:
                df_indices.append(-1)
                res = -1
        elif(direction == 'front'):
            firstHit = exactPoly.where(exactPoly == True).first_valid_index()
            if(firstHit != None):
                res = sub_df.iloc[firstHit]['RecordingTimestamp'] - df.loc[row['last']]['RecordingTimestamp']
                df_indices.append(sub_df.iloc[firstHit]['copy_index'])   
            else:
                df_indices.append(-1)
                res = -1
        df_listLatency.append(res)
    res_list =[df_listLatency, df_indices]
    return res_list

def main():
    #PATH FOR INPUT FILES HERE!!!!!
    path1 = "/Users/obarral/Documents/CANARY/Data/PRE-LOCKDOWN-ALL-DATA/Preprocessing/Eye_with_infoUnit" 
    all_files = glob.glob(path1 + "/*.csv")

    polys = gp.GeoSeries({
    'BOY': Polygon([(590,165),(680,200),(605,655),(495,617),(510,300),(595,170)]),
    'JAR': Polygon([(400,115),(520,115),(520,235),(400,235)]),
    'COOKIE': Polygon([(420,260),(475,260),(475,320),(420,320)]),
    'STOOL': Polygon([(495,617),(605,655),(522,878),(438,869),(386,788)]),
    'GIRL': Polygon([(255,444),(452,350),(413,739),(386,788),(420,845),(420,900),(325,910)]),
    'WOMAN': Polygon([(920,145),(1020,145),(1045,350),(985,395),(990,455),(950,500),(990,565),(1040,560),(1065,670),(1010,880),(890,890),(830,550)]),
    'PLATE': Polygon([(1045,350),(1110,365),(1110,415),(1060,460),(990,455),(985,395)]),
    'DISHCLOTH': Polygon([(1060,460),(1040,560),(990,565),(950,500),(990,455)]),
    'CURTAINS': Polygon([(970,145),(970,90),(1480,90),(1480,615),(1385,575),(1425,445),(1285,230),(1250,120),(1210,230),(1080,355),(1045,350),(1020,145)]),
    'WINDOW': Polygon([(1250,120),(1285,230),(1425,445),(1385,575),(1055,510),(1060,460),(1110,415),(1110,365),(1089,355),(1210,230)]),
    'SINK': Polygon([(1055,510),(1350,565),(1222,672),(1055,605),(1040,560)]),
    'WATER': Polygon([(1222,672),(1155,990),(890,990),(830,940),(890,890),(1010,880),(1065,670),(1055,605)]),
    'DISHES': Polygon([(1300,625),(1480,625),(1480,710),(1300,710)])
    })
    allOutputs = []
    for file in all_files:
        path = file
        patient = os.path.basename(path).replace('.csv', '')
        df = pd.read_csv(path)
        dfAOI = df[df["InfoUnit"].notna()]
        if dfAOI.empty:
            print('participant: '+ patient +" has no InfoUnit mentions!")
            allOutputs.append(pd.DataFrame([[patient,-1,-1,-1,[]]],columns=['Patient','InfoUnits','BackLatency','FrontLatency','HITS_0']))
            continue
        #TO COMPUTE
        a = ComputeVals(0, 0, dfAOI, df, polys, patient, 'full')
        # TO ADD EXTRA HIT COLUMNS MAKE A CALL USING 'hits' on the mode paramenter and add them to a list in the following way
        # b = ComputeVals(500,500, dfAOI, df,polys, patient,'hits')
        # c = ComputeVals(1000,1000, dfAOI, df, polys, patient,'hits')
        # d = ComputeVals(2000,2000, dfAOI, df, polys, patient,'hits')
        # full_list = [a, b['HITS_500'], c['HITS_1000'] , d['HITS_2000']]
        # resultsToCSV = reduce(lambda  left,right: pd.merge(left,right,right_index=True, left_index=True), full_list)
        # allOutputs.append(resultsToCSV)
        allOutputs.append(a)
        print('participant: '+ patient +" processed")

    pd.concat(allOutputs).to_csv('/Users/obarral/Documents/CANARY/Diego/multimodal/MultimodalFeatures.csv',encoding='utf-8')    

if __name__ == "__main__":
    main()