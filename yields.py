import pandas as pd
import numpy as np
import statsmodels.api as sm
from sklearn import datasets, linear_model, metrics 
from sklearn.model_selection import train_test_split
import matplotlib.pyplot as plt
import datetime

def read_yields():
  input_file = "output_test.csv"
  fieldOfInterest='yFYield_CSUSHPINSA'
  #https://stackoverflow.com/questions/36519086/pandas-how-to-get-rid-of-unnamed-column-in-a-dataframe/36519122
  df = pd.read_csv(input_file, header = 0, index_col=0)
  #append if desire a sample
  #.sample(50, axis=1)
  #attempts at trying to count 0's and drop columns have failed in R, so I'm hard coding the column drop here
  x = df.drop(columns=['test2_z.date','CSUSHPINSA','future'])
  y = df.loc[0:,['CSUSHPINSA']]
  date = df['test2_z.date']
  xLagged = x.shift(+1)
  yLagged = y.shift(+1)
  yFuture = y.shift(-1)
  xYield = (x/xLagged)-1
  yYield = (y/yLagged)-1
  yFYield = (yFuture/y)-1
  symbols = [date, x, xYield,y, yYield, yFYield]
  #https://pandas.pydata.org/pandas-docs/stable/merging.html
  #Set logic on the other axes¶
  #https://stackoverflow.com/questions/17477979/dropping-infinite-values-from-dataframes-in-pandas
  xYield.columns = ['xYield_' + str(col) for col in xYield.columns]
  y.columns = ['y_' + str(col) for col in y.columns]
  x.columns = ['x_' + str(col) for col in x.columns]
  yYield.columns = ['yYield_' + str(col) for col in yYield.columns]
  yFYield.columns = ['yFYield_' + str(col) for col in yFYield.columns]
  result = pd.concat([date, xYield, y, yYield, yFYield], axis=1, sort=False)
  #replace inf's with 0
  result.replace(np.inf, 0, inplace=True)
  result.to_csv("output.csv", sep=',')
  #plt.matshow(result.corr())
  #https://stackoverflow.com/questions/29432629/correlation-matrix-using-pandas
  corr = result.corr()
  #matches corr formula in excel
  #matches formula here: https://libguides.library.kent.edu/SPSS/PearsonCorr
  #abs(corr)
  upperLimit = max(abs(corr[fieldOfInterest]).mean(),abs(corr[fieldOfInterest]).median())
  #upperLimit
  #https://stackoverflow.com/questions/42613467/how-to-select-all-rows-which-contain-values-greater-than-a-threshold
  upper=corr[fieldOfInterest][abs(corr[fieldOfInterest]) > upperLimit]
  #https://stackoverflow.com/questions/26640145/python-pandas-how-to-get-the-row-names-from-index-of-a-dataframe
  upperList = upper.axes[0].tolist()
  #filters columns
  upperSet=corr[upperList]
  #column means
  lowerLimit = min(abs(upperSet).mean().mean(),abs(upperSet).median().median())
  LowerList1 = abs(upperSet).mean()[abs(upperSet).mean() < lowerLimit].axes[0].tolist()
  LowerList2 = abs(upperSet).median()[abs(upperSet).median() < lowerLimit].axes[0].tolist()
  yFieldList = fieldOfInterest
  finalSet=set(LowerList1)&set(LowerList2)
  #odd example of how to print by row and column name
  corrSet = corr.loc[finalSet][list(finalSet)]
  corrSet.to_csv("corr.csv", sep=',')
  #https://stackoverflow.com/questions/30405413/python-pandas-extract-year-from-datetime-dfyear-dfdate-year-is-not
  pDates = pd.to_datetime(date)
  qDates = pDates.dt.quarter
  #result
  finResult = pd.concat([date,result[list(finalSet)], yFYield], axis=1, sort=False)
  #https://chrisalbon.com/python/data_wrangling/pandas_create_column_using_conditional/
  finResult['Q1'] = np.where(qDates==1, 1, 0)
  finResult['Q2'] = np.where(qDates==2, 1, 0)
  finResult['Q3'] = np.where(qDates==3, 1, 0)
  finResult['Q4'] = np.where(qDates==4, 1, 0)
  abs(upperSet).median()[abs(upperSet).median() < lowerLimit].axes[0].tolist()
  #https://stackoverflow.com/questions/19913659/pandas-conditional-creation-of-a-series-dataframe-column
  #df['color'] = np.where(df['Set']=='Z', 'green', 'red')
  #adds binary logistic categorical variable
  finResult['BL_'+ fieldOfInterest] = np.where(finResult[fieldOfInterest] > 0 , 1, 0)
  #from 1st non na to last non na.  na.omit in R
  finResult.dropna().to_csv("prepped.csv", sep=',', index=False)
  return
