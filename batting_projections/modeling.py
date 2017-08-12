## Ben Kite

import pandas, numpy
import statsmodels.api as sm
from sklearn import linear_model
import sklearn
from sklearn.ensemble import RandomForestRegressor
from sklearn.neural_network import MLPRegressor

train = pandas.read_csv("train.csv")
test = pandas.read_csv("test.csv")

preds = ["Age_previous", "AB_previous", "OPS_previous", "BA_previous", "HR_previous", "2B_previous", "3B_previous", "BB_previous", "SO_previous", "SLG_previous",
         "Average_BA", "Average_OPS", "Average_SLG"]            
outcome = "BA"   

clf = RandomForestRegressor()
clf.fit(train[preds], train[outcome])
test["Predicted" + outcome] = clf.predict(test[preds])













