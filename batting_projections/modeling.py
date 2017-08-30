## Ben Kite

import pandas, numpy, sys
from sklearn import linear_model
import sklearn
from sklearn.ensemble import RandomForestRegressor
from sklearn.neural_network import MLPRegressor
from sklearn.ensemble import GradientBoostingRegressor
from sklearn.metrics import r2_score

train = pandas.read_csv("train.csv")
test = pandas.read_csv("test.csv")

preds = ["Age_previous", "AB_previous", "OPS_previous", "BA_previous", "HR_previous", "2B_previous", "3B_previous", "BB_previous", "SO_previous", "SLG_previous",
         "Average_BA", "Average_OPS", "Average_SLG", "AboveAverage_BA", "AboveAverage_OPS", "AboveAverage_SLG", "Team_Change"]            
outcome = "BA"   

## Crossvalidate the model using ml-tools
## I have ml-tools clone from github.com/BenKite/ml-tools.git
sys.path.append('../../ml-tools')
import cross_validate as cv

## Test a GBR model
## This section takes 10 minutes
##ds = [1, 2, 3, 4, 5]
##es = [25, 50, 100, 150]
##values = []
##for d in ds:
##    for e in es:
##        values.append([d, e])
##model = "GradientBoostingRegressor"
##param = ["max_depth", "n_estimators"]
##outputGBR = cv.tuner(param, model, values, preds, outcome, train, "r2", propTrain = .90, maxTime = 10)
##cv.modelSelect(outputGBR).sort_values("mean fit")

## RandomForest
## This section takes 10 minutes
##RandomForestRegressor()
##ms = [1, 2, 3, 4, 5]
##ns = [25, 50, 100, 150]
##values = []
##for m in ms:
##    for n in ns:
##        values.append([m, n])
##model = "RandomForestRegressor"
##param = ["max_depth", "n_estimators"]
##outputRFR = cv.tuner(param, model, values, preds, outcome, train, "r2", propTrain = .90, maxTime = 10)
##cv.modelSelect(outputRFR).sort_values("mean fit")


## It appears that a GBR model with a depth of 4 and 50 estimators is the best.
## More testing should be done to better tune the model.

clf = GradientBoostingRegressor(max_depth = 4, n_estimators = 50)
clf.fit(train[preds], train[outcome])
test["Prediction"] = clf.predict(test[preds])
imps = clf.feature_importances_
x = dict()
for i in range(0, len(imps)):
    x[i] = {preds[i]:imps[i]}
x

## Remove the cases with low values
test = test.loc[test["BA"] > .100]
test = test.loc[test["BA_previous"] > .100]

import matplotlib.pyplot as plt
## Predicted vs observed
plt.scatter(test["Prediction"], test["BA"])
## R-squared
numpy.corrcoef(test["BA"], test["Prediction"])[1,0]**2
#r2_score(test["BA"], test["Prediction"])

## What if we just look at the previous season?
plt.scatter(test["BA_previous"], test["BA"])
## R-squared
numpy.corrcoef(test["BA"], test["BA_previous"])[1,0]**2
#r2_score(test["BA"], test["BA_previous"])

## What if we just look at career BA?
plt.scatter(test["Average_BA"], test["BA"])
## R-squared
#numpy.corrcoef(test["BA"], test["Average_BA"])[1,0]**2







