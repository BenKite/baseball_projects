## Ben Kite

import pandas, numpy
import statsmodels.api as sm
from sklearn import linear_model
import matplotlib.pyplot as plt
import sklearn
from sklearn.ensemble import RandomForestRegressor
from sklearn.neural_network import MLPRegressor

dat = pandas.read_csv("TeamBatting.csv")

dat = dat.loc[dat.Rk.notnull()]

def datArrange(name, year, dat):
    pyear = year - 1
    #pdat = dat.loc[(dat["Name"] == name) & (dat["Year"] <= pyear)]
    #if numpy.sum(pdat["AB"] > 0):
    #    careerBA = numpy.sum(pdat["H"])/numpy.sum(pdat["AB"])
    #else:
    #    careerBA = 0
    predictors = dat.loc[(dat.Name == name) & (dat.Year == pyear)]
    predictors = predictors.reset_index()   
    predictors = predictors[["Name", "Age", "G", "PA", "AB", "OPS", "BA", "HR", "2B", "3B", "BB", "SO", "SLG", "Team"]]
    predictors.columns = ["Name", 
                          "Age_previous",
                          "G_previous",
                          "PA_previous",
                          "AB_previous",
                          "OPS_previous",
                          "BA_previous", 
                          "HR_previous",
                          "2B_previous",
                          "3B_previous", 
                          "BB_previous", 
                          "SO_previous", 
                          "SLG_previous",
                          "Team_previous"]

    outcomes = dat.loc[(dat.Name == name) & (dat.Year == year)]
    outcomes = outcomes.reset_index()
    outcomes = outcomes[["G", "BA", "OPS", "HR", "SLG", "Team"]]
    outcomes.columns = ["G", "BA", "OPS", "HR", "SLG", "Team"]

    out = pandas.concat([predictors, outcomes], axis = 1)
    
    out["BA_change"] = out["BA"] - out["BA_previous"]
    out["OPS_change"] = out["OPS"] - out["OPS_previous"]
    out["SLG_change"] = out["SLG"] - out["SLG_previous"]
    
    careerdat = dat.loc[(dat["Name"] == name) & (dat["Year"] < year)]
    cba = numpy.mean(careerdat["BA"])
    cops = numpy.mean(careerdat["OPS"])
    cslg = numpy.mean(careerdat["SLG"])
    
    out["Average_BA"] = cba
    out["Average_OPS"] = cops
    out["Average_SLG"] = cslg
    out["AboveAverage_BA"] = numpy.where((out["BA_previous"] > out["Average_BA"]), 1, 0)
    out["AboveAverage_OPS"] = numpy.where((out["OPS_previous"] > out["Average_OPS"]), 1, 0)
    out["AboveAverage_SLG"] = numpy.where((out["SLG_previous"] > out["Average_SLG"]), 1, 0)
    out["Team_Change"] = numpy.where(out["Team"] == out["Team_previous"], 0, 1)
    #out["CareerBA"] = careerBA
    out["Dropoff"] = numpy.where((out["BA"] < out["BA_previous"]) & (out["OPS"] < out["OPS_previous"]) & (out["SLG"] < out["SLG_previous"]), 1, 0)
    out["year"] = year
    return(out)
    
years = list(range(2000,2015))
    
mdat = dict()  
for y in years:
    tmpdat = dat.loc[dat["Year"] == y]
    for n in numpy.unique(tmpdat["Name"]):
        mdat[n, y] = datArrange(n, y, dat)
        
mdat = pandas.concat(mdat)
mdat = mdat.loc[mdat["G"] > 50]
mdat = mdat.loc[mdat["G_previous"] > 50]
mdat = mdat.loc[mdat["PA_previous"] > 100]
mdat = mdat.loc[(mdat.BA.notnull()) & (mdat.BA_previous.notnull())]
mdat.reset_index(drop = True, inplace = True)
mdat.to_csv("train.csv")


ayear = 2016

adat = dict()  
tmpdat = dat.loc[dat["Year"] == ayear]
for n in numpy.unique(tmpdat["Name"]):
    adat[n] = datArrange(n, ayear, dat)
        
adat = pandas.concat(adat)
adat = adat.loc[adat["G"] > 50]
adat = adat.loc[adat["G_previous"] > 50]
adat = adat.loc[(adat.BA.notnull()) & (adat.BA_previous.notnull())]
adat.reset_index(drop = True, inplace = True)
adat.to_csv("test.csv")