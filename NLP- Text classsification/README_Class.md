# Multi-Class Text Classification Using PySpark

**Project Description:**

In this project, we will build Logistic regression, Naive Bayes and Random Forest models to classify San Francisco Crime Description into 33 pre-defined categories. 

**Data Dictionary:**

All datasets can be dowloaded from repository that I have created on GitHub. This dataset contains incidents derived from SFPD Crime Incident Reporting system. The data ranges from 1/1/2003 to 5/13/2015. The training set and test set rotate every week, meaning week 1,3,5,7... belong to test set, week 2,4,6,8 belong to training set. 

Data fields

Dates - timestamp of the crime incident

Category - category of the crime incident (only in train.csv). This is the target variable you are going to predict.

Descript - detailed description of the crime incident (only in train.csv)

DayOfWeek - the day of the week

PdDistrict - name of the Police Department District

Resolution - how the crime incident was resolved (only in train.csv)

Address - the approximate street address of the crime incident 

X - Longitude

Y - Latitude
