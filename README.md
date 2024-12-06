**##Capstone Project Fraud Detection Using R and Power Bi**


**Machine Learning Capstone project: Credit Card Approval.**

**Problem:** Leveraged machine learning in R Studio to predict creditworthiness in a 1 million record dataset.

**Challenges:** Highly imbalanced data (99.5% creditworthy customers) hindered model performance, and data did not consist of a dependent variable so I had to perform  feature engineering to create a dependent to be able to perform supervised learning techniques for predictive modeling such as regression, classification tree, and KNN.

**Solution:** Implemented KNN, Logistic Regression, and Classification trees with ensembles (Random Forest, XGBoost) for creditworthiness prediction. Employed data cleaning, feature engineering, and various sampling techniques (under, over, synthetic, SMote) to address data imbalance.  Engineered a dependent variable through feature engineering by pivoting the data longer and summarising how many months customers were overdue in a year. The metric we used was if a customer was overdue 6 months in one year they were not credit-worthy and were considered delinquent.

**Outcome:** Created a model with accuracy (95%) but models couldn't outperform the NIR baseline accuracy (99.5%), so I created a novel feature predicting "late fee " customers to gain valuable insights despite having severe data limitations 

