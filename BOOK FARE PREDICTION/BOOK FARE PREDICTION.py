#!/usr/bin/env python
# coding: utf-8

# In[1]:


import pandas as pd


# In[2]:


train = pd.read_excel("/home/santosh/DATASCIENCE/HCKATHON/book fare prediction/Data_Train.xlsx")
test = pd.read_excel("/home/santosh/DATASCIENCE/HCKATHON/book fare prediction/Data_Test.xlsx")


# In[3]:


train.head()


# In[4]:


train.info()


# In[5]:


train.describe()


# In[6]:


print(train.columns)


# In[7]:


## Removing Synopsis since here we are not going to use this feature

train = train[['Title', 'Author', 'Edition', 'Reviews', 'Ratings','Genre',
               'BookCategory', 'Price']]

test = test[['Title', 'Author', 'Edition', 'Reviews', 'Ratings','Genre',
               'BookCategory']]


# In[8]:


train.head()


# In[9]:


train.isnull().sum()


# In[10]:


#A method to clean and restructure the Edition column

def split_edition(data):  
  
  edition  = list(data)
  
  ed_type = [i.split(",– ")[0].strip().upper() for i in edition]
  
  edit_date = [i.split(",– ")[1].strip() for i in edition]
  
  m_y = [i.split()[-2:] for i in edit_date]
  
  
  for i in range(len(m_y)):
    if len(m_y[i]) == 1:
      m_y[i].insert(0,'NA')
      
  # Based on the given dataset below is the list of possible values for Months
  
  months =  ['Apr','Aug','Dec','Feb', 'Jan', 'Jul','Jun','Mar','May','NA','Nov','Oct','Sep']
  
  ed_month = [m_y[i][0].upper() if m_y[i][0] in months else 'NA' for i in range(len(m_y))]
  ed_year = [int(m_y[i][1].strip()) if m_y[i][1].isdigit() else 0 for i in range(len(m_y))]
  
  return ed_type, ed_month, ed_year


# In[11]:


#Identifying the maximum number of authors for a single book from the given datasets
authors_1 = list(train['Author'])
authors_2 = list(test['Author'])

authors_1.extend(authors_2)

authorslis = [i.split(",") for i in authors_1]

max = 1
for i in authorslis:
  if len(i) >= max:
    max = len(i)
print("Max. number of authors for a single boook = ",max)

for i in range(len(authorslis)):
  if len(authorslis[i]) == max:
    print(i)    
    
all_authors = [author.strip().upper() for listin in authorslis for author in listin]


# In[12]:


# A method to split the Author column in to 7 new columns
def split_authors(data):
  
  authors = list(data)
  
  A1 = []
  A2 = []
  A3 = []
  A4 = []
  A5 = []
  A6 = []
  A7 = []
  for i in authors:
    
    try :
      A1.append(i.split(',')[0].strip().upper())
    except :
      A1.append('NONE')
      
    try :
      A2.append(i.split(',')[1].strip().upper())
    except :
      A2.append('NONE')
        
    try :
      A3.append(i.split(',')[2].strip().upper())
    except :
      A3.append('NONE')
        
    try :
      A4.append(i.split(',')[3].strip().upper())
    except :
      A4.append('NONE')
        
    try :
      A5.append(i.split(',')[4].strip().upper())
    except :
      A5.append('NONE')
      
    try :
      A6.append(i.split(',')[5].strip().upper())
    except :
      A6.append('NONE')
     
    try :
      A7.append(i.split(',')[6].strip().upper())
    except :
      A7.append('NONE')

      
  return A1,A2,A3,A4,A5,A6,A7
  
all_authors.append('NONE')


# In[13]:


#Identifying the maximum number of Genres for a single book from the given datasets

genre_1 = list(train['Genre'])
genre_2 = list(test['Genre'])

genre_1.extend(genre_2)

genre_lis = [i.split(",") for i in genre_1]


max = 1
for i in genre_lis:
  if len(i) >= max:
    max = len(i)
print("Max. number of genres for a single boook = ",max)
      
all_genres = [genre.strip().upper() for listin in genre_lis for genre in listin]


# In[14]:


# A method to split the Genre column in to 7 new columns

def split_genres(data):
  
  genres = list(data)
  
  G1 = []
  G2 = []
  
  for i in genres:
    
    try :
      G1.append(i.split(',')[0].strip().upper())
      
    except :
      G1.append('NONE')
      
    try :
      G2.append(i.split(',')[1].strip().upper())
    except :
      G2.append('NONE')


      
  return G1,G2
  
all_genres.append('NONE')


# In[15]:


#Identifying the maximum number of Categories for a single book from the given datasets

cat_1 = list(train['BookCategory'])
cat_2 = list(test['BookCategory'])

cat_1.extend(cat_2)

cat_lis = [i.split(",") for i in cat_1]


max = 1
for i in cat_lis:
  if len(i) >= max:
    max = len(i)
print("Max. number of Categories for a single boook = ",max)

all_categories = [cat.strip().upper() for listin in cat_lis for cat in listin]


# In[16]:


# A method to split the Category column in to 7 new columns

def split_categories(data):
  
  cat = list(data)
  
  C1 = []
  C2 = []

  for i in cat:
    
    try :
      C1.append(i.split(',')[0].strip().upper())
    except :
      C1.append('NONE')
      
    try :
      C2.append(i.split(',')[1].strip().upper())
    except :
      C2.append('NONE')


      
  return C1,C2
  
all_categories.append('NONE')


# In[17]:


# A method to clean and restructure the datasets

import re

def restructure(data):
  
  #Cleaning Title Column
  titles = list(data['Title'])
  titles = [title.strip().upper() for title in titles]
  
  #Cleaning & Restructuring Author Column
  a1,a2,a3,a4,a5,a6,a7 = split_authors(data['Author']) 
  
  #Cleaning & Restructuring Edition Column
  ed_type, ed_month, ed_year = split_edition(data['Edition'])
  
  #Cleaning Ratings Column
  ratings = list(data['Reviews'])
  ratings = [float(re.sub(" out of 5 stars", "", i).strip()) for i in ratings]
  
  #Cleaning Reviews Column
  reviews = list(data['Ratings'])
  plu = ' customer reviews'
  reviews = [re.sub(" customer reviews", "", i) if plu in i else re.sub(" customer review", "", i) for i in reviews  ]
  reviews = [int(re.sub(",", "", i).strip()) for i in reviews ]
  

  #Cleaning & Restructuring Genre Column
  g1, g2 = split_genres(data['Genre'])
  
  #Cleaning & Restructuring BookCategory Column
  c1,c2 = split_categories(data['BookCategory'])

  # Forming the Structured dataset
  structured_data = pd.DataFrame({'Title': titles,
                                  'Author1': a1,
                                  'Author2': a2,
                                  'Author3': a3,
                                  'Author4': a4,
                                  'Author5': a5,
                                  'Author6': a6,
                                  'Author7': a7,
                                  'Edition_Type': ed_type,
                                  'Edition_Month': ed_month,
                                  'Edition_Year': ed_year,
                                  'Ratings': ratings,
                                  'Reviews': reviews,
                                  'Genre1': g1,
                                  'Genre2': g2,
                                  'Category1': c1,
                                  'Category2': c2
                                  
                               })
  
  return structured_data


# In[18]:


restructure(train).head(3)


# In[21]:


X_train = restructure(train)

Y_train = train.iloc[:, -1].values

X_test = restructure(test)


# In[22]:


X_train.describe(include = 'all')


# In[23]:


X_train.info()


# In[24]:


X_test.info()


# In[25]:


# A method for Finding Unique items for all columns
def unique_items(list1, list2):
  a = list1
  b = list2
  a.extend(b)
  return list(set(a))


# In[26]:


from sklearn.preprocessing import LabelEncoder

le_Title = LabelEncoder()
all_titles = unique_items(list(X_train.Title),list(X_test.Title))
le_Title.fit(all_titles)

le_Edition_Type = LabelEncoder()
all_etypes = unique_items(list(X_train.Edition_Type),list(X_test.Edition_Type))
le_Edition_Type.fit(all_etypes)


le_Edition_Month = LabelEncoder()
all_em = unique_items(list(X_train.Edition_Month),list(X_test.Edition_Month))
le_Edition_Month.fit(all_em)

le_Author = LabelEncoder()
all_Authors = list(set(all_authors))
le_Author.fit(all_Authors)

le_Genre = LabelEncoder()
all_Genres = list(set(all_genres))
le_Genre.fit(all_Genres)

le_Category = LabelEncoder()
all_Categories = list(set(all_categories))
le_Category.fit(all_Categories)


# In[27]:


X_train['Title'] = le_Title.transform(X_train['Title'])

X_train['Edition_Type'] = le_Edition_Type.transform(X_train['Edition_Type'])



X_train['Edition_Month'] = le_Edition_Month.transform(X_train['Edition_Month'])

X_train['Author1'] = le_Author.transform(X_train['Author1'])
X_train['Author2'] = le_Author.transform(X_train['Author2'])
X_train['Author3'] = le_Author.transform(X_train['Author3'])
X_train['Author4'] = le_Author.transform(X_train['Author4'])
X_train['Author5'] = le_Author.transform(X_train['Author5'])
X_train['Author6'] = le_Author.transform(X_train['Author6'])
X_train['Author7'] = le_Author.transform(X_train['Author7'])


X_train['Genre1'] = le_Genre.transform(X_train['Genre1'])
X_train['Genre2'] = le_Genre.transform(X_train['Genre2'])


X_train['Category1'] = le_Category.transform(X_train['Category1'])
X_train['Category2'] = le_Category.transform(X_train['Category2'])


# In[28]:


X_train.head()


# In[29]:


X_test['Title'] = le_Title.transform(X_test['Title'])

X_test['Edition_Type'] = le_Edition_Type.transform(X_test['Edition_Type'])



X_test['Edition_Month'] = le_Edition_Month.transform(X_test['Edition_Month'])

X_test['Author1'] = le_Author.transform(X_test['Author1'])
X_test['Author2'] = le_Author.transform(X_test['Author2'])
X_test['Author3'] = le_Author.transform(X_test['Author3'])
X_test['Author4'] = le_Author.transform(X_test['Author4'])
X_test['Author5'] = le_Author.transform(X_test['Author5'])
X_test['Author6'] = le_Author.transform(X_test['Author6'])
X_test['Author7'] = le_Author.transform(X_test['Author7'])


X_test['Genre1'] = le_Genre.transform(X_test['Genre1'])
X_test['Genre2'] = le_Genre.transform(X_test['Genre2'])


X_test['Category1'] = le_Category.transform(X_test['Category1'])
X_test['Category2'] = le_Category.transform(X_test['Category2'])


# In[30]:


X_test.head()


# In[31]:


# Feature Scaling

from sklearn.preprocessing import StandardScaler
sc = StandardScaler()

X_train = sc.fit_transform(X_train)

X_test = sc.transform(X_test)

#Reshaping ti fit the scaler
Y_train = Y_train.reshape((len(Y_train), 1)) 

Y_train = sc.fit_transform(Y_train)

#Restoring the original shape after scaling
Y_train = Y_train.ravel()


# In[32]:


X_train.shape #SC


# In[33]:


Y_train.shape #SC


# In[34]:


X_train


# In[35]:


X_test


# In[36]:


from sklearn.model_selection import train_test_split

train_x, val_x, train_y, val_y = train_test_split(X_train, Y_train, test_size = 0.1, random_state = 123)


# In[37]:


print(train_x.shape)
print(train_y.shape)
print(val_x.shape)
print(val_y.shape)


# In[38]:


from xgboost import XGBRegressor
import numpy as np

xgb=XGBRegressor( objective='reg:squarederror', max_depth=6, learning_rate=0.1, n_estimators=100, booster = 'gbtree', n_jobs = -1,random_state = 1)
xgb.fit(train_x,train_y)

y_pred = sc.inverse_transform(xgb.predict(val_x))
y_true = sc.inverse_transform(val_y)

error = np.square(np.log10(y_pred +1) - np.log10(y_true +1)).mean() ** 0.5
score = 1 - error

print("RMLSE Score = ", score)


# In[39]:


# Fitting the complete training set (inclusing val_x and val_y)
xgb.fit(X_train,Y_train)


# In[40]:


# Predicting for test set
y_pred_xgb = sc.inverse_transform(xgb.predict(X_test))


# In[41]:


# Saving the predictions in excel file

solution = pd.DataFrame(y_pred_xgb, columns = ['Price'])
solution.to_excel('Predict_Book_Price_Soln.xlsx', index = False)


# In[42]:


solution.head(10)


# In[43]:


get_ipython().system('pip install bayesian-optimization')


# In[44]:


from bayes_opt import BayesianOptimization
import xgboost as xgb
#from sklearn.metrics import mean_squared_error,mean_squared_log_error


# In[45]:


dtrain = xgb.DMatrix(X_train, label= Y_train)


# In[46]:


def bo_tune_xgb(max_depth, gamma, n_estimators ,learning_rate):
    params = {'max_depth': int(max_depth),
              'gamma': gamma,      
              'n_estimators': int(n_estimators),
              'learning_rate':learning_rate,
              'subsample': 0.8,
              'eta': 0.1,
              'eval_metric': 'rmse'}
    
    #Cross validating with the specified parameters in 5 folds and 70 iterations
    cv_result = xgb.cv(params, dtrain, num_boost_round=100, nfold=10)    
    
    #Return the negative RMSE
    return -1.0 * cv_result['test-rmse-mean'].iloc[-1]


# In[47]:


xgb_bo = BayesianOptimization(bo_tune_xgb, {'max_depth': (1, 300), 
                                             'gamma': (0, 1),
                                             'learning_rate':(0,1),
                                             'n_estimators':(1,1000)
                                            })


xgb_bo.maximize(n_iter=10, init_points=10, acq='ei')


# In[48]:


#Extracting the best parameters
params = xgb_bo.max['params']

print(params)

#Conversting the max_depth and n_estimator values from float to int
params['max_depth']= int(round(params['max_depth']))
params['n_estimators']= int(round(params['n_estimators']))

print(params)


# In[49]:


#Initialize an XGB with the tuned parameters and fit the training data
from xgboost import XGBRegressor
reg = XGBRegressor(**params).fit(X_train,Y_train)

y_pred_reg = sc.inverse_transform(reg.predict(X_test))


# In[50]:


solution_bo = pd.DataFrame(y_pred_reg, columns = ['Price'])

solution_bo.head(10)


# In[51]:


solution_bo.to_excel('Predict_Book_Prices_BO_Soln.xlsx', index = False)


# In[ ]:




