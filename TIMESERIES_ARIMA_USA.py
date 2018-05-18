
# coding: utf-8

# In[86]:


import pandas as pd
import numpy as np
import matplotlib.pyplot as plt          
get_ipython().magic('matplotlib inline')


# In[87]:


globaltemp=pd.read_csv("C:/Users/Phani deep/Desktop/PA2/GlobalLandTemperaturesByCountry.csv")


# In[88]:


globaltemp.shape


# In[89]:


globaltemp.describe()


# In[90]:


globaltemp.head()


# In[91]:


USAtemp = globaltemp[globaltemp.Country=='United States']


# In[92]:


USAtemp=USAtemp[["dt","AverageTemperature"]]


# In[93]:


USAtemp.head()


# In[49]:


usatemp=usatemp[['dt','AverageTemperature']]


# In[94]:


USAtemp.dt=pd.to_datetime(USAtemp.dt)


# In[95]:


USAtemp.index=USAtemp.dt


# In[96]:


USAtemp['Year']=USAtemp["dt"].apply(lambda x:x.year)


# In[97]:


USAtemp.groupby(['Year']).mean()


# In[98]:


USAtemp.groupby(['Year']).mean().plot(kind="density", figsize=(14,14))


# In[99]:


USAtemp1=USAtemp['1913':]


# In[100]:


USAtemp1.groupby(['Year']).mean().plot(kind="density", figsize=(14,14))


# In[101]:


USAtemp1=USAtemp1.dropna()


# In[107]:


from statsmodels.tsa.stattools import adfuller


# In[108]:


USAtempadf=adfuller(USAtemp1.AverageTemperature)


# In[109]:


USAtempadf


# In[110]:


USAtemp1.AverageTemperature.diff(23).plot(kind="line",figsize=(15,15))


# In[112]:


from statsmodels.tsa.seasonal import seasonal_decompose


# In[113]:


result = seasonal_decompose(USAtemp1.AverageTemperature, model='additive',freq=12)
result.plot()


# In[116]:


from statsmodels.graphics.tsaplots import plot_acf


# In[117]:


plot_acf(USAtemp1.AverageTemperature,lags=40)


# In[118]:


from statsmodels.graphics.tsaplots import plot_pacf


# In[119]:


plot_pacf(USAtemp1.AverageTemperature,lags=40)


# In[120]:


import itertools
from statsmodels.tsa.arima_model import ARIMA


# In[124]:


p=d=q=range(0,5)                                #to find out pdq automaticallynand see the results which have least AIC that is our pdq values
pdq=list(itertools.product(p,d,q))
for param in pdq:
    try:
        mod=ARIMA(USAtemp1.AverageTemperature,order=param)
        results=mod.fit()
        print('ARIMA{}-AIC:{}'.format(param,results.aic))
    except:
        continue


# In[125]:


model=ARIMA(USAtemp1.AverageTemperature,order=(2,1,4), freq='M')
results=model.fit()


# In[126]:


USAtemp1.tail()


# In[127]:


temppredict=results.predict('2013-01-09','2018-03-12')
temppredict.plot()


# In[134]:


results.forecast(30)


# In[135]:


temppredict

