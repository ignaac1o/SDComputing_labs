import pandas as pd

data=pd.read_csv("/Users/ignacioalmodovarcardenas/Desktop/Scalable and distributed computing/SDComputing_labs/LAB2/tripdata_2017-01.csv",header=0)
print(data.head(10))

data.head(10)
data.columns.values

#Choose 3 different analysis from the data set

# 1 - Most picked-up zones per hour
fst_analysis=data.loc[:,["PULocationID","tpep_pickup_datetime"]]
fst_analysis.min()

fst_analysis['tpep_pickup_time']=fst_analysis['tpep_pickup_datetime'].apply(lambda x: x.split(' ')[1])
fst_analysis['hour']=pd.cut(x=fst_analysis['tpep_pickup_time'],bins=["00:00:00","06:00:00","12:00:00","18:00:00","23:59:59"])

index=pd.date_range(start="00:00:00",end="23:59:59",freq="H").hour       
hour=pd.timedelta_range(start="00:00:00",end="23:59:59",freq="H")
hour=pd.DatetimeIndex.hour(start="00:00:00",end="23:59:59",freq="H")

# 2 - Average total price per trip including tips
snd_analysis=data['total_amount']
meanprice=snd_analysis.mean()
snd_analysis



# 3 - type pf payment in terms od hours


# 4 - Type of payment in term of drop-off zone
fourth_analysis=data.loc[:,['DOLocationID','payment_type','total_amount']]
groups=fourth_analysis.groupby('payment_type')

group1=groups.get_group(1)
group2=groups.get_group(2)

df=fourth_analysis.groupby(['DOLocationID','payment_type'])['total_amount'].mean()

import seaborn as sns
sns.set(color_codes=True)

df.plot(kind='bar',stacked=True)

fourth_analysis['payment_type'].unique()
fourth_analysis['DOLocationID'].unique()
fourth_analysis['total_amount'].mean()

index=fourth_analysis['DOLocationID']==1

fourth_analysis[index]['total_amount'].mean()

sns.barplot(x=df.index,y=df.values)

df.tidy()