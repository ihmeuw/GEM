import os
import sys
import numpy as np
import pandas as pd
import gpr.gpr as gpr
#reload(gpr)

def logit(p):
	return np.log(p) - np.log(1 - p)

def inv_logit(p):
	return np.exp(p) / (1 + np.exp(p))



data = pd.read_csv('/mnt/team/gem/data/gpr_time_series/submission2/monthly_linear_priors2.csv')
data['year'] = data['month']

df_list = []
gpr_data = pd.DataFrame()

for i in pd.unique(data['indicator']):
    for c in pd.unique(data['ihme_loc_id']):
        indicator_country_data = data.loc[((data.indicator == i) & (data.ihme_loc_id == c)),:]
        for s in pd.unique(indicator_country_data['sex']):
            for a in pd.unique(indicator_country_data['age']):
                print('{i}_{c}_{s}_{a}'.format(i=i,c=c,s=s,a=a))
                indicator_country_age_sex_data = indicator_country_data.loc[((indicator_country_data.age == a) & (indicator_country_data.sex == s)),:]
                amp = indicator_country_age_sex_data['mad'].values[0] * 1.4826 
                gpr_out = gpr.fit_gpr(indicator_country_age_sex_data,year_variable='year',amp=amp,scale=4,obs_variable='logit_value', obs_var_variable='logit_variance', mean_variable='logit_value_pred',diff_degree=2,draws=100)
                df_list.append(gpr_out)
print('test')

gpr_data = pd.concat(df_list)
gpr_data.to_csv('/mnt/team/gem/data/gpr_time_series/submission2/monthly_gpr_output_draws.csv')
