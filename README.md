# World Refugee Dataset, 1989-2015

Please cite when using the data: 

- Marbach, Moritz. 2018. On Imputing UNHCR Data. Research and Politics (forthcoming). 
- Marbach, Moritz. 2018. World Refugee Dataset, 1989-2015. Version: 1.1.0

Note, that the UNHCR updates data points in their database and consequently future versions of this dataset might differ from previous versions due to these changes. The version from the Research and Politics article is v1.1.0. 

The raw data for this dataset come from the United Nations High Commissioner for Refugees (UNHCR) [Statistical Online Population Database](http://popstats.unhcr.org/en/overview) and complemented with data from the United Nations Relief and Works Agency for Palestine Refugees in the Near East (UNRWA). The dataset's unit of analysis is a country-year as defined by the [system-membership file](http://www.correlatesofwar.org/data-sets/state-system-membership) of Correlates of War project. Use the [R countrycode package](https://cran.r-project.org/web/packages/countrycode/) to convert the codes to names. 

The dataset [wrd_1.1.0.csv](usedata/wrd_1.1.0.csv) comes with the following variables: 

* asylum/origin_ccode - Correlates of War country code for refugees' country of origin and destination 
* year - year of observation 
* y - total number of refugees (raw data)
* ylinpol - total number of refugees (interpolated)
* ylassox - total number of refugees (LASSO imputed)

This dataset can be generated by running [MAIN.R](MAIN.R).