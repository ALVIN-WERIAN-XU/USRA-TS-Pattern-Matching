Research Title: Time Series Imputation Via Pattern Matching

Role：Research Assistant 

Supervisor: Glen Takahara, Professor of Department of Math & Stat, Queen’s University 

https://www.queensu.ca/mathstat/people/faculty/profiles/takahara

Project Description: Data is often in the form of time series, for example economic and financial data or data in the natural sciences. Most statistical methods for analysing such data assume that the observations have been collected at regular time intervals. In practice, these 
time series observations often contain gaps where the data is missing or corrupted for certain periods of time, and imputing these missing values is of interest. We wish to use the available data to give reasonable estimates of missing data. This project focuses on a nonparametric 
pattern matching approach for imputation. For a given gap, a simple imputation method, such as linear interpolation, can be used to fill in the gap. Combined with available data immediately before and after the gap, we create a target segment. We then select multiple predictive segments, each the same length as the target segment and each containing full data. The main objectives are


(i) explore similarity measures between predictive segments and the target segment, 

(ii) combine the predictive segments weighted by their similarity to the target segment to produce a single predictive segment, which will be used to replace the naively imputed values in the target segment

The project will require extensive coding in R or Python to implement selection of predictive segments, computation of one or more similarity measures for pattern matching, and performance analysis. Related to this, a common goal in time series analysis is forecasting, where unobserved future values are to be predicted. If time allows, a modification of (i) and (ii) can be used to explore forecasting.

Student’s Role: Come up with reasonable similarity measures jointly with supervisor for pattern matching time series segments, and implement these in R code. Then, again in R or Python, use this code to produce a weighted combination of segments of a time series to produce an imputation of missing values, and evaluate the performance of the imputation.

Prerequisites: Student should be proficient in R or Python. Familiarity with time series methods preferred but not necessary
