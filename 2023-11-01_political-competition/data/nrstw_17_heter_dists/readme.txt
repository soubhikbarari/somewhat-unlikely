Read Me file for "Geography, Uncertainty, and Polarization"

In order to recreate the empirical results in our paper, please follow these steps:

1) Create the following subdirectories
	- Plots
		- pdf
			- Density
			- Models
			- Scatter
	- Tables
	
2) Open R and change the working directory to the folder where you've placed all the replication materials.

3) Run geography_replication.R

4) Open STATA and run tracts.do

5) This should output tex files for all our tables and pdf files for each figure.  The following describes how the resulting files map to the tables/figures in our paper:

FIGURES
	Figure 1: Distributions of Legislator and District Median Ideal Points: legislators_individuals_s.pdf
	Figure 2: Legislative polarization and ideological heterogeneity: between_district_polarization_s.pdf, within_district_polarization_s.pdf
	Figure 3: Average District Ideology and Within-District Polarization: ideology_heterogeneity_s.pdf
	Figure 4a: Within-district distributions of votes and ideology (Precinct-level 2008 Obama vote share): 
 	Figure 4b: Within-district distributions of votes and ideology (Within-district distribution of ideology, pivotal districts)
 	Figure 5: Scatterplot of Legislator Ideology and State Senate District Opinion, by Heterogeneity Tercile: opinion_ideology_s.pdf
	Figure 6: Predicted values of Republican and Democratic ideal points as a function of district heterogeneity: predicted_s_d.pdf, predicted_s_r.pdf
	Figure 7: Scatterplot of District Heterogeneity and Partisan Divergence: within_district_divergence_s.pdf, within_district_year_divergence_s.pdf
	Figure 8: Race, Ethnicity, and Distance from City Center
	Figure 9: Income and Distance from City Center
	Figure C.1: Distributions of Legislator and District Median Ideal Point: legislators_individuals_h.pdf
	Figure C.2: Legislative polarization and ideological polarization: within_district_polarization_h.pdf, between_district_polarization_h.pdf
	Figure C.3: Average District Ideology and Within-District Polarization: ideology_heterogeneity_h.pdf
	Figure C.4: Scatterplot of Legislator Ideology and District Opinion, by Heterogeneity Tercile: opinion_ideology_h.pdf 
	Figure C.5: Scatterplot of District Heterogeneity and Partisan Divergence: opinion_ideology_h.pdf, within_district_year_divergence_h.pdf
	Figure D.1: Scatterplot of Representative Ideology and District Opinion, by Heterogeneity Tercile: opinion_ideology_congress.pdf
	
TABLES
	Table 1: Heterogeneity - Upper Chamber Score Models (Multilevel): div_heterogeneity_party_mlm_s.tex
	Table 2: Matching Estimates of the AIDD (Average Treatment Effect) in the Upper Chamber: match_results_s.tex
	Table B.2: Uncertainty - Legislator Score Models (Multilevel): div_uncertainty_party_mlm_s.tex
	Table B.3: Matching Estimates of the AIDD in the Upper Chamber: match_results_uncertainty_s.tex
	Table B.4: Heterogeneity - Legislator Score Models (Multilevel): st_heterogeneity_party_mlm_s.tex
	Table C.1: Heterogeneity - Lower Chamber Score Models (Multilevel): div_heterogeneity_party_mlm_h.tex
	Table C.2: Matching Estimates of the AIDD (Average Treatment Effect) in the Lower Chamber: match_results_h.tex
	Table D.1: Hetereogeneity - Congress Models (OLS): mlm_heterogeneity_congress.tex
	Table D.2: Matching Estimates of the AIDD (Average Treatment Effect): match_results_congress.tex
	Table E.3: Percentiles - Legislator Score Models (Multilevel): div_percentile_party_mlm_s.tex


An example log file for the R code is "log.txt". An example for the
STATA code is in tracts_historical_log.txt. 
