# Directory Information
- *Supporting Information* :
This folder contains all the analyses used in the supporting information.
	
	- Model Simulation
	
			This folder contains the code to simulate the disease-structured generalized N-mixture model
	
	- NaivePrevalence
	
			This folder contains data, code, and output to calculate the naive prevalence (i.e., the proportion of individuals infected)

				- ALL_Cope2008_2014.txt

						Contains data in long format from El Copé, Panama from 2008 to 2014

				- Prevalence code.R

						Code used to calculate prevalence each season

				- prev.csv

						Results from model (R output)

				- prev.xlxs

						Formatted R output
	
	- Ordination
	
			This code performs the non-metric multidimensional scaling analysis used to determine if community composition varied by season, habitat, and transect.

				- Cope2010_2014.txt

						Contains data in long format from El Copé, Panama from 2010 to 2014

				- Ordination 20 July 2017.pdf

						Ordination output

				- Ordination.R

						R code for Ordination
	
	- Testing_closure_assumption

			This folder contains code, data, and final figures testing the closure assumption.

				- Code

					- To create figures

						- Closure Assumption.R

						Contains code to determine the difference in the number of individuals found within a season from the first to the last survey each season

					- To format data 
						
						Contains code to format data from long to wide format

				- Data Survey dates

						Contains raw data of the Julian days the survey was performed

				- Figures

						- Infected_seasonal.pdf

							Histogram of infected individuals each season

						- Not_infected_seasonal.pdf

							Histogram of uninfected individuals each season	

	- Testing_Poisson_assumption

				- Poisson assumption.R

				Code used to calculate the mean and the variance of the number of individuals captured at each 20 m section

				- Poisson_assumption.pdf

				Figure used in the Supporting information
		
	- Testing_distribution_of_snout_to_vent_length

				- SVL distribution.R

				Code to create histograms describing the snout-to-vent length of each species. Examined for sexual dimorphism and age structure in the data. 

	- Testing_for_prior_sensitivity

				- Prior sensitivity.R

				Code to create the figure examining the parameter estimates when priors are changed in the model

	- Testing_for_spatial_autocorrelation

				- Testing4SpatialAutocorrelation.R

				Contains code to test for spatial autocorrelation among the 20-m sections
	


