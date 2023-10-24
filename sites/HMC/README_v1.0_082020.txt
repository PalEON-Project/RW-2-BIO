ReadMe file written by Alyssa Willson on 24-10-2023.

-------------------
GENERAL INFORMATION
-------------------

Title of Dataset: Huron Mountain Club Tree-Ring Biomass Estimates 

Author Information (Name, Institution, Address, Email)
	Principal Investigator: Andria Dawson, Mount Royal University, 4825 Mt Royal Gate SW, Calgary, AB T3E 6K6, adawson@mtroyal.ca 
	Alternate Contact(s): Marissa Kivi, University of Notre Dame, Notre Dame, IN, 46556, marissakivi3@gmail.com
			      Alyssa Willson, University of Notre Dame, Notre Dame, IN 46556, EMAIL

Date of data processing: 24 October 2023

Model version: Version 1.0 

Configuration file: 

Geographic location of site: Based on tree rings measured at Huron Mountain Club (46.87988,	-87.93482)

-----------
INPUT DATA 
-----------

Input Files: 
1. HMC_treeMeta_082020.csv
2. HMC_census_082020.csv
2. 10 RWL files from four plots 

The foundational dataset used in the processing of these biomass estimates is a tree ring dataset collected by Alex Dye that was uploaded on 21 May 2018 to the PalEON Wiki website (version 1.0). His team cored trees within 4 plots using a fixed radius plot design. The following species were included in the final ring width dataset: 

- ACSA = sugar maple (Acer saccharum)
- BEAL = yellow birch (Betula alleghaniensis)
- TIAM = American basswood (Tilia americana)
- TSCA = eastern hemlock (Tsuga canadensis)
- ACRU = red maple (Acer rubrum)
- POGR = bigtooth aspen (Populus grandidentata)
- THOC = arborvitae (Thuja occidentalis)

-----------
METHODOLOGY
-----------

Using the available tree-ring and diameter estimates for trees at the site, we fit and sampled from a STAN statistical model that estimates annual diameter for each individual in the dataset. We, then, use the diameter estimations to estimate aboveground biomass for each individual. For more detailed information on the statistical model, see the STAN model documentation in the model repository. 

For this site, both the STAN RW model and STAN RW + CENSUS were run. 1 chain was run with 500 iterations. Only the last 250 iterations were considered for estimating aboveground biomass. The final model samples are processed to give individual-level diameter and biomass estimates and species-level biomass estimates for the site. 

Biomass was estimated using taxa-level allometric equations given by Chojnacky et. al (2014) that estimate aboveground biomass (AB; in Kg) from diameter at breast height (DBH; in cm). The equation is as follows, where BO and B1 are estimated for a wide array of tree taxa: AB = exp(B0 + B1 * log(DBH)). We used the file titled "acronym_to_chojnacky_v0.1.csv" to match the species from the tree ring dataset to the Chojnacky coefficients. (Chojnacky, D.C., L.S. Heath, and J.C. Jenkins (2014). Updated generalized biomass equations for North American tree species. Forestry (87): 129-151.)

Due to the gaps in sampling introduced by the sampling design of the tree-ring dataset, we also corrected for sampling biases in the species-level biomass estimates by weighting biomass contributions from different class sizes based on the total plot area where that class size was sampled. For example, in the triple-nested design, trees of the smallest class size (< 20 cm.) were cored only in the smallest ring of the plot. Therefore, we determined the biomass contribution of that particular class size by dividing the total cumulative biomass from all individuals of that size for each species by the total area of the smallest rings for all plots. 

Individual aboveground biomass increment estimates were determined by calculating the difference in biomass for that individual between the present and the next year. Species-level aboveground biomass increment estimates were determined by calculating the difference in biomass contribution for that species between the present and the next year. 

------------
OUTPUT DATA 
------------

Data time range: 1900 - 2013

Number of iterations: 250 

File list: 
1. DBH_STAN_HMC_v2.0_082020.RDS
2. AGB_STAN_HMC_v2.0_082020.RDS
3. AGBI_STAN_HMC_v2.0_082020.RDS
4. AGB_TAXA_STAN_HMC_v2.0_082020.RDS

1. DBH_STAN_HMC_v1.0_082020.RDS: This file contains the estimates of annual tree diameter at breast height (cm) for all sampled individuals for all years and iterations from the STAN model results.  

Number of observations: 
Number of columns:  

2. AGB_STAN_HMC_v1.0_082020.RDS: This file contains the estimates of aboveground biomass (Kg) for all sampled individuals for all years and iterations based on the diameter estimates given in the above file. 

Number of observations: 
Number of columns: 

3. AGBI_STAN_HMC_v1.0_082020.RDS: This file contains the estimates of aboveground biomass increment (Kg/year) for all sampled individuals for all years and iterations based on the difference in aboveground biomass between years as given in the above file. 

Number of observations: 
Number of columns: 

4. AGB_TAXA_STAN_HMC_v1.0_082020.RDS: This file contains estimates of species-level aboveground biomass (Mg/ha) across all species for all plots measured for the site for all iterations. The sampling correction described above was applied to produce the results in this file. Therefore, class sizes were appropriately weighted to account for biomass lost due to the sampling design. 

Number of observations: 
Number of columns: 

