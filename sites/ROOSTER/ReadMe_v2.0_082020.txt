ReadMe file written by Marissa Kivi on 2020-08-XX.

-------------------
GENERAL INFORMATION
-------------------

Title of Dataset: Rooster Hill Tree-Ring Biomass Estimates 

Author Information (Name, Institution, Address, Email)
	Principal Investigator: Andria Dawson, Mount Royal University, 4825 Mt Royal Gate SW, Calgary, AB T3E 6K6, adawson@mtroyal.ca 
	Alternate Contact(s): Marissa Kivi, University of Notre Dame, Notre Dame, IN, 46556, marissakivi3@gmail.com
			      Alyssa Willson, University of Notre Dame, Notre Dame, IN 46556, EMAIL

Date of data processing: XX August 2020

Model version: Version 2.0 

Configuration file: 

Geographic location of site: Based on tree rings measured at Rooster Hill (43.2309, -74.5267)

-----------
INPUT DATA 
-----------

Input Files: 
1. ROOSTER_treeMeta_082020.csv 
2. 11 RWL files from two plots 

The foundational dataset used in the processing of these biomass estimates is a tree ring dataset collected by Dr. Neil Pederson at the Harvard Forest Research Area that was uploaded on 24 May 2018 to the PalEON Wiki website. His team cored trees within 2 plots following the PalEON triple-nested design (https://paleon.geography.wisc.edu/doku.php/working_groups;npp_protocols). The following species were included in the final ring width dataset: 

- ACRU = red maple (Acer rubrum) 
- BEPA = paper birch (Betula papyrifera) 
- FAGR = American beech (Fagus grandifolia) 
- PCRU = Red spruce (Picea rubens)
- PIST = Eastern white pine (Pinus strobus)
- PRSE = Black cherry (Prunus serotina)
- QURU = Northern red oak (Quercus rubra) 

The data files used from the dataset include the 11 RWL files in the ZipFile, as well as the "RoosterHillAllPlots.csv" file, which contains information specific to each of the tree IDs referenced in the ring width series. In the site folder "data/raw/past" of the model repository, you will find the original input data files, as well as the script(s) used to reformat the original files for the workflow. 

No census data were available for this site. 

The final CSV file contains the following information: 
1. site: string containing site prefix and plot number (e.g. 'RH1') 
2. ID: tree ID that contains site prefix, plot number, and tree number that maps trees to ring series files (e.g. 'RH1001') 
3. species: 4-letter species code for each tree
4. dbh: diameter at breast height measured in centimeters at time of coring
5. distance: distance in meters from plot center

-----------
METHODOLOGY
-----------

Using the available tree-ring and diameter estimates for trees at the site, we fit and sampled from a STAN statistical model that estimates annual diameter for each individual in the dataset. We, then, use the diameter estimations to estimate aboveground biomass for each individual. For more detailed information on the statistical model, see the STAN model documentation in the model repository. 

For this site, only the STAN RW model was run, and diameter measurement error was estimated for the model using the average value from the RW + Census model fit for Harvard Forest (Version 2.0; XX August 2020). 1 chain was run with 5000 iterations. Only the last 250 iterations were considered for estimating aboveground biomass. The final model samples are processed to give individual-level diameter and biomass estimates and species-level biomass estimates for the site. 

Biomass was estimated using taxa-level allometric equations given by Chojnacky et. al (2014) that estimate aboveground biomass (AB; in Kg) from diameter at breast height (DBH; in cm). The equation is as follows, where BO and B1 are estimated for a wide array of tree taxa: AB = exp(B0 + B1 * log(DBH)). We used the file titled "acronym_to_chojnacky_v0.1.csv" to match the species from the tree ring dataset to the Chojnacky coefficients. (Chojnacky, D.C., L.S. Heath, and J.C. Jenkins (2014). Updated generalized biomass equations for North American tree species. Forestry (87): 129-151.)

Biomass was not estimated in years for an individual when its average estimated DBH was less than 5 cm. This is because... 

Due to the gaps in sampling introduced by the sampling design of the tree-ring dataset, we also corrected for sampling biases in the species-level biomass estimates by weighting biomass contributions from different class sizes based on the total plot area where that class size was sampled. For example, in the triple-nested design, trees of the smallest class size (< 20 cm.) were cored only in the smallest ring of the plot. Therefore, we determined the biomass contribution of that particular class size by dividing the total cumulative biomass from all individuals of that size for each species by the total area of the smallest rings for all plots. 

Individual aboveground biomass increment estimates were determined by calculating the difference in biomass for that individual between the present and the next year. Species-level aboveground biomass increment estimates were determined by calculating the difference in biomass contribution for that species between the present and the next year. 

------------
OUTPUT DATA 
------------

Data time range: 1900 - ????

Number of iterations: 250 

File list: 
1. DBH_STAN_ROOSTER_v2.0_082020.RDS
2. AGB_STAN_ROOSTER_v2.0_082020.RDS
3. AGBI_STAN_ROOSTER_v2.0_082020.RDS
4. AGB_TAXA_STAN_ROOSTER_v2.0_082020.RDS

1. DBH_STAN_ROOSTER_v2.0_082020.RDS: This file contains the estimates of annual tree diameter at breast height (cm) for all sampled individuals for all years and iterations from the STAN model results.  

Number of observations: 
Number of columns:  

2. AGB_STAN_ROOSTER_v2.0_082020.RDS: This file contains the estimates of aboveground biomass (Kg) for all sampled individuals for all years and iterations based on the diameter estimates given in the above file. 

Number of observations: 
Number of columns: 

3. AGBI_STAN_ROOSTER_v2.0_082020.RDS: This file contains the estimates of aboveground biomass increment (Kg/year) for all sampled individuals for all years and iterations based on the difference in aboveground biomass between years as given in the above file. 

Number of observations: 
Number of columns: 

4. AGB_TAXA_STAN_ROOSTER_v2.0_082020.RDS: This file contains estimates of species-level aboveground biomass (Mg/ha) across all species for all plots measured for the site for all iterations. The sampling correction described above was applied to produce the results in this file. Therefore, class sizes were appropriately weighted to account for biomass lost due to the sampling design. 

Number of observations: 
Number of columns: 

