ReadMe file written by Marissa Kivi on 2021-01

-------------------
GENERAL INFORMATION
-------------------

Title of Dataset: Harvard Forest (Lyford Plots) Tree-Ring Biomass Estimates 

Author Information (Name, Institution, Address, Email)
	Principal Investigator: Andria Dawson, Mount Royal University, 4825 Mt Royal Gate SW, Calgary, AB T3E 6K6, adawson@mtroyal.ca 
	Alternate Contact(s): Marissa Kivi, University of Notre Dame, Notre Dame, IN, 46556, marissakivi3@gmail.com
			      Alyssa Willson, University of Notre Dame, Notre Dame, IN 46556, awillso2@nd.edu 

Date of data processing:  January 2021

Model version: Version 2.0 

Configuration file: 

Geographic location of site: Based on tree rings measured at the Lyford Plots at Harvard Forest, MA (42.53, -72.18)

-----------
INPUT DATA 
-----------

Input Files: 
1. HARVARD_treeMeta_102020.csv 
2. 8 RWL files from two plots 
3. HARVARD_census_102020.csv

There are two foundational datasets used in the processing of these biomass estimates. The first is a tree ring dataset collected by ------ at the Harvard Forest Research Area that was uploaded on 20 April 2016 to the PalEON Wiki website as "version 2". Three 20m plots were measured following a double-nested design (https://paleon.geography.wisc.edu/doku.php/working_groups;npp_protocols). The following species were included in the final ring width dataset: 

- ACRU = red maple (Acer rubrum) 
- BEAL = yellow birch (Betula alleghaniensis) 
- BELE = sweet birch (Betula lenta)
- FAGR = American beech (Fagus grandifolia) 
- HAVI = Witch hazel (Hamamelis virginiana) 
- PIST = Eastern white pine (Pinus strobus)
- QURU = Northern red oak (Quercus rubra) 
- QUVE = Black oak (Quercus velutina)
- TSCA = Eastern hemlock (Tsuga canadensis)

The data files used from the tree ring dataset include the 8 RWL files in the ZipFile, as well as the "LyfordAllPlots.csv" file, which contains information specific to each of the tree IDs referenced in the ring width series. In the site folder "data/raw/past" of the model repository, you will find the original input data files, as well as the script(s) used to reformat the original files for the workflow. 

There were two changes applied to the tree ring RWL files: 
1. All Tree IDS were extended to be compatible with three-digit tree numbers (i.e. LF101 was changed to LF1001) 
2. The NA values for ring widths between 102 and 112 for LF2040 were changed to zeros since the tree was marked as still living in the census for these years, as well as in the year of coring.

There were only formatting changes applied to the tree ring metadata file. The final treeMeta CSV file contains the following information for the RW trees:  
1. site: string containing site prefix and plot number (e.g. 'RH1') 
2. ID: tree ID that contains site prefix, plot number, and tree number that maps trees to ring series files (e.g. 'RH1001') 
3. species: 4-letter species code for each tree
4. dbh: diameter at breast height measured in centimeters at time of coring
5. distance: distance in meters from plot center

Citation on tree ring dataset: Dye, A. Barker Plotkin, D. Bishop, N. Pederson, B. Poulter, and A. Hessl. Comparing tree-ring and permanent plot estimates of aboveground net primary production in three eastern us forests. Ecosphere, 7(9), 2016.

The second foundational dataset for this site includes the census data measured for the Lyford Plots, which was uploaded to the PalEON Wiki website on 7 April 2016 and is available on the Harvard Forest data archive website. The file is titled "hf032-01.csv". Censuses were conducted in 1962, 1969, 1975, 1987-1991, 2001, and 2011 and mapped all trees with DBH greater than 5 cm. in a mapped area covering 2.88 ha. The three ring-width plots include only a fraction of this area, and, therefore, we determined which census trees were located within the plot areas and considered only those trees for the model. The process by which we did this can be found in the reformatting scripts mentioned above. We excluded all census measurements for trees which were not considered 'living' at the time of the census (denoted by 'cond' == 'L').

All individuals of species which were not also found in the ring width data were removed from the census dataset. 

There were 7 trees between the ring width data and the census data where we see discrepancies in species identification. 
- For 5 of the individuals, we see a logical pattern in diameter growth, a similar location, and the same family for species identifications. We assume misidentified species during the previous assessment in the census since some species are hard to identify at young ages, and we assume the most recent identification is correct (i.e. in the ring width data). These individuals include #1568, 2160, 2168, 4063, and 4107. 
- We removed individual #3926 as it is marked as living outside of the plot range. 
- We completely remove the census data for individual #1006 because the species are not in the same family (ACRU vs. FAGR), and though we do believe the two records are for the same tree, the ring width data is sufficient to account for the biomass of the individual in the model. It is unlikely that the beech tree replaced the red maple in just a year or so. 

There were also other discrepancies between the ring width and census data for this site that we had to handle. In particular, there were 11 trees measured and marked alive in the final census in 2011, but were not included in the ring width data. We made the following decisions on their condition of coring: 
  - IDs #1070, 2139, 2170, and 3106 were outside of the sampling design of the ring width plots, so these were considered alive at time of coring.  
  - ID #3036 is noted in the RW data as missing data, but marked as alive. Considered alive. 
  - IDs #1130 and 2126 have no clear reason for being excluded, so assumed dead.
  - IDs #1014, 2027, 2045, 3016 were noted as dead in the ring width data, so assumed dead. 

-----------
METHODOLOGY
-----------

Using the available tree-ring and diameter estimates for trees at the site, we fit and sampled from a STAN statistical model that estimates annual diameter for each individual in the dataset. We, then, used the diameter estimations to estimate aboveground biomass for each individual. For more detailed information on the statistical model, see the STAN model documentation in the model repository. 

For this site, we ran the STAN RW + Census model and the STAN RW only model. 3 chains were run for both models with 5000 iterations. Looking at the figures, we noted convergence within the final *** iterations, and we thinned these final iterations from all three chains, taking only 300 values for estimating aboveground biomass. The final model samples are processed to give individual-level diameter and biomass estimates and species-level biomass estimates for the site. 

Biomass was estimated using taxa-level allometric equations given by Chojnacky et. al (2014) that estimate aboveground biomass (AB; in Kg) from diameter at breast height (DBH; in cm). The equation is as follows, where BO and B1 are estimated for a wide array of tree taxa: AB = exp(B0 + B1 * log(DBH)). We used the file titled "acronym_to_chojnacky_v0.1.csv" to match the species from the tree ring dataset to the Chojnacky coefficients. (Chojnacky, D.C., L.S. Heath, and J.C. Jenkins (2014). Updated generalized biomass equations for North American tree species. Forestry (87): 129-151.)

Any annual biomass contribution from a tree with an estimated diameter of less than 5 cm was eliminated from the dataset, since the census data only included information on trees of DBH 5 cm or larger. 

Due to the gaps in sampling introduced by the sampling design of the tree-ring dataset, we also corrected for sampling biases in the species-level biomass estimates in the RW only model results by weighting biomass contributions from different class sizes based on the total plot area where that class size was sampled. For example, trees of the smallest class size (< 20 cm.) were cored only in the smallest ring of the plot. Therefore, we estimate the per-area biomass contribution of that particular class size by dividing the cumulative species-level biomass from all individuals in that plot's size class by the area of the smallest ring. 

Individual aboveground biomass increment estimates were determined by calculating the difference in biomass for that individual between the present and the next year. Species-level aboveground biomass increment estimates were determined by calculating the difference in biomass contribution for that species between the present and the next year. 

------------
OUTPUT DATA 
------------

Data time range: 1900 - ****

Number of iterations: 300 

File list: 
1. DBH_STAN_ROOSTER_v2.0_102020.RDS
2. AGB_STAN_ROOSTER_v2.0_102020.RDS
3. AGBI_STAN_ROOSTER_v2.0_102020.RDS
4. AGB_TAXA_STAN_ROOSTER_v2.0_102020.RDS

1. DBH_STAN_ROOSTER_v2.0_102020.RDS: This file contains the estimates of annual tree diameter at breast height (cm) for all sampled individuals for all years and iterations from the STAN model results.  

Number of observations: 
Number of columns:  

2. AGB_STAN_ROOSTER_v2.0_102020.RDS: This file contains the estimates of aboveground biomass (Kg) for all sampled individuals for all years and iterations based on the diameter estimates given in the above file. 

Number of observations: 
Number of columns: 

3. AGBI_STAN_ROOSTER_v2.0_102020.RDS: This file contains the estimates of aboveground biomass increment (Kg/year) for all sampled individuals for all years and iterations based on the difference in aboveground biomass between years as given in the above file. 

Number of observations: 
Number of columns: 

4. AGB_TAXA_STAN_ROOSTER_v2.0_102020.RDS: This file contains estimates of species-level aboveground biomass (Mg/ha) across all species for all plots measured for the site for all iterations. The sampling correction described above was applied to produce the results in this file. Therefore, class sizes were appropriately weighted to account for biomass lost due to the sampling design. 

Number of observations: 
Number of columns: 

