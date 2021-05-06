################################################################################

## The scripts provided in this repository are a compilation of several scripts found in the literature, on github, and edited by me.

________________________________________________________________________________

### **Repository under construction...**



### Step 0.

Primarily, you have to have a list with the target species' names.
It is important to check if the species' names are correct. You can
search species names using this first routine.

* 0_search_flora.R

If you already have your own list, you can check species' names using 
these routines:

* 0_check_flora.R
* 0_check_string.R

** check_string.R (function that you need run along the 6_variable_VIF.R)

### Step 1.

Now that you have the list of species' names, you can download the species' 
occurrences, using:

* 1_download_gbif_specieslink.R


### Step 2.

The list of species occurrences usually comes with some coordinate errors. 
In addition, your interest may not be at all coordinates. 
Therefore, in this step we will do the first cleaning of the data and the 
selection of some specific information desired.

* 2_clean_records.R


### Step 3.

Still about cleaning.
The occurrence records comes with geographical and environmental bias.
We must choose what kind of refinement we want to do:
Whether the geographical (spThin) or environmental (Varela) refinement, or both.

* 3_spThin.R
* 3_Varela.R


### Step 4.

Creating a map with occurence records and minimum convex polygon

** Ps .: under construction...


### Step 5.

Cropping environmental variables

* 5_crop_environmental_variables.R


### Step 6.

When you use variables available on sites like worldclim / eta ... 
and you do not know which is the best variable, biologically speaking, 
it is necessary to make a selection of variables. 
There are 3 main methods for selecting environmental variables: 
selection of variables that are less correlated using the Pearson or Spearman method, 
from the VIF analysis, 
or from a PCA.

* 6_variable_correlation_spearman_and_pearson.R
* 6_variable_PCA.R (building...)
* 6_variable_VIF.R (building...)
  
  ** vif_conc.R (function that you need run along the 6_variable_VIF.R)


### Step 7.

Here is the Correlative Ecological Niche Model itself.
There are many packages available to run ENM. 
Below there are only 3 option: modelR, dismo, biomod

* 7_modelR.R
* 7_dismo.R
* 7_biomod.R





#### References of the most scripts

* Andrea Sanchez-Tapia 
https://github.com/AndreaSanchezTapia
* Sara Mortara 
https://github.com/saramortara
* Bruno M. Carvalho 
https://github.com/brunomc-eco
* Diogo Rocha 
https://github.com/diogosbr
* Daniele Moreira 
https://github.com/daniomoreira
* Daphne Spier 
https://github.com/#########
* Aiello-Lammens et al. 2015
https://cran.r-project.org/web/packages/spThin/spThin.pdf
https://doi.org/10.1111/ecog.01132
* Varela, S., Anderson, R.P., Garcia-Valdes, R., Fernandez-Gonzalez, F., 2014. 
https://github.com/SaraVarela/envSample
