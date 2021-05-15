
## Classic workflow for ENM
________________________________________________________________________________


#### The scripts provided in this repository are a compilation of several scripts found in the literature, on github, and edited by me.



### **Repository under construction...**
Some scripts are ready, others are still being edited. Below, I signal which scripts are still being edited.



#### Step 0.

Primarily, you need a list with your target species names.
It is important checking if the species names are correct. 

You can search species plants names using this first routine:

* 0_search_flora.R

If you already have your own list, you can check species' names using 
these routines:

* 0_check_flora.R
* 0_check_string.R

** check_string.R (function that you need run before 0_check_string.R)

** I am still editting 0_check_string.R, but you can find the original one in
<https://github.com/liibre/rocc/blob/master/R/check_string.R>



#### Step 1.

Now that you have your list of species names, you can download the species 
occurrences, from GBIF and speciesLink websites using:

* 1_download_gbif_specieslink.R



#### Step 2.

The list of species occurrences usually comes with some coordinate errors. 
In addition, your interest may not be at all coordinates. 
Therefore, in this step we will do the first cleaning of the data and the 
selection of some specific information desired.
Here, you can also add your list from a museum or from papers and cleaning it all together.

* 2_clean_records.R



#### Step 3.

Still about cleaning.
The occurrence records comes with geographical and environmental bias.
We must choose what kind of refinement we want to do if the geographical (spThin) or environmental (Varela) cleaning, or even both.

* 3_spThin.R
* 3_varela.R



#### Step 4.

Creating a map with occurrence records and a minimum convex polygon

** under construction...



#### Step 5.

Cropping environmental variables.
After choosing which variables you are going to use 
in the step 6, you can return to step 5 to run the routine 5_organizing_future_variables.R
That way, you won't have to crop all future variables that you won't be using.

* 5_crop_environmental_variables.R
* 5_organizing_future_variables.R



#### Step 6.

When you are using variables from websites like worldclim, ETA ... 
and you do not know which is the best variable for your species (biologically speaking), 
it is necessary to make a selection of variables. 
There are 3 main methods to select environmental variables: 
(1) selecting the least correlated variables, using Pearson or Spearman methods, 
(2) using VIF analysis (then, first run the function vif_conc.R), or (3) using a PCA.

* 6_variable_correlation_spearman_and_pearson.R
* 6_variable_PCA.R (editing... see the original in <https://github.com/Projeto-BHRD-INMA/MNE_BHRD/blob/master/R/5_PCA.R>)
* 6_variable_VIF.R (editing...see the original in <https://github.com/Projeto-BHRD-INMA/MNE_BHRD/blob/master/R/7_VIF.R>)
  
  ** vif_conc.R (function that you need run along the 6_variable_VIF.R)



#### Step 7.

Here is the Correlative Ecological Niche Model itself (or Species Niche Model, see Peterson et al. 2012 to understand the difference, in <http://dx.doi.org/10.4322/natcon.2012.019>).
There are many packages available to run ENM/SDM. 
Below are 3 options: modelR, dismo, biomod. 

* 7_modelR_ENM_with_mpc.R
* 7_modelR_ENM_without_mpc.R
* 7_dismo_ENM.R and 7_dismo_ENM_diff_pseudoabs.R
* 7_biomod2_pseudoans.R (editing... here, only pseudoabsence routine)
* 7_euclidean_distance.R


** I included 2 options for modelR, one with and one without the minimum convex polygon (mpc).
** I included 2 options for dismo, one with one option of pseudoabsence table, and the other with many pseudoabsence tables
** The Euclidean distance "modelling" is a good option when you are dealing with species with few occurrence records
** Euclidean_distance_function.R (function that you need run along the 7_euclidean_distance.R)



##### References of the most scripts

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


