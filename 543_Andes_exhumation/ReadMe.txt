These scripts predit rates of exhumation across the Andes Mountains using an MLP nueral network model. Code to train, validate, test,analyze, and save the MLP model can be found in the '543_MLP_model.ipynb' file. The '543_final_analysis.ipynb' code  reads in all the same information, but is used to just plot the reuslts.

This research aims to take advantage of the strong relationships between climate and tectonics to model rates of exhumation over the last 0 to 2 Ma across the Andes. This work 1) finds that rates of exhumation, as typically measured by thermochronology analyses, can be predicted using widely available environmental datasets, 2) uses this model to predict exhumation across the Andes, and 3) examines preliminary relationships between modeled exhumation and regional tectonic setting and expression. 
- A final written report summarizing the model and results can be found in the repo as a .pdf.
- Exumation data used to train the model was kindly and privately provided by N Stalder et al., (2020), and not included in this repo (thus the model cannot be run alone). 
- Additional environemntal dataset used to train the model include local geology (USGS, 1999), precipitation (NASA TRMM, 2018), denudation (Fagundes et al., (2020), and sediment yield (Fagundes et al., (2020)).  

Data Used
- Fagundes, H. de O., Fan, F.M., Dias de Paiva, R.C., Siqueira, V.A., Buarque, D.C., Kornowski, L.W., Laipelt, L., and Collischonn, W., 2020, Sediment flows in South America supported by daily hydrologic-hydrodynamic modeling: Hydrology preprint, doi:10.1002/essoar.10503046.2.

- NASA, 2018, TRMM (TMPA/3B43) Rainfall Estimate L3 1 month 0.25 degree x 0.25 degree V7:, doi:10.5067/TRMM/TMPA/MONTH/7.

- Stalder, N.F., Herman, F., Fellin, M.G., Coutand, I., Aguilar, G., Reiners, P.W., and Fox, M., 2020, The relationships between tectonics, climate and exhumation in the Central Andes (18–36°S): Evidence from low-temperature thermochronology: Earth-Science Reviews, v. 210, p. 103276, doi:10.1016/j.earscirev.2020.103276.

- United States Geological Survey (USGS), 1999, Maps Showing Geology, Oil and Gas Fields and Geologic Provinces of the South America Region: Open-File Report Open-File Report.


