
This tab allows to create a new cohort by operating the cohorts in the *CohortOperations*'s Workbench. 

In the middle section an *UpSet-plot* shows the overlap of patients in all the cohorts currently in the Workbench. 

Changing the operations form will output a **Result cohort** on the lower part. 
Provenance of the patients in the  **Result cohort** will be indicated in the *UpSet-plot* in black. 


#### Entry cohorts 
Here we select the cohorts that define the COHORT_START_DATE and COHORT_END_DATE in the **Result cohort**.
However, if the **Result cohort** do not need this information, this step can be omited.  

All the patients belonging to the cohorts selected in "Select entry cohorts" are union in the new cohort. 
For overlapping patients, we must decide whether the "new cohort starts with" the "first" or "last" COHORT_START_DATE; and 
whether the "new cohort ends with" the "first" or "last" COHORT_END_DATE. 

#### Operation cohorts
An operation expression can be build by dragging elements from **Operations Elements** into **Operate entry cohorts**. 

If an entry cohort has been created, this will be the first element on the **Operation Expression**. 


#### Result cohort
Once we have the desired **Result cohort**, it can be copied to the *CohortOperations*'s Workbench. 
