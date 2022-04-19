
This tab allows to create a PheWAS analysis between two cohorts in the *CohortOperations*'s Workbench . 


#### Cohorts to compare

The first step is to select the cases and controls cohorts. 

This will show an initial assessment of the patients in the colors. How many patients in each cohort have phenotipic data available for the analysis. How many patients ovelap between cases and controls 

#### Analysis Settings

The PheWAS analysis can be limited to compare tho codes from a list of registers and vocabularies (by default all are included). 

Also, excluding the Endpoints in the comparison is optional. 

**Advance vocabulary options**
In some vocabularies the precision of the code define the hierarchy. 

On this tap we can reduce the precision of the codes before the comparison for each of these vocabularies. 

For example, if we set precision 3 in ICD10fi, code J45.9 and J45 are both counted as the same code, J45. 


#### Run PheWAS analiyis & download resutls

Once the analysis settings are ready you can click `Run PheWAS analiyis & download resutls` button. 

This will generate the results report and prompt a download dialog in the browser, where you can select where to save it.
Notice that the default download folder in Firefox is `/home/ivm/Donloads`. 

The report is a .html exploratory dashboard that can be opened with the Firefox browser. 

