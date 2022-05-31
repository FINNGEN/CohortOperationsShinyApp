
This tab allows to import cohorts from external sources into the **CohortOperations**'s Workbench. 

Two external sources are supported: 


#### from Atlas
Select the CDM-database/data-freeze and or more cohorts to import. 

Cohort must be previously generated in Atlas for the selected CDM-database/data-freeze. 

To confirm this you can navigate to **Atlas** > "Cohort Definitions" > open your target cohort > tab "Generation". Here, "Generation Status" must be "COMPLETE" for the selected CDM-database/data-freeze. If it is not, click on "Generate". 
run


#### from File
Choose a file in tab-separated-values (.tsv) from your local machine.

Columns in the .tsv file must follow the **CohortData** format. 

If your are importing a file outputted by other **Sandbox** tool, you don't need to worry as they follow this format. 

However, if you created the file check the specifications in [FINNGEN/FinnGenTableTypes](https://github.com/FINNGEN/FinnGenTableTypes). 

