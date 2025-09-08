# threatCategory
#  R code note
### R code note

This repository contains R scripts for “overlooked risks of plant  biodiversity 
loss under climate change revealed by a global assessment”.

### Repository Structure


|---- generation length

||--- generation_lenth_model.R
  main code for generation-length mode

||--- targetSpan_table.csv
  targert corresponding time span according to growth from and ecological group



|---- lifeFomrRation
||--- lifeFormSR.R
  calculate species richness of each life form
||--- lifeFormRatio.R
  calculate the porportion of species number(species richness) of each life 
    form to all species


|---- IUCNcategory
||-- combine.R 
  combine threat categories from different iucn criteria
||-- ensemble_category_simplify 
  ensemble categories incorporating future climate change with categories in 
  current iucn red list
||--- IUCN_category_map.R
  mapping species richness in each categories
||--- IUCN_map.R
  mapping red list index
||--- A3c 
  codes for species based on IUCN red list criterion A3c
|||-- A3c.R 
  main code
||--- B1abiii 
  codes for species based on IUCN red list criterion B1abiii
|||-- B1abiii.R 
  main code
|||-- B1abiii_NT.R 
  classify near-threathened species
||--- D2
|||-- D2.R 
  main code
||--- redListCateory
|||-- summary_redList.R 
  summary threat categories in red list


|---- ProtectedAreas
||--- PAsRation
  summary PA coverage ratio for each species and grid cells in space


|---- habitat_prediction
||--- modeling_habitat.R
  fit models for habitat
||--- modeling_habitat_par.R
  fit models for habitat; multithread setting
||--- pred_habitat.R
  habitat prediction by the fitted habitat models 
  
|---- mapping
  codes to generates figures

|--- synthesize
||-- sp_catagory.R
  code synthesize species threat categories
  
|---- spRange
  codes to generate species range maps in present and under future climate change
