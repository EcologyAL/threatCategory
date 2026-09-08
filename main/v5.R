setwd('~/work//RLI/')

sapply(c('ggplot2','stringr','sf','dplyr','raster','RColorBrewer','circlize',
         'patchwork','rasterVis','ggbeeswarm','ggpmisc','glmmTMB'), 
       require,character.only=T)
#pathData <- '../data/'
paths <- c(
  # Known species-range summaries and supporting model outputs
  spRangeChange= 'data_modOutput/v5/spInfo_change.csv',
  spEX='data_modOutput/v5/spEX.csv', # A3c, no extinction sp.
  spTSS='data_modOutput/v5/sp_tss.csv',
  spECO='data_modOutput/v5/sp_eco.csv',
  spSampl='data_modOutput/v5/uncertain/sampleIntensity.csv',
  
  # data unrelated with sp range maps
  spMarkAll='data/spInfo/spls_mark_all.csv',
  spLS='data/spInfo/spls.csv',
  scenario='data/scenarios_list.csv',
  IUCNredList='data/IUCNcategory/redlist_species_data/simple_summary_match_ag.csv',
  baseMap='data/map/data_1d/data_1d_land.shp',
  lifeForm='data/spInfo/species_lifefrom_NOgenusFill.csv',
  SI='data/spInfo/sampleIntensity.csv',
  
  # data related with sp range maps
  A3cNoEX='data_modOutput/v5/A3c_noEX.csv', # A3c, no extinction sp.
  A3cEX=paste0('data_modOutput/v5/A3c_ex.csv'), # A3c, only extinction sp
  A3c='data_modOutput/v5/A3c.csv', # A3c, All species 
  B2="data_modOutput/v5/B1abiii.csv",
  
  spAssAll='data_modOutput/v5/sp_assess_all.csv', # assess by 3 criterion
  spAssSim='data_modOutput/v5/sp_assess_simple.csv', 
  
  # assess integrated with IUCN red list only one scenario
  # check agian!!
  spCty='data_modOutput/v5/sp_cty.csv',
  spBiome='data_modOutput/v5/sp_biome.csv',
  spPA='data_modOutput/v5/sp_PAs.csv',
  spContinent='data/spInfo/sp_continent.csv',
  
  # distribution
  spds1d='data_modOutput/v5/sp_ds_1d.csv',
  cellInfo='data_modOutput/v5/cellInfo.csv',
  cellInfo_all_sn='data_modOutput/v5/cellInfo_all_sn.csv',
  
  #PAs
  PAs.dbf='data/map/PAs/PA_1d_30p.dbf',
  PAs.mapping='results/fig4/map1d/data_1d_land.shp',
  
  # table 
  faInfo='results/table/family/faInfo.csv',
  ctyInfo='results/table/country/ctyInfo.csv',
  # figures
  fig1_dir='results/fig1/rawFigs/',
  
  fig2_dir = 'results/fig2/rawFigs/',
  
  fig3a='results/fig3/rawFigs/figure3a.pdf', 
  fig3b='results/fig3/rawFigs/figure3b.pdf', 
  
  fig4='results/fig4/sppa/sppa_gl.jpg', 
  
  fig5a='results/fig5/rawFigs/figure5a.pdf', 
  fig5b='results/fig5/rawFigs/figure5b.pdf',
  
  # sfig
  sfig_sr_rli = 'results/sfig/sfig_sr_rli.jpg',
  sfig_map_uncertainty='results/sfig/sfig_map_uncertainty/sfig_map_uncertainty.jpg',
  sfig_mod_summary='results/sfig/mod_summary.jpg',
  sfig_RLI_AB='results/sfig/RLI_CRITERION.jpg',
  sfig_RLI_sns='results/sfig/RLI_sns.jpg',
  sfig_senarios='results/sfig/senarios.jpg',
  sfig_fa='results/sfig/sfig_fa.jpg',
  sfig_sr_category='results/sfig/SR_Category.jpg',
  sfig_noInt_a='results/sfig/notIntCompare/fig2a_notIntCircle.ps',
  sfig_noInt_b='results/sfig/notIntCompare/figure2b.jpg',
  sfig_noInt_c='results/sfig/notIntCompare/figure2c_em.jpg',
  sfig_SI='results/sfig/SI/',
  sfig_pa_cty='results/sfig/cty_deltaRLI.jpg',
  sfig_delta_rli_cty='results/sfig/delta_rli_cty.jpg',
  sfig_subsampl_map='results/sfig/subsampling/',
  stable_deltaTsp='results/table/stable_deltaTsp.csv'
)
rm(list = ls()[!ls() %in% "paths"])

# Start from known species ranges -----------------------------------------
# Supply paths['spds1d'] as a CSV with speciesKey and cell_poll columns.
# Range-change, extinction, habitat and model-quality summaries above must
# also be prepared before running the assessments below.

# code for assessment -----------------------------------------------------
source('code/IUCNcategory/A3c/A3c_ex.R') # assess by A3c
source('code/IUCNcategory/A3c/A3c.R') # assess by A3c
rm(list = ls()[!ls() %in% "paths"])

source('code/IUCNcategory/B1abiii/B1abiii.R') # clean mod outputs
rm(list = ls()[!ls() %in% "paths"])

source('code/IUCNcategory/combine.R') # combine three criterion
rm(list = ls()[!ls() %in% "paths"])

source('code/IUCNcategory/ensemble_category_simplify.R') # integrate three criterion
rm(list = ls()[!ls() %in% "paths"])

# cell info ---------------------------------------------------------------
source('code/summaryShp/cellInfo_mid.R') # RLI in mid scenario
rm(list = ls()[!ls() %in% "paths"])

source('code/summaryShp/cellInfo_all_sn.R')
rm(list = ls()[!ls() %in% "paths"])

source('code/ProtectedAreas/spPA.R')
rm(list = ls()[!ls() %in% "paths"])

# Table S -----------------------------------------------------------------

# family table 
source('code/tables/family.R') 
rm(list = ls()[!ls() %in% "paths"])
# country table
source('code/tables/country.R') 
rm(list = ls()[!ls() %in% "paths"])


# figures -----------------------------------------------------------------

# Fig. 1
source('code/mapping/fig1.R')
rm(list = ls()[!ls() %in% "paths"])

# Fig. 2
source('code/mapping/fig2.R')
rm(list = ls()[!ls() %in% "paths"])

# Fig. 3
source('code/mapping/fig3.R')
rm(list = ls()[!ls() %in% "paths"])

# Fig. 4
source('code/mapping/fig4.R')
rm(list = ls()[!ls() %in% "paths"])

# Fig. 5
source('code/mapping/fig5.R')
rm(list = ls()[!ls() %in% "paths"])



# Figure S -----------------------------------------------------------------
# Fig. S2 compare senarios
source('code/mapping/sfig/fig_scenarios.R')
rm(list = ls()[!ls() %in% "paths"])

# Fig. S3 family
source('code/mapping/sfig/fa_pa.R')
rm(list = ls()[!ls() %in% "paths"])

# Fig. S4 RLI in biome and lifeForm
source('code/mapping/sfig/sfig_biome_lifeForm.R')
rm(list = ls()[!ls() %in% "paths"])

# Fig. S5 compare scenarios in maps 
source('code/mapping/sfig/RLI_sns.R')
rm(list = ls()[!ls() %in% "paths"])

# Fig. S6 compare criterion in mid scenario 
source('code/mapping/sfig/RLI_CRITERION.R')
rm(list = ls()[!ls() %in% "paths"])

# Fig. S7 sr in each threat category 
source('code/mapping/sfig/SR_Category.R')
rm(list = ls()[!ls() %in% "paths"])

# Fig. S8 campare v2 
source('code/mapping/sfig/sfig_noInt.R')
rm(list = ls()[!ls() %in% "paths"])

# Fig. S8 country PAs
source('code/mapping/sfig/cty_pa.R')
rm(list = ls()[!ls() %in% "paths"])

# Fig. S9 cty speciesDensity
source('code/mapping/sfig/sfig_cty_speciesDensity.R')
rm(list = ls()[!ls() %in% "paths"])

# Fig. S10 RLI across biomes and life forms 
source('code/mapping/sfig/cty_pa.R')
rm(list = ls()[!ls() %in% "paths"])

# Fig. S11 RLI in cty
source('code/mapping/sfig/sfig_delta_rli_cty.R')
rm(list = ls()[!ls() %in% "paths"])

# Fig. S13-14 uncertainty map
source('code/mapping/sfig/map_uncertainty_sampl.R')
rm(list = ls()[!ls() %in% "paths"])

# Fig.sumpl map S15-16
source('code/mapping/sfig/sfig_SI.R')
rm(list = ls()[!ls() %in% "paths"])

# Fig. S17 wide range sp. & high SI
source('code/mapping/sfig/subsampl_map.R')
rm(list = ls()[!ls() %in% "paths"])

# Fig.sumpl map S18-19
rm(list = ls()[!ls() %in% "paths"])

# STable delta proportion of threathened species
source('code/tables/deltaTSp.R')
rm(list = ls()[!ls() %in% "paths"])

# STablt Cty model
file.edit('code/mapping/sfig/stable_cty_income.R')
