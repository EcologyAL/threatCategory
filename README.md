# Climate change and plant extinction risk

R scripts for estimating generation length, predicting habitat, assessing species against IUCN Red List criteria, and summarizing species richness, Red List Index, and protected-area coverage.

## Repository structure

.
├── generation_length/
│   ├── generation_length_model.R       # Main code for the generation-length model
│   └── targetSpan_table.csv            # Target time spans by growth form and ecological group
├── lifeFormRatio/
│   ├── lifeFormSR.R                    # Calculate species richness for each life form
│   └── lifeFormRatio.R                 # Calculate each life form's proportion of total species richness
├── IUCNcategory/
│   ├── combine.R                       # Combine threat categories across IUCN criteria
│   ├── combine_criterion_mid.R         # Combine categories for the middle scenario
│   ├── ensemble_category_simplify.R     # Integrate future assessments with current IUCN Red List categories
│   ├── IUCN_category_map.R             # Map species richness by threat category
│   ├── IUCN_map.R                      # Map the Red List Index
│   ├── A3c/
│   │   ├── A3c.R                       # Main assessment under criterion A3c
│   │   ├── A3c_ex.R                    # Handle projected extinctions
│   │   └── A3c_mid.R                   # Assessment for the middle scenario
│   ├── B1abiii/
│   │   ├── B1abiii.R                   # Main assessment under criterion B1abiii
│   │   └── B1abiii_NT.R                # Classify Near Threatened species
│   ├── D2/
│   │   └── D2.R                        # Main assessment under criterion D2
│   └── redListCategory/
│       └── summary_redList.R           # Summarize existing Red List threat categories
├── ProtectedAreas/
│   ├── PAsRatio.R                      # Summarize protected-area coverage for species and grid cells
│   └── spPA.R                          # Summarize species-level protected-area coverage
├── habitat_prediction/
│   ├── modeling_habitat.R              # Fit habitat models
│   ├── modeling_habitat_par.R          # Fit habitat models with parallel processing
│   └── pred_habitat.R                  # Predict habitat using fitted models
├── mapping/                            # Generate main and supplementary figures
│   └── sfig/                           # Supplementary figures
├── synthesize/
│   └── sp_category.R                   # Synthesize species threat categories
├── main/
│   └── v5.R                            # Main workflow, starting from known species ranges
├── summaryShp/                         # Spatial summaries and Red List Index maps
├── tables/                             # Family, country, biome, and time-span summaries
├── uncertainty/                        # Model performance, rare species, and subsampling analyses
└── FILE_MAP.md                         # Complete original-to-publication filename mapping
