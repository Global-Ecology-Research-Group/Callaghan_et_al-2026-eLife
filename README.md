# Body size and urban affinity

## Overview

This repository contains the code, processed data, and documentation required to reproduce the analyses presented in:

Callaghan CT, et al. eLife: https://doi.org/10.7554/eLife.109047

The overall objective of this study was to quantify whether body size is consistently associated with species' urban affinity across the tree of life. To accomplish this, we integrated three major components:

1) global species occurrence records from GBIF,
2) remotely sensed estimates of urbanization, and
3) body size measurements compiled from the published literature.

These components were combined to generate species-level urban affinity scores, assemble a harmonized body size database, fit Bayesian hierarchical models across multiple taxonomic levels, and produce all analyses and figures presented in the manuscript.

The repository follows the workflow below:

```text
1. Calculate urban affinity scores
                │
                ▼
2. Compile and harmonize body-size data
                │
                ▼
3. Prepare analysis datasets
                │
                ▼
4. Scientific analyses
   ├── Species Urbanness Distributions (SUDs)
   └── Bayesian hierarchical models
                │
                ▼
5. Produce figures, tables, and manuscript outputs
```

Each stage is described below, together with the primary scripts, required inputs, and resulting outputs.

## Step 1. Calculate urban affinity scores
### Objective

The first stage of the workflow quantifies a continuous measure of urban affinity for every species within each biogeographic subrealm. Urban affinity is calculated by combining species occurrence records from the Global Biodiversity Information Facility (GBIF) with remotely sensed estimates of nighttime light intensity (VIIRS), following the methods described in the manuscript.

The primary output from this stage is a species-by-subrealm dataset containing urban affinity scores that forms the foundation for all downstream analyses.

Scripts
R/scripts_to_calculate_urban_scores/

This directory contains the scripts used to:

download and summarize GBIF occurrence data using Google BigQuery;
assign observations to One Earth bioregions and subrealms;
generate spatial layers required for Google Earth Engine processing;
calculate species-level urban affinity scores; and
compile the final urban affinity dataset used throughout the remainder of the analysis.
External dependencies

This workflow relies on external cloud-based services that cannot be executed directly from this repository without appropriate authentication and access.

Specifically, portions of the workflow require:

Google BigQuery
Google Earth Engine
Google Cloud authentication
SQL queries provided in the SQL/ directory

For this reason, the scripts are provided primarily for transparency and documentation of the analytical workflow rather than as a fully executable pipeline.

Output

The final product of this workflow is

urban_scores/subrealm_potential_urban_scores.RDS

This file contains the species-level urban affinity scores used throughout all subsequent analyses and serves as the primary input to the body size integration workflow described in Step 2.

## Step 2. Prepare the body size database
### Objective

This study integrates species-specific urban affinity scores with a global database of body size measurements compiled from the published literature. The literature search, data extraction, quality control, and standardization procedures used to construct this database are described in detail in the Methods section of the manuscript.

Because many original body size datasets cannot be redistributed due to copyright or licensing restrictions, this repository begins with the processed body size database used in our analyses.

Input
body_size_data_all/all_body_size.RDS

This dataset contains standardized body size measurements compiled across plants and animals and serves as the primary body size input for all downstream analyses.

## Step 3. Prepare analysis datasets
### Objective

The body size database is linked with the species-specific urban affinity scores to create a single integrated analysis dataset. During this step, taxonomic harmonization is performed to maximize matching between GBIF species names and the compiled body size database, accounting for taxonomic revisions and synonyms.

The resulting harmonized dataset forms the foundation for every subsequent analysis presented in the manuscript.

Scripts
R/cleaning_and_processing_files/

The primary script is

create_a_dataframe_for_analysis.R

which:

joins the urban affinity scores with the compiled body size database;
performs taxonomic harmonization for plants and animals separately;
creates both the direct-match and harmonized analysis datasets; and
summarizes the number of species recovered through taxonomic harmonization.
Outputs
analysis_data/analysis_data.RDS
analysis_data/analysis_data_harmonized.RDS

analysis_data.RDS contains species that matched directly between the urban affinity and body size datasets.

analysis_data_harmonized.RDS additionally incorporates species recovered through taxonomic harmonization and is the primary dataset used throughout the remainder of the analyses.