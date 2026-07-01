# Body size and urban affinity

## Overview

This repository contains the code, processed data, and documentation required to reproduce the analyses presented in:

Callaghan CT, et al. eLife: <https://doi.org/10.7554/eLife.109047>

The overall objective of this study was to quantify whether body size is consistently associated with species' urban affinity across the tree of life. To accomplish this, we integrated three major components:

1)  global species occurrence records from GBIF,
2)  remotely sensed estimates of urbanization, and
3)  body size measurements compiled from the published literature.

These components were combined to generate species-level urban affinity scores, assemble a harmonized body size database, fit Bayesian hierarchical models across multiple taxonomic levels, and produce all analyses and figures presented in the manuscript.

The repository follows the workflow below:

``` text
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
5. Summarize results and produce manuscript outputs
```

Each stage is described below, together with the primary scripts, required inputs, and resulting outputs.

If the goal is only to explore the prepared data and model results to reproduce the figures presented in the manuscript, focus on *Step 4. Scientific analyses: Species Urbanness Distributions (SUDs)*, *Step 4. Scientific analyses: Supporting analyses*, and *Step 5. Summarize results and produce manuscript outputs*.

## Repository contents

This GitHub repository contains the code and processed datasets required to understand and reproduce the analytical workflow presented in the manuscript.

To keep the repository lightweight and easy to navigate, several large intermediate datasets and fitted Bayesian model objects are not stored on GitHub.

A complete archived version of the repository, including all intermediate data products, SQL outputs, and fitted model objects, is available through Zenodo:

DOI: XXXXX

## Step 1. Calculate urban affinity scores

### Objective

The first stage of the workflow quantifies a continuous measure of urban affinity for every species within each biogeographic subrealm. Urban affinity is calculated by combining species occurrence records from the Global Biodiversity Information Facility (GBIF) with remotely sensed estimates of nighttime light intensity (VIIRS), following the methods described in the manuscript.

The primary output from this stage is a species-by-subrealm dataset containing urban affinity scores that forms the foundation for all downstream analyses.

## Scripts

``` text
R/scripts_to_calculate_urban_scores/
```

This directory contains the scripts used to:

download and summarize GBIF occurrence data using Google BigQuery; assign observations to One Earth bioregions and subrealms; generate spatial layers required for Google Earth Engine processing; calculate species-level urban affinity scores; and compile the final urban affinity dataset used throughout the remainder of the analysis.

## External dependencies

This workflow relies on external cloud-based services that cannot be executed directly from this repository without appropriate authentication and access.

Specifically, portions of the workflow require:

Google BigQuery Google Earth Engine Google Cloud authentication SQL queries provided in the SQL/ directory

For this reason, the scripts are provided primarily for transparency and documentation of the analytical workflow rather than as a fully executable pipeline.

## Outputs

The final product of this workflow is

``` text
urban_scores/subrealm_potential_urban_scores.RDS
```

This file contains the species-level urban affinity scores used throughout all subsequent analyses and serves as the primary input to the body size integration workflow described in Step 2.

During the calculation of urban affinity scores, several lookup tables and taxonomic reference files are generated to facilitate matching among GBIF records, spatial datasets, and downstream analyses. These are retained in the taxonomic_keys/ directory for completeness and to document the data processing workflow.

## Step 2. Prepare the body size database

### Objective

This study integrates species-specific urban affinity scores with a global database of body size measurements compiled from the published literature. The literature search, data extraction, quality control, and standardization procedures used to construct this database are described in detail in the Methods section of the manuscript.

Because many original body size datasets cannot be redistributed due to copyright or licensing restrictions, this repository begins with the processed body size database used in our analyses.

## Inputs

``` text
body_size_data_all/all_body_size.RDS
```

This dataset contains standardized body size measurements compiled across plants and animals and serves as the primary body size input for all downstream analyses.

## Step 3. Prepare analysis datasets

### Objective

The body size database is linked with the species-specific urban affinity scores to create a single integrated analysis dataset. During this step, taxonomic harmonization is performed to maximize matching between GBIF species names and the compiled body size database, accounting for taxonomic revisions and synonyms.

The resulting harmonized dataset forms the foundation for every subsequent analysis presented in the manuscript.

## Scripts

``` text
R/cleaning_and_processing_files/
```

The primary script is

create_a_dataframe_for_analysis.R

which:

joins the urban affinity scores with the compiled body size database; performs taxonomic harmonization for plants and animals separately; creates both the direct-match and harmonized analysis datasets; and summarizes the number of species recovered through taxonomic harmonization.

## Outputs

``` text
analysis_data/analysis_data.RDS
analysis_data/analysis_data_harmonized.RDS
```

analysis_data.RDS contains species that matched directly between the urban affinity and body size datasets.

analysis_data_harmonized.RDS additionally incorporates species recovered through taxonomic harmonization and is the primary dataset used throughout the remainder of the analyses.

# Step 4. Scientific analyses

The harmonized analysis dataset (`analysis_data/analysis_data_harmonized.RDS`) forms the starting point for all scientific analyses presented in the manuscript. These analyses can be divided into two complementary components: (1) empirical summaries of Species Urbanness Distributions (SUDs) and (2) Bayesian hierarchical models quantifying relationships between body size and urban affinity.

## Species Urbanness Distributions (SUDs)

### Objective

Species Urbanness Distributions (SUDs) summarize the distribution of urban affinity values within taxonomic groups and geographic regions. These analyses were used to characterize the overall shape of urban affinity distributions across the tree of life and to evaluate whether common patterns emerged among taxonomic groups and biogeographic subrealms.

The SUD analyses are descriptive and provide the empirical foundation for the manuscript by illustrating how species are distributed along the urban affinity gradient before investigating whether body size explains this variation.

### Scripts

``` text
R/SUDs/
```

This directory contains the scripts used to calculate, summarize, and visualize Species Urbanness Distributions, including the empirical summaries presented in the main text and supplementary material.

### Outputs

The primary outputs from this workflow are the empirical summaries and figures describing Species Urbanness Distributions presented in the manuscript and supplementary material.

## Bayesian hierarchical models

### Objective

Bayesian hierarchical models were used to quantify the relationship between species body size and urban affinity across the tree of life. Rather than fitting a single global model, analyses were conducted independently at five taxonomic levels (family, order, class, phylum, and kingdom), allowing relationships to be evaluated within progressively broader evolutionary groups.

All primary analyses used the harmonized analysis dataset produced in **Step 3** (`analysis_data/analysis_data_harmonized.RDS`) together with urban affinity scores calculated from VIIRS night-time lights. The Version of Record is based exclusively on the VIIRS analyses; exploratory analyses using the Global Human Modification (GHM) index were not included in this release repository.

### Scripts

``` text
R/modelling/
```

The modelling workflow follows the same general structure for each taxonomic level:

``` text
analysis_data/analysis_data_harmonized.RDS
                │
                ▼
Create taxonomic-specific analysis dataset
                │
                ▼
Determine appropriate model structure
                │
                ▼
Fit Bayesian hierarchical models
                │
                ▼
Save fitted model objects
                │
                ▼
Summarize posterior estimates
```

Separate scripts are provided for each taxonomic level:

``` text
family_level_modelling_viirs.R
order_level_modelling_viirs.R
class_level_modelling_viirs.R
phylum_level_modelling_viirs.R
kingdom_level_modelling_viirs.R
```

For the family- and order-level analyses, revised (`v2`) scripts are also included. These correspond to the final modelling workflow used in the Version of Record following additional refinement during peer review. Earlier versions have been retained for transparency and to document the development of the analysis.

### Model preparation

Each modelling script begins from the harmonized analysis dataset and performs several preprocessing steps prior to model fitting.

These include:

- selecting a single representative body size measurement for each species when multiple measurements were available;
- filtering taxonomic groups that did not meet minimum sample-size requirements for modelling;
- generating taxonomic summaries describing the number of species, observations, subrealms, and independent body size datasets represented within each group; and
- constructing taxonomic-level analysis datasets used for model fitting.

These intermediate datasets are written to:

``` text
analysis_data/
```

including files such as

``` text
final_data_for_analysis_family.RDS
summary_family.RDS
```

with analogous files produced for order, class, phylum, and kingdom analyses.

### Model fitting

Models were fitted using the **brms** package in R. Because taxonomic groups differed substantially in sample size and data availability, each taxonomic group was assigned one of several predefined model structures depending on the availability of independent subrealms and body size datasets.

Where supported by the data, models incorporated varying intercepts and slopes for biogeographic subrealms and body size metadata sources. Simpler model structures were automatically used for taxonomic groups lacking sufficient replication.

Each script iterates over every eligible taxonomic group within its respective level, fits the appropriate Bayesian hierarchical model, and saves the fitted model object for downstream summarization.

Fitted model objects are written to:

``` text
model_objects/
```

with separate subdirectories for each taxonomic level.

### Model summarization

Following model fitting, companion summarization scripts extract posterior parameter estimates, credible intervals, convergence diagnostics, and additional model summaries from the fitted Bayesian models.

These scripts produce the intermediate result files used to generate the figures and tables presented in the manuscript.

### Supporting analyses

Several additional scripts are included to document methodological decisions and sensitivity analyses performed during model development.

These include:

- **checking_metadata_interactions.R** – explores whether relationships between body size and urban affinity differ among body size metadata sources and compares the original and revised modelling approaches.
- **simulations.R** – simulation-based demonstrations illustrating the behaviour of alternative hierarchical model structures and random-effects specifications.
- **testing_some_sensitivity.R** – evaluates the sensitivity of model results to alternative modelling choices, including body size scaling and random-effects parameterization.
- **random_effects_modelling_viirs.R** and **kingdom_level_random_effects_modelling_viirs.R** – additional exploratory analyses examining alternative random-effects structures.

These scripts are included for transparency and documentation of the analytical development process but are not required to reproduce the primary results presented in the manuscript.

# Step 5. Summarize results and produce manuscript outputs

## Objective

Following Bayesian model fitting, posterior distributions and model summaries are synthesized to quantify the prevalence, strength, and direction of body size–urban affinity relationships across taxonomic groups. These scripts also generate visualizations of fitted models, summarize analytical results across taxonomic levels, and produce the figures, tables, and summary statistics presented in the main text and supplementary material.

## Scripts

``` text
R/data_summarizing_and_visualization/
```

This directory contains scripts used to:

- summarize posterior distributions across taxonomic levels;
- summarize the processed analysis datasets and urban affinity scores;
- compare alternative modelling approaches (e.g., original versus revised family-level analyses);
- quantify the strength and direction of estimated relationships;
- visualize fitted Bayesian models and posterior distributions;
- generate manuscript figures and supplementary figures; and
- produce summary statistics and intermediate tables used throughout the manuscript.

## Inputs

These scripts primarily use data from:

``` text
analysis_data/
intermediate_results/
model_objects/
urban_scores/
```

## Outputs

The primary outputs include:

- manuscript figures;
- supplementary figures;
- visualizations of fitted Bayesian models and posterior distributions;
- summary statistics describing model results across taxonomic levels; and
- intermediate tables used to generate the results presented in the manuscript.

------------------------------------------------------------------------
