# Exploring the Rule of Law in Academic Discourse: Bibliometric Description and Thematic Analysis with the Web of Science Data

## Overivew

This repository contains the replication files of the paper "Exploring the Rule of Law in Academic Discourse: Bibliometric Description and Thematic Analysis with the Web of Science Data."

> The rule of law is a foundational concept of state governance, admired for its ideals but also subject to significant controversy, particularly concerning its precise definition. Given the complexity of this evolving field, both in theory and practice, our article analyzes the existing academic literature on the rule of law. First and foremost, it aims to tackle the research question of what the thematic focus is within the rule of law literature. To accomplish this, the article extracts data from the Web of Science (WoS) and employs a bibliometric analysis that includes topic modeling and network analysis techniques. The article likewise presents a rich description of the field by elucidating its intellectual structure and identifying, among other aspects, key sources, references, and influential authors. This article serves as an invaluable resource for those who are new to the field of the rule of law, providing a concise and comprehensive overview of its complex landscape.

## Metadata and Preservation

This repository has been created with Git version control. Below is a brief description of the relevant files in this repository:

- **code\functions\clean_authors.R**. An R function that is imported into `stage_1_preprocessing.R` to clean and correct authors' names.

- **code\stage_1_preprocessing.R**. An R script that preprocesses WoS data and saves the dataset using the `pins package` versioning.

- **code\stage_2_descriptives.R**. An R script that generates our descriptive outputs.

- **code\stage_3_topic_modelling**. An R script that performs topic modelling.

- **README.md**. The main overview of this repository provides metadata, preservation information and instructions for replicating the analyses.

- **renv.lock**. The renv lock file that specifies exact package versions used in this project, ensuring a consistent computational environment for replication.

## Getting Started

The original code was written in 2024. The revised version for deposit was modularised and rerun in September 2025 using `R v4.5.0 -- How About a Twenty-Six`. It is important to note that `renv.lock` is the project's lock file, and all installed and required R packages are recorded there.

The following files should be run in order to replicate the main results of the paper:

- `stage_1_preprocessing.R`
- `stage_2_descriptives.R`
- `stage_3_topic_modelling`

> [!CAUTION]
> We are not able to share the WoS files used in `stage_1_preprocessing.R` to create the bibliographic data frame. Because WoS data are proprietary, we cannot redistribute records and, therefore, cannot guarantee strict data-level reproducibility. Clarivate's license prohibits the redistribution of WoS records, and full public data release is legally impossible. 

We were able to access WoS records using our institutional subscriptions, however, you can easily reuse this code by replacing those files with yours. At the end of `stage_1_preprocessing.R`, we provide some lines of code based on the `pins package` to save a versioned file of the processed dataset. This file can then be imported straightforwardly into `stage_2_descriptives.R` and `stage_3_topic_modelling`.

## Authors

Jarosław Kantorowicz \
Leiden University \
[ORCID iD 0000-0002-1186-5427](https://orcid.org/0000-0002-1186-5427)

Bastián González-Bustamante \
Universidad Diego Portales and Leiden University \
[ORCID iD 0000-0003-1510-6820](https://orcid.org/0000-0003-1510-6820) 

## Corresponding Author

Emails: bastian.gonzalez.b@mail.udp.cl; b.a.gonzalez.bustamante@fgga.leidenuniv.nl

### Latest Revision

September 15, 2025.
