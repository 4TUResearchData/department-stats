# Retrieval of departmental information using djehuty API

This repository contains an R project designed to retrieve and analyze departmental information using the djehuty API.

## Overview

The `department-stats` project facilitates the extraction of departmental data from the 4TU.ResearchData.

*NOTE: For now, code retrieves dataset from TU/e depositing authors only**

## Features

- **Data Retrieval**: Connects to the djehuty API to fetch up-to-date departmental information.
- **Data Prcoessing**: Provides tools to process the retrieved data.
- **Reporting**: Generates a visualisation on stats per department.

## Requirements

- R version 4.0 or higher
- Packages: `httr`, `jsonlite`, `dplyr`, `ggplot2`

## Installation

1. **Clone the repository**:

  ```bash
  git clone https://github.com/4TUResearchData/department-stats.git
  ```
2. Navigate to the project directory:

  ```bash
  cd department-stats
  ```
3. Restore the project environment (if using `renv`)

 ``` r
 renv::restore()
 ```
or Install required packages:

  ```r
  install.packages('httr2', 'jsonlite', 'dplyr', 'purrr', 'tidyr', 'stringr', 'ggplot2')
  ```

