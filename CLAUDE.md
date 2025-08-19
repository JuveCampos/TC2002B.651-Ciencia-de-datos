# Project Configuration for Claude Code

## Project Overview
Data Science course project focusing on Machine Learning and SQL using R programming language.

## Primary Language
R

## Development Environment
- Primary language: R
- Secondary: SQL
- Focus areas: Machine Learning, Data Analysis, Statistical Computing

## Common Commands
```bash
# R commands
Rscript script.R                 # Run R script
R CMD check package/             # Check R package
R CMD INSTALL package.tar.gz     # Install R package

# Package management
# Use install.packages("package_name") in R console
# Use library(package_name) to load packages

# SQL commands (if using database files)
sqlite3 database.db < script.sql # Execute SQL script
```

## Dependencies and Libraries
Common R packages for this project:
- tidyverse (data manipulation and visualization)
- caret (machine learning)
- randomForest (random forest algorithms)
- e1071 (support vector machines)
- ggplot2 (visualization)
- dplyr (data manipulation)
- DBI/RSQLite (SQL database connectivity)

## Testing
```bash
# R testing (if using testthat)
Rscript -e "devtools::test()"
```

## Project Structure
- Data analysis notebooks and scripts in R
- SQL scripts for database operations
- Machine learning model implementations
- Statistical analysis and visualizations

## Notes
- Focus on reproducible research practices
- Document data preprocessing steps
- Include model evaluation metrics
- Maintain clean code structure for ML pipelines