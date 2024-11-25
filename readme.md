# Protein Structure Visualiser

An interactive R Shiny application for visualising and analysing protein structures.

## Features
- 3D protein structure visualisation with multiple display styles
- Interactive sequence viewer
- Structure analysis tools including Ramachandran plots and B-factor analysis
- Multiple colouring schemes
- Chain visibility controls
- Structure validation metrics

## Prerequisites
- R (version 4.0.0 or higher)
- RStudio (recommended)

## Installation

1. Clone repository:
```bash
git clone https://github.com/yourusername/protein-structure-visualiser.git
cd protein-structure-visualiser
```

2. Install dependencies:
```r
source("install_dependencies.R")
```

## Usage
1. Launch RStudio
2. Open project directory
3. Run application:
```r
shiny::runApp()
```

## Input Data
- Supports PDB and mmCIF files
- Maximum file size: 50MB

## Contributing
1. Fork repository
2. Create feature branch
3. Commit changes
4. Push to branch
5. Create Pull Request

## Licence
This project is licensed under the MIT Licence
