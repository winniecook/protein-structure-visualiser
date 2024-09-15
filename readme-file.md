# Protein Structure Analyzer

This project provides a Python-based tool for analyzing and visualizing protein structures. It fetches protein data from the Protein Data Bank (PDB), calculates phi and psi angles, assigns secondary structures, and generates both Ramachandran plots and 3D structure visualizations.

## Features

- Fetch protein structures from the PDB
- Calculate phi and psi angles for each residue
- Assign secondary structures based on phi/psi angles
- Generate Ramachandran plots
- Create 3D visualizations of protein structures with color-coded secondary structures
- Provide detailed output of phi/psi angles and secondary structure assignments

## Installation

1. Clone this repository:
   ```
   git clone https://github.com/yourusername/protein-structure-analyzer.git
   cd protein-structure-analyzer
   ```

2. Install the required dependencies:
   ```
   pip install -r requirements.txt
   ```

## Usage

1. Run the script with Python:
   ```
   python protein_structure_analyzer.py
   ```

2. By default, the script analyzes the hemoglobin structure (PDB ID: 1GZX). To analyze a different protein, modify the `pdb_id` variable in the `main()` function.

3. The script will generate two output files:
   - `ramachandran_plot.png`: A Ramachandran plot of the protein's phi and psi angles
   - `protein_structure_3d.png`: A 3D visualization of the protein structure with color-coded secondary structures

4. Console output will provide detailed information about phi/psi angles and secondary structure assignments for each residue, as well as a summary of the secondary structure composition.

## Customization

- To analyze a different protein, change the `pdb_id` variable in the `main()` function.
- Adjust the secondary structure assignment criteria in the `assign_secondary_structure()` function if needed.
- Modify the color scheme for the 3D visualization in the `visualize_structure()` function.


## Acknowledgments

- [Biopython](https://biopython.org/) for providing tools to work with protein structures
- [Matplotlib](https://matplotlib.org/) for data visualization capabilities
- [NumPy](https://numpy.org/) for numerical computations
