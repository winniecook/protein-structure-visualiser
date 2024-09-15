# protein_structure_analyzer.py

import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from Bio import PDB
import warnings

# Suppress PDBConstructionWarnings
warnings.simplefilter('ignore', PDB.PDBExceptions.PDBConstructionWarning)

def fetch_pdb(pdb_id):
    pdb_list = PDB.PDBList()
    filename = pdb_list.retrieve_pdb_file(pdb_id, pdir='.', file_format='pdb')
    return filename

def parse_structure(filename):
    parser = PDB.PDBParser()
    structure = parser.get_structure('protein', filename)
    return structure

def calculate_phi_psi(structure):
    phi_psi = []
    for model in structure:
        for chain in model:
            polypeptide = PDB.Polypeptide.Polypeptide(chain)
            for phi, psi in polypeptide.get_phi_psi_list():
                if phi and psi:
                    phi_psi.append((np.degrees(phi), np.degrees(psi)))
    return phi_psi

def assign_secondary_structure(phi, psi):
    if -145 < phi < -35 and -70 < psi < 50:
        return 'H'  # Alpha helix
    elif -180 < phi < -40 and 90 < psi < 180:
        return 'E'  # Beta sheet
    else:
        return 'C'  # Coil

def visualize_ramachandran(phi_psi):
    phi, psi = zip(*phi_psi)
    
    plt.figure(figsize=(10, 8))
    plt.scatter(phi, psi, alpha=0.7)
    plt.xlabel('Phi (degrees)')
    plt.ylabel('Psi (degrees)')
    plt.title('Ramachandran Plot')
    plt.xlim(-180, 180)
    plt.ylim(-180, 180)
    plt.axhline(y=0, color='k', linestyle='--', linewidth=0.5)
    plt.axvline(x=0, color='k', linestyle='--', linewidth=0.5)
    plt.grid(True, linestyle=':', alpha=0.5)
    plt.savefig('ramachandran_plot.png')
    plt.close()
    print("Ramachandran plot saved as 'ramachandran_plot.png'")

def visualize_structure(structure, ss_assignments):
    fig = plt.figure(figsize=(10, 8))
    ax = fig.add_subplot(111, projection='3d')
    
    colors = {'H': 'red', 'E': 'yellow', 'C': 'green'}
    
    for model in structure:
        for chain in model:
            coords = []
            ss_colors = []
            for residue in chain:
                if 'CA' in residue:
                    coords.append(residue['CA'].coord)
                    ss = ss_assignments.get(residue.id[1], 'C')
                    ss_colors.append(colors[ss])
            
            coords = np.array(coords)
            ax.plot(coords[:, 0], coords[:, 1], coords[:, 2], color='gray', alpha=0.5)
            for i in range(len(coords) - 1):
                ax.plot(coords[i:i+2, 0], coords[i:i+2, 1], coords[i:i+2, 2], color=ss_colors[i])

    ax.set_title("Protein Structure")
    ax.set_xlabel('X')
    ax.set_ylabel('Y')
    ax.set_zlabel('Z')
    plt.savefig('protein_structure_3d.png')
    plt.close()
    print("3D protein structure visualization saved as 'protein_structure_3d.png'")

def main():
    pdb_id = "1GZX"  # Hemoglobin structure
    filename = fetch_pdb(pdb_id)
    structure = parse_structure(filename)
    phi_psi = calculate_phi_psi(structure)

    print(f"Protein structure loaded: {pdb_id}")
    print(f"Number of residues with calculated phi/psi angles: {len(phi_psi)}")

    ss_composition = {'H': 0, 'E': 0, 'C': 0}
    ss_assignments = {}
    print("\nPhi/Psi Angles and Secondary Structure Assignments:")
    print("Residue\tPhi\tPsi\tSecondary Structure")
    print("-" * 50)
    for i, (phi, psi) in enumerate(phi_psi, start=1):
        ss = assign_secondary_structure(phi, psi)
        ss_composition[ss] += 1
        ss_assignments[i] = ss
        print(f"{i}\t{phi:.2f}\t{psi:.2f}\t{ss}")

    print("\nSecondary Structure Composition:")
    total = sum(ss_composition.values())
    for ss, count in ss_composition.items():
        percentage = (count / total) * 100
        print(f"{ss}: {count} ({percentage:.2f}%)")

    visualize_ramachandran(phi_psi)
    visualize_structure(structure, ss_assignments)

if __name__ == "__main__":
    main()
