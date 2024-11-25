# global.R
library(shiny)
library(shinydashboard)
library(bio3d)
library(plotly)
library(DT)
library(data.table)
library(parallel)
library(future)
library(promises)
library(logger)
library(stringr)
library(jsonlite)
library(httr)
library(shinyjs)

# Try to load r3dmol
if (!require("r3dmol")) {
  message("r3dmol package not found. 3D visualisation will be disabled.")
  message("Install using: remotes::install_github('swsoyee/r3dmol')")
}

# Initialize parallel processing
plan(multisession)

# Configure logging
log_threshold(INFO)
if (!dir.exists("logs")) {
  dir.create("logs")
}
log_appender(appender_file("logs/structure_validator.log"))

# Constants
ALLOWED_EXTENSIONS <- c("pdb", "ent", "cif")
MAX_FILE_SIZE <- 50 * 1024 * 1024  # 50MB

# Color schemes
SS_COLOURS <- list(
  helix = "#FF0000",     # Red
  sheet = "#FFFF00",     # Yellow
  loop = "#0000FF"       # Blue
)

# Residue properties and colors
RESIDUE_PROPERTIES <- list(
  hydrophobic = c("ALA", "VAL", "LEU", "ILE", "MET", "PHE", "TRP", "PRO"),
  polar = c("SER", "THR", "CYS", "TYR", "ASN", "GLN"),
  negative = c("ASP", "GLU"),
  positive = c("LYS", "ARG", "HIS"),
  special = c("GLY")
)

RESIDUE_COLOURS <- list(
  hydrophobic = "#CCCCCC",  # Grey
  polar = "#FF0000",        # Red
  negative = "#0000FF",     # Blue
  positive = "#FF00FF",     # Purple
  special = "#33FF33"       # Green
)

# One and three letter code mappings
AA_CODES <- list(
  "A" = "ALA", "C" = "CYS", "D" = "ASP", "E" = "GLU", "F" = "PHE",
  "G" = "GLY", "H" = "HIS", "I" = "ILE", "K" = "LYS", "L" = "LEU",
  "M" = "MET", "N" = "ASN", "P" = "PRO", "Q" = "GLN", "R" = "ARG",
  "S" = "SER", "T" = "THR", "V" = "VAL", "W" = "TRP", "Y" = "TYR"
)

# Create reverse mapping
AA_CODES_REV <- setNames(names(AA_CODES), unlist(AA_CODES))

# Helper functions
get_residue_property <- function(residue) {
  for(prop in names(RESIDUE_PROPERTIES)) {
    if(residue %in% RESIDUE_PROPERTIES[[prop]]) {
      return(prop)
    }
  }
  return("other")
}

get_residue_colour <- function(residue) {
  prop <- get_residue_property(residue)
  return(RESIDUE_COLOURS[[prop]] %||% "#CCCCCC")
}

get_ss_colour <- function(ss_type) {
  ss_type <- tolower(ss_type)
  if(ss_type %in% c("h", "g", "i")) return(SS_COLOURS$helix)
  if(ss_type %in% c("e", "b")) return(SS_COLOURS$sheet)
  return(SS_COLOURS$loop)
}

# Function to normalize B-factors
normalize_bfactors <- function(bfactors) {
  min_b <- min(bfactors, na.rm = TRUE)
  max_b <- max(bfactors, na.rm = TRUE)
  normalized <- (bfactors - min_b) / (max_b - min_b)
  return(normalized)
}

# Function to convert between one and three letter codes
convert_aa_code <- function(seq, to = "three") {
  if(to == "three") {
    sapply(strsplit(seq, "")[[1]], function(x) AA_CODES[[x]] %||% x)
  } else {
    sapply(seq, function(x) AA_CODES_REV[[x]] %||% x)
  }
}

# Create necessary directories
for(dir in c("uploads", "logs", "results")) {
  if (!dir.exists(dir)) {
    dir.create(dir)
  }
}