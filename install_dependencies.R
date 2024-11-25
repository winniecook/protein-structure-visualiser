# install_dependencies.R

# Print status messages
status_message <- function(msg, type = "info") {
  symbol <- switch(type,
    "info" = "ℹ",
    "success" = "✔",
    "error" = "✖",
    "warning" = "⚠"
  )
  cat(sprintf("%s %s\n", symbol, msg))
}

# Function to safely install CRAN packages
safe_install_cran <- function(package_name) {
  if (!require(package_name, character.only = TRUE, quietly = TRUE)) {
    status_message(sprintf("Installing %s from CRAN...", package_name))
    tryCatch({
      install.packages(package_name, quiet = TRUE)
      if (require(package_name, character.only = TRUE, quietly = TRUE)) {
        status_message(sprintf("Successfully installed %s", package_name), "success")
      } else {
        status_message(sprintf("Failed to install %s", package_name), "error")
      }
    }, error = function(e) {
      status_message(sprintf("Error installing %s: %s", package_name, e$message), "error")
    })
  } else {
    status_message(sprintf("%s is already installed", package_name), "success")
  }
}

# Function to safely install GitHub packages
safe_install_github <- function(repo) {
  package_name <- basename(repo)  # Extract package name from repo path
  if (!require(package_name, character.only = TRUE, quietly = TRUE)) {
    status_message(sprintf("Installing %s from GitHub...", repo))
    tryCatch({
      if (!require(remotes)) {
        install.packages("remotes", quiet = TRUE)
      }
      remotes::install_github(repo, quiet = TRUE)
      if (require(package_name, character.only = TRUE, quietly = TRUE)) {
        status_message(sprintf("Successfully installed %s", package_name), "success")
      } else {
        status_message(sprintf("Failed to install %s", package_name), "error")
      }
    }, error = function(e) {
      status_message(sprintf("Error installing %s: %s", repo, e$message), "error")
    })
  } else {
    status_message(sprintf("%s is already installed", package_name), "success")
  }
}

# Clear console and print header
cat("\014")  # Clear console
status_message("Starting installation of required packages...\n")

# Install required CRAN packages
cran_packages <- c(
  "shiny",
  "shinydashboard",
  "bio3d",
  "plotly",
  "DT",
  "data.table",
  "parallel",
  "future",
  "promises",
  "logger"
)

# Install CRAN packages
status_message("\nInstalling packages from CRAN:")
for (pkg in cran_packages) {
  safe_install_cran(pkg)
}

# Install r3dmol from GitHub
status_message("\nInstalling r3dmol from GitHub:")
safe_install_github("swsoyee/r3dmol")

# Create necessary directories
dirs <- c("logs", "uploads", "results")
for (dir in dirs) {
  if (!dir.exists(dir)) {
    dir.create(dir)
    status_message(sprintf("Created directory: %s", dir), "success")
  } else {
    status_message(sprintf("Directory already exists: %s", dir), "info")
  }
}

# Verify installations
status_message("\nVerifying installations:")
all_packages <- c(cran_packages, "r3dmol")
missing_packages <- character(0)

for (pkg in all_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    missing_packages <- c(missing_packages, pkg)
  }
}

if (length(missing_packages) > 0) {
  status_message("Some packages are missing:", "error")
  for (pkg in missing_packages) {
    status_message(sprintf("  - %s", pkg), "error")
  }
  status_message("\nPlease try installing missing packages manually or check for errors above.", "warning")
} else {
  status_message("All packages installed successfully!", "success")
}

# Final instructions
cat("\n=== Next Steps ===\n")
status_message("1. Check that all packages installed correctly")
status_message("2. Ensure all required files are in your working directory:")
status_message("   - global.R")
status_message("   - ui.R")
status_message("   - server.R")
status_message("3. Run the application using shiny::runApp()")