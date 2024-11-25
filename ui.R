# ui.R
ui <- dashboardPage(
  dashboardHeader(title = "Protein Structure Visualiser"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Upload", tabName = "upload", icon = icon("upload")),
      menuItem("Analysis", tabName = "analysis", icon = icon("chart-bar")),
      menuItem("Visualisation", tabName = "visualization", icon = icon("eye"))
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #ffffff;
        }
        .box {
          margin-bottom: 15px;
          min-height: 400px;
          box-shadow: 0 1px 3px rgba(0,0,0,0.12);
        }
        .controls-section {
          background-color: white;
          padding: 15px;
          border-radius: 4px;
          margin-bottom: 15px;
          border: 1px solid #ddd;
        }
        .style-info {
          margin-top: 5px;
          font-size: 0.9em;
          color: #666;
          padding: 5px;
          background: #f8f9fa;
          border-radius: 3px;
        }
        .scheme-info {
          margin-top: 8px;
          padding: 8px;
          background: #f1f3f5;
          border-left: 3px solid #4a90e2;
          font-size: 0.9em;
        }
        .sequence-viewer {
          font-family: monospace;
          font-size: 14px;
          background: white;
          padding: 15px;
          border-radius: 4px;
          border: 1px solid #ddd;
          overflow-x: auto;
          white-space: nowrap;
        }
        .legend-item {
          display: flex;
          align-items: center;
          margin-bottom: 8px;
          padding: 3px;
          border-radius: 3px;
          transition: background-color 0.2s;
        }
        .legend-item:hover {
          background-color: #f8f9fa;
        }
        .colour-box {
          width: 24px;
          height: 24px;
          margin-right: 10px;
          border: 1px solid #ddd;
          border-radius: 3px;
        }
      "))
    ),
    
    tabItems(
      # Upload tab
      tabItem(
        tabName = "upload",
        fluidRow(
          box(
            title = "Upload Structure",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            fileInput(
              "structure_file",
              "Choose PDB/mmCIF file",
              accept = c(".pdb", ".ent", ".cif")
            ),
            div(
              class = "style-info",
              HTML("
                <strong>Supported file formats:</strong><br>
                • PDB (.pdb)<br>
                • mmCIF (.cif)<br>
                • ENT (.ent)<br>
                <br>
                <strong>Maximum file size:</strong> 50MB
              ")
            )
          )
        )
      ),

      # Analysis tab
      tabItem(
        tabName = "analysis",
        fluidRow(
          box(
            title = "Protein Information",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            div(
              class = "controls-section",
              fluidRow(
                column(
                  width = 4,
                  h4("Basic Information", class = "text-primary"),
                  uiOutput("protein_basic_info")
                ),
                column(
                  width = 4,
                  h4("Structure Details", class = "text-primary"),
                  uiOutput("protein_structure_info")
                ),
                column(
                  width = 4,
                  h4("Publication", class = "text-primary"),
                  uiOutput("protein_publication_info")
                )
              )
            )
          )
        ),
        fluidRow(
          box(
            title = "Validation Summary",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            DTOutput("validation_summary")
          )
        ),
        fluidRow(
          box(
            title = "Ramachandran Plot",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("rama_plot", height = "400px")
          ),
          box(
            title = "B-factor Analysis",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("bfactor_plot", height = "400px")
          )
        )
      ),

      # Visualisation tab
      tabItem(
        tabName = "visualization",
        fluidRow(
          box(
            title = "Structure and Sequence Visualisation",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            fluidRow(
              # Controls column
              column(
                width = 3,
                div(
                  class = "structure-controls",
                  # Display style selection with specific color scheme options
                  div(
                    class = "controls-section",
                    selectInput(
                      "display_style",
                      "Display Style",
                      choices = c(
                        "Cartoon" = "cartoon",
                        "Stick" = "stick",
                        "Sphere" = "sphere",
                        "Line" = "line"
                      ),
                      selected = "cartoon"
                    ),
                    
                    # Dynamic color scheme options based on display style
                    uiOutput("color_scheme_options"),
                    
                    div(
                      class = "style-info",
                      uiOutput("style_description")
                    ),
                    
                    # Transparency control
                    sliderInput(
                      "transparency",
                      "Transparency",
                      min = 0,
                      max = 1,
                      value = 0,
                      step = 0.1
                    )
                  ),
                  
                  # Chain visibility
                  div(
                    class = "controls-section",
                    h4("Chain Visibility"),
                    uiOutput("chain_controls")
                  ),
                  
                  # Dynamic color legend
                  div(
                    class = "controls-section",
                    h4("Colour Legend"),
                    uiOutput("color_legend")
                  )
                )
              ),
              
              # Main visualization column
              column(
                width = 9,
                # Structure viewer
                div(
                  class = "controls-section",
                  if (exists("r3dmolOutput")) {
                    r3dmolOutput("structure_viewer", height = "500px")
                  } else {
                    div(
                      class = "alert alert-warning",
                      "3D visualisation requires the r3dmol package.",
                      tags$br(),
                      "Install using: ",
                      tags$code("remotes::install_github('swsoyee/r3dmol')")
                    )
                  }
                ),
                
                # Sequence tools
                div(
                  class = "controls-section",
                  fluidRow(
                    column(
                      width = 12,
                      textInput(
                        "sequence_search",
                        "Search Sequence",
                        placeholder = "Enter sequence (e.g., RGD, NPXY)"
                      ),
                      div(
                        class = "style-info",
                        HTML("
                          <strong>Search Tips:</strong><br>
                          • Use 3-letter codes (ALA, GLY) or 1-letter codes (A, G)<br>
                          • Search for motifs like 'RGD' or 'NPXY'<br>
                          • Case-insensitive search<br>
                          <br>
                          <strong>Examples:</strong><br>
                          • Single residue: GLY or G<br>
                          • Motif: RGD or KDEL<br>
                          • Multiple residues: RGDS
                        ")
                      )
                    )
                  ),
                  fluidRow(
                    column(
                      width = 6,
                      checkboxInput("show_numbers", "Show Residue Numbers", TRUE)
                    )
                  )
                ),
                
                # Sequence viewer
                div(
                  class = "sequence-viewer",
                  uiOutput("sequence_viewer")
                )
              )
            )
          )
        )
      )
    )
  )
)