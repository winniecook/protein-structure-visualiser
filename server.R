# server.R
server <- function(input, output, session) {
  rv <- reactiveValues(
    current_file = NULL,
    validation_results = NULL,
    selected_residue = NULL,
    pdb_id = NULL,
    sequence_data = NULL,
    min_bfactor = NULL,
    max_bfactor = NULL,
    highlighted_residues = NULL,
    current_style = NULL
  )

  CPK_COLORS <- list(
    C = "#808080", 
    N = "#0000FF", 
    O = "#FF0000", 
    S = "#FFA500", 
    H = "#FFFFFF", 
    P = "#FFA500",
    default = "#FFFF00"
  )

  output$color_scheme_options <- renderUI({
    req(input$display_style)
    
    choices <- switch(input$display_style,
      "cartoon" = c(
        "Secondary Structure" = "ss",
        "Chain" = "chain",
        "Rainbow" = "rainbow",
        "Residue Type" = "restype",
        "B-factors" = "bfactor"
      ),
      "stick" = c(
        "CPK (Atoms)" = "cpk",
        "Secondary Structure" = "ss",
        "Chain" = "chain",
        "Residue Type" = "restype"
      ),
      "sphere" = c(
        "CPK (Atoms)" = "cpk",
        "Residue Type" = "restype",
        "Chain" = "chain"
      ),
      "line" = c(
        "Secondary Structure" = "ss",
        "Chain" = "chain",
        "Residue Type" = "restype"
      )
    )
    
    selectInput(
      "color_scheme",
      "Colour Scheme",
      choices = choices,
      selected = switch(input$display_style,
        "cartoon" = "ss",
        "stick" = "cpk",
        "sphere" = "cpk",
        "line" = "ss"
      )
    )
  })

  output$style_description <- renderUI({
    req(input$display_style, input$color_scheme)
    desc <- switch(input$display_style,
      "cartoon" = "Ribbon representation showing protein backbone trace",
      "stick" = "Atomic detail showing bonds between atoms",
      "sphere" = "Space-filling model showing atomic radii",
      "line" = "Simple wire representation of protein structure"
    )
    
    color_desc <- switch(input$color_scheme,
      "cpk" = "Standard atomic colors (CPK coloring)",
      "ss" = "Helix (Red), Sheet (Yellow), Loop (Blue)",
      "chain" = "Each chain colored uniquely",
      "restype" = "Colored by amino acid properties",
      "bfactor" = "Blue (low) to Red (high) B-factors",
      "rainbow" = "N-terminus (Blue) to C-terminus (Red)"
    )
    
    HTML(sprintf("<strong>Style:</strong> %s<br><strong>Coloring:</strong> %s", 
                desc, color_desc))
  })

  observeEvent(input$structure_file, {
    req(input$structure_file)
    file <- input$structure_file
    ext <- tools::file_ext(file$datapath)
    
    if (!ext %in% ALLOWED_EXTENSIONS) {
      showNotification("Please upload a PDB or mmCIF file", type = "error")
      return(NULL)
    }
    
    if (file$size > MAX_FILE_SIZE) {
      showNotification("File size must be under 50MB", type = "error")
      return(NULL)
    }
    
    withProgress(message = 'Processing structure...', value = 0, {
      tryCatch({
        incProgress(0.2, detail = "Reading structure")
        pdb <- bio3d::read.pdb(file$datapath)
        
        incProgress(0.3, detail = "Calculating torsion angles")
        torsions <- bio3d::torsion.pdb(pdb)
        valid_residues <- !is.na(torsions$phi) & !is.na(torsions$psi)
        
        incProgress(0.4, detail = "Processing B-factors")
        ca_atoms <- pdb$atom$elety == "CA"
        bfactors <- pdb$atom$b[ca_atoms]
        rv$min_bfactor <- min(bfactors, na.rm = TRUE)
        rv$max_bfactor <- max(bfactors, na.rm = TRUE)
        
        incProgress(0.5, detail = "Computing secondary structure")
        ss <- try({
          dssp_result <- bio3d::dssp(pdb)
          if (!is.null(dssp_result)) dssp_result else rep("loop", sum(ca_atoms))
        }, silent = TRUE)
        
        if (inherits(ss, "try-error")) {
          ss <- rep("loop", sum(ca_atoms))
        }
        
        incProgress(0.6, detail = "Processing sequence")
        chains <- unique(pdb$atom$chain)
        sequence_data <- lapply(chains, function(chain) {
          chain_atoms <- pdb$atom[pdb$atom$chain == chain & pdb$atom$elety == "CA",]
          list(
            sequence = chain_atoms$resid,
            numbers = chain_atoms$resno,
            chain = chain,
            ss = if (!is.null(ss)) ss[pdb$atom$chain == chain] else rep("loop", length(chain_atoms$resid))
          )
        })
        names(sequence_data) <- chains
        
        if (!dir.exists("uploads")) dir.create("uploads")
        output_file <- file.path("uploads", basename(file$name))
        file.copy(file$datapath, output_file, overwrite = TRUE)
        
        rv$validation_results <- list(
          structure = pdb,
          torsions = list(
            phi = torsions$phi[valid_residues],
            psi = torsions$psi[valid_residues]
          ),
          bfactors = bfactors,
          resnames = pdb$atom$resid[ca_atoms],
          chain = pdb$atom$chain[ca_atoms],
          resno = pdb$atom$resno[ca_atoms],
          ss = ss
        )
        rv$current_file <- output_file
        rv$sequence_data <- sequence_data
        
        showNotification("Structure processed successfully", type = "message")
        
      }, error = function(e) {
        showNotification(sprintf("Error processing file: %s", e$message), type = "error")
      })
    })
  })

  output$protein_basic_info <- renderUI({
    req(rv$validation_results)
    pdb <- rv$validation_results$structure
    headers <- pdb$header$record
    
    title <- grep("^TITLE", headers, value = TRUE)
    title <- sub("^TITLE\\s+\\d*\\s*", "", title)
    title <- paste(title, collapse = " ")
    
    compnd <- grep("^COMPND", headers, value = TRUE)
    molecule <- grep("MOLECULE:", compnd, value = TRUE)[1]
    molecule <- sub(".*MOLECULE:\\s*", "", molecule)
    
    source <- grep("^SOURCE", headers, value = TRUE)
    organism <- grep("ORGANISM:", source, value = TRUE)[1]
    organism <- sub(".*ORGANISM:\\s*", "", organism)
    
    tagList(
      div(class = "info-item", tags$strong("Title: "), title),
      div(class = "info-item", tags$strong("Molecule: "), molecule),
      div(class = "info-item", tags$strong("Organism: "), organism)
    )
  })

  output$protein_structure_info <- renderUI({
    req(rv$validation_results)
    pdb <- rv$validation_results$structure
    
    resolution <- if (!is.null(pdb$resolution)) {
      sprintf("%.2f Å", pdb$resolution)
    } else {
      "Not available"
    }
    
    chains <- length(unique(pdb$atom$chain))
    residues <- length(unique(paste(pdb$atom$chain, pdb$atom$resno)))
    atoms <- nrow(pdb$atom)
    
    tagList(
      div(class = "info-item", tags$strong("Resolution: "), resolution),
      div(class = "info-item", tags$strong("Chains: "), chains),
      div(class = "info-item", tags$strong("Residues: "), residues),
      div(class = "info-item", tags$strong("Atoms: "), atoms)
    )
  })

  output$protein_publication_info <- renderUI({
    req(rv$validation_results)
    headers <- rv$validation_results$structure$header$record
    
    jrnl <- grep("^JRNL", headers, value = TRUE)
    jrnl <- sub("^JRNL\\s+\\d*\\s*", "", jrnl)
    jrnl <- paste(jrnl, collapse = " ")
    
    auth <- grep("^AUTHOR", headers, value = TRUE)
    auth <- sub("^AUTHOR\\s+\\d*\\s*", "", auth)
    auth <- paste(auth, collapse = " ")
    
    tagList(
      div(class = "info-item", tags$strong("Authors: "), auth),
      div(class = "info-item", tags$strong("Publication: "), jrnl)
    )
  })

  output$chain_controls <- renderUI({
    req(rv$validation_results)
    chains <- unique(rv$validation_results$chain)
    
    tagList(
      checkboxInput("show_all_chains", "Show All Chains", value = TRUE),
      div(
        class = "chain-controls",
        lapply(chains, function(chain) {
          checkboxInput(
            paste0("chain_", chain),
            paste("Chain", chain),
            value = TRUE
          )
        })
      )
    )
  })

  observeEvent(input$show_all_chains, {
    req(rv$validation_results)
    chains <- unique(rv$validation_results$chain)
    for(chain in chains) {
      updateCheckboxInput(session, 
                         paste0("chain_", chain), 
                         value = input$show_all_chains)
    }
  })

  output$color_legend <- renderUI({
    req(input$color_scheme)
    
    legend_items <- switch(input$color_scheme,
      "cpk" = lapply(names(CPK_COLORS), function(atom) {
        list(colour = CPK_COLORS[[atom]], label = sprintf("%s atoms", atom))
      }),
      "ss" = list(
        list(colour = SS_COLOURS$helix, label = "Alpha Helix"),
        list(colour = SS_COLOURS$sheet, label = "Beta Sheet"),
        list(colour = SS_COLOURS$loop, label = "Loop/Other")
      ),
      "chain" = {
        if (!is.null(rv$validation_results)) {
          chains <- unique(rv$validation_results$chain)
          colours <- rainbow(length(chains))
          mapply(function(chain, colour) {
            list(colour = colour, label = sprintf("Chain %s", chain))
          }, chains, colours, SIMPLIFY = FALSE)
        } else {
          list(list(colour = "#CCCCCC", label = "No chains loaded"))
        }
      },
      "rainbow" = list(
        list(colour = "#0000FF", label = "N-terminus"),
        list(colour = "#00FF00", label = "Middle"),
        list(colour = "#FF0000", label = "C-terminus")
      ),
      "restype" = lapply(names(RESIDUE_PROPERTIES), function(type) {
        residues <- paste(RESIDUE_PROPERTIES[[type]], collapse = ", ")
        list(
          colour = RESIDUE_COLOURS[[type]],
          label = sprintf("%s (%s)", type, residues)
        )
      }),
      "bfactor" = list(
        list(colour = "#0000FF", label = "Low B-factor"),
        list(colour = "#FFFFFF", label = "Medium B-factor"),
        list(colour = "#FF0000", label = "High B-factor")
      )
    )
    
    div(
      class = "colour-legend",
      lapply(legend_items, function(item) {
        div(
          class = "legend-item",
          div(
            class = "colour-box",
            style = sprintf("background-color: %s;", item$colour)
          ),
          div(
            class = "legend-label",
            item$label
          )
        )
      })
    )
  })

  output$sequence_viewer <- renderUI({
    req(rv$sequence_data)
    sequence_elements <- lapply(names(rv$sequence_data), function(chain) {
      if (!is.null(input[[paste0("chain_", chain)]]) && 
          !input[[paste0("chain_", chain)]]) {
        return(NULL)
      }
      
      chain_data <- rv$sequence_data[[chain]]
      residues <- mapply(function(residue, number, ss_type) {
        color <- switch(input$color_scheme,
          "cpk" = CPK_COLORS$C,
          "ss" = SS_COLOURS[[tolower(ss_type)]],
          "chain" = rainbow(length(names(rv$sequence_data)))[
            which(names(rv$sequence_data) == chain)
          ],
          "rainbow" = {
            pos <- which(chain_data$numbers == number)
            rainbow(length(chain_data$numbers))[pos]
          },
          "restype" = get_residue_colour(residue),
          "bfactor" = {
            b_value <- rv$validation_results$bfactors[
              rv$validation_results$chain == chain & 
              rv$validation_results$resno == number
            ]
            if (length(b_value) > 0) {
              normalized <- (b_value - rv$min_bfactor) / 
                          (rv$max_bfactor - rv$min_bfactor)
              rgb(normalized, 0, 1-normalized)
            } else {
              "#CCCCCC"
            }
          }
        )
        
        tags$span(
          class = "sequence-residue",
          id = sprintf("res_%s_%d", chain, number),
          style = sprintf(
            "background-color: %s; opacity: %s;",
            color,
            1 - (input$transparency %||% 0)
          ),
          `data-chain` = chain,
          `data-resno` = number,
          `data-restype` = residue,
          title = sprintf(
            "Chain %s: %s%d\nSecondary Structure: %s",
            chain, residue, number, toupper(ss_type)
          ),
          onclick = sprintf(
            "Shiny.setInputValue('selected_residue', {chain: '%s', resno: %d});",
            chain, number
          ),
          residue
        )
      }, 
      chain_data$sequence,
      chain_data$numbers,
      chain_data$ss,
      SIMPLIFY = FALSE)
      
      div(
        class = "sequence-chain",
        div(
          class = "sequence-header",
          sprintf("Chain %s", chain)
        ),
        if (input$show_numbers) {
          div(
            class = "sequence-numbers",
            paste(chain_data$numbers, collapse = " ")
          )
        },
        div(
          class = "sequence-line",
          residues
        )
      )
    })
    
    div(
      class = "sequence-container",
      sequence_elements
    )
    })

  output$structure_viewer <- renderR3dmol({
    req(rv$validation_results, rv$current_file)
    
    view <- r3dmol() %>%
      m_add_model(data = rv$current_file) %>%
      m_zoom_to()
    
    chains <- unique(rv$validation_results$chain)
    visible_chains <- chains[sapply(chains, function(chain) {
      !is.null(input[[paste0("chain_", chain)]]) && input[[paste0("chain_", chain)]]
    })]
    
    opacity <- 1 - (input$transparency %||% 0)
    
    view <- view %>%
      m_set_style(style = list(
        cartoon = list(hidden = TRUE),
        stick = list(hidden = TRUE),
        line = list(hidden = TRUE),
        sphere = list(hidden = TRUE)
      ))
    
    if (length(visible_chains) > 0) {
      style_properties <- switch(input$display_style,
        "cartoon" = list(cartoon = list(
          hidden = FALSE,
          opacity = opacity,
          thickness = 0.3
        )),
        "stick" = list(stick = list(
          hidden = FALSE,
          opacity = opacity,
          radius = 0.2,
          bondRadius = 0.1,
          bonds = TRUE
        )),
        "sphere" = list(sphere = list(
          hidden = FALSE,
          opacity = opacity,
          radius = 0.7
        )),
        "line" = list(line = list(
          hidden = FALSE,
          opacity = opacity,
          linewidth = 2
        ))
      )
      
      for (chain in visible_chains) {
        base_style <- style_properties
        
        if (input$color_scheme == "cpk" && input$display_style %in% c("stick", "sphere")) {
          view <- view %>%
            m_set_style(
              sel = list(chain = chain),
              style = modifyList(base_style, list(
                colorscheme = list(
                  prop = "elem",
                  map = CPK_COLORS
                )
              ))
            )
        } else {
          colored_style <- switch(input$color_scheme,
            "ss" = modifyList(base_style, list(
              colorscheme = list(
                prop = "ss",
                map = SS_COLOURS
              )
            )),
            "chain" = {
              color <- rainbow(length(chains))[which(chains == chain)]
              modifyList(base_style, list(color = color))
            },
            "rainbow" = modifyList(base_style, list(color = "spectrum")),
            "restype" = modifyList(base_style, list(
              colorscheme = list(
                prop = "resname",
                map = RESIDUE_COLOURS
              )
            )),
            "bfactor" = modifyList(base_style, list(colorfunc = "temperature"))
          )
          
          view <- view %>%
            m_set_style(
              sel = list(chain = chain),
              style = colored_style
            )
        }
      }
    }
    
    if (!is.null(input$sequence_search) && nchar(input$sequence_search) > 0) {
      search_text <- toupper(input$sequence_search)
      for (chain in visible_chains) {
        chain_data <- rv$sequence_data[[chain]]
        sequence <- paste(chain_data$sequence, collapse = "")
        matches <- gregexpr(search_text, sequence)[[1]]
        
        if (matches[1] != -1) {
          pattern_length <- nchar(search_text)
          for (match_pos in matches) {
            residue_indices <- match_pos:(match_pos + pattern_length - 1)
            residue_numbers <- chain_data$numbers[residue_indices]
            
            view <- view %>%
              m_add_style(
                sel = list(chain = chain, resi = residue_numbers),
                style = list(
                  stick = list(
                    hidden = FALSE,
                    color = "yellow",
                    radius = 0.3
                  )
                )
              )
          }
        }
      }
    }
    
    if (!is.null(rv$selected_residue)) {
      view <- view %>%
        m_add_style(
          sel = list(
            chain = rv$selected_residue$chain,
            resi = rv$selected_residue$resno
          ),
          style = list(
            stick = list(
              hidden = FALSE,
              color = "yellow",
              radius = 0.3
            )
          )
        )
    }
    
    view %>% m_set_background_color("#FFFFFF")
  })
  
  output$rama_plot <- renderPlotly({
    req(rv$validation_results)
    
    valid_indices <- which(!is.na(rv$validation_results$torsions$phi) & 
                          !is.na(rv$validation_results$torsions$psi))
    
    plot_data <- data.frame(
      phi = rv$validation_results$torsions$phi[valid_indices],
      psi = rv$validation_results$torsions$psi[valid_indices],
      resname = rv$validation_results$resnames[valid_indices],
      resno = rv$validation_results$resno[valid_indices],
      chain = rv$validation_results$chain[valid_indices]
    )
    
    plot_ly(
      data = plot_data,
      x = ~phi,
      y = ~psi,
      type = "scatter",
      mode = "markers",
      text = ~sprintf(
        "Residue: %s%d<br>Chain: %s<br>Phi: %.1f°<br>Psi: %.1f°",
        resname, resno, chain, phi, psi
      ),
      hoverinfo = "text",
      marker = list(
        color = "#1f77b4",
        size = 6,
        line = list(color = "#444", width = 1)
      )
    ) %>%
      layout(
        title = "Ramachandran Plot",
        xaxis = list(
          title = "Phi (degrees)",
          range = c(-180, 180),
          zeroline = TRUE,
          gridcolor = "#E2E2E2"
        ),
        yaxis = list(
          title = "Psi (degrees)",
          range = c(-180, 180),
          zeroline = TRUE,
          gridcolor = "#E2E2E2"
        ),
        plot_bgcolor = "#FFFFFF",
        paper_bgcolor = "#FFFFFF"
      )
  })
  
  output$bfactor_plot <- renderPlotly({
    req(rv$validation_results)
    
    plot_data <- data.frame(
      resno = seq_along(rv$validation_results$bfactors),
      bfactor = rv$validation_results$bfactors,
      resname = rv$validation_results$resnames,
      chain = rv$validation_results$chain
    )
    
    plot_ly(
      data = plot_data,
      x = ~resno,
      y = ~bfactor,
      split = ~chain,
      type = "scatter",
      mode = "lines+markers",
      text = ~sprintf(
        "Residue: %s%d<br>Chain: %s<br>B-factor: %.2f",
        resname, resno, chain, bfactor
      ),
      hoverinfo = "text",
      marker = list(size = 6)
    ) %>%
      layout(
        title = "B-factors by Residue",
        xaxis = list(
          title = "Residue Number",
          gridcolor = "#E2E2E2"
        ),
        yaxis = list(
          title = "B-factor",
          gridcolor = "#E2E2E2"
        ),
        plot_bgcolor = "#FFFFFF",
        paper_bgcolor = "#FFFFFF"
      )
  })
  
  output$validation_summary <- renderDT({
    req(rv$validation_results)
    
    summary_data <- data.frame(
      Metric = c(
        "Total Residues",
        "Average B-factor",
        "B-factor StdDev",
        "Number of Chains",
        "Secondary Structure Elements"
      ),
      Value = c(
        length(rv$validation_results$bfactors),
        round(mean(rv$validation_results$bfactors, na.rm = TRUE), 2),
        round(sd(rv$validation_results$bfactors, na.rm = TRUE), 2),
        length(unique(rv$validation_results$chain)),
        paste(
          sprintf("Helix: %d", sum(rv$validation_results$ss == "H", na.rm = TRUE)),
          sprintf("Sheet: %d", sum(rv$validation_results$ss == "E", na.rm = TRUE)),
          sprintf("Loop: %d", sum(rv$validation_results$ss == "L", na.rm = TRUE)),
          sep = ", "
        )
      )
    )
    
    datatable(
      summary_data,
      options = list(
        dom = 't',
        ordering = FALSE,
        pageLength = -1
      ),
      rownames = FALSE,
      class = 'cell-border stripe'
    )
  })
  
  session$onSessionEnded(function() {
    isolate({
      if (!is.null(rv$current_file)) {
        try(unlink(rv$current_file), silent = TRUE)
      }
      try(unlink(list.files("uploads", full.names = TRUE)), silent = TRUE)
    })
  })
}