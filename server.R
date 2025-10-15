server <- function(input, output, session) {
  
  # == 1. CONNEXION PRINCIPALE À LA BASE ------------------------------------
  conn <- get_db_conn()
  session$onSessionEnded(function() {
    if (dbIsValid(conn)) dbDisconnect(conn)
  })
  
  
  # == 2. RÉACTIFS : STRUCTURES DE DONNÉES ----------------------------------
  all_tables <- reactive({
    tryCatch({
      conn_temp <- get_db_conn()
      on.exit(dbDisconnect(conn_temp), add = TRUE)
      
      tables <- dbListTables(conn_temp)
      exclude <- c("city", "admin_log", "audit_log", "arron_pop_fixed", "tree_genus", "tree_species")
      filtered_tables <- setdiff(tables, exclude)
      
      list(
        bird   = filtered_tables[grepl("^bird", filtered_tables)],
        tree = filtered_tables[grepl("^tree", filtered_tables)],
        autres = setdiff(
          filtered_tables,
          c(filtered_tables[grepl("^bird", filtered_tables)],
            filtered_tables[grepl("^tree", filtered_tables)])
        )
      )
    }, error = function(e) {
      showNotification("Erreur lors de la récupération des tables.", type = "error")
      list()
    })
  })
  
  available_tables <- reactive({
    groups   <- all_tables()
    selected <- input$selected_projects
    if (is.null(selected) || length(selected) == 0) return(character(0))
    unlist(groups[selected], use.names = FALSE)
  })
  
  
  #' Nettoie et prépare une table extraite de la BDD
  #' - Convertit geometry en texte si `sf`
  #' - Renomme WKT selon le nom de la table
  #' - Supprime les colonnes "blob" ou listes
  clean_export_table <- function(df, table_name) {
    # 1. Si sf : extraire WKT
    if (inherits(df, "sf")) {
      df$geometry <- st_as_text(df$geometry)
      df <- as.data.frame(df)
    }
    
    # 2. Si colonne WKT (manuelle) → renommer selon table
    if ("WKT" %in% names(df)) {
      new_wkt_col <- switch(
        table_name,
        "permanent_plot" = "WKT_plots",
        "tree_individuals" = "WKT_trees",
        paste0("WKT_", table_name)
      )
      names(df)[names(df) == "WKT"] <- new_wkt_col
    }
    
    # 3. Supprimer colonnes problématiques (blobs, listes, etc.)
    bad_cols <- names(df)[sapply(df, function(col) inherits(col, "blob") || is.list(col))]
    df <- df[, setdiff(names(df), bad_cols), drop = FALSE]
    
    # 4. Ajout optionnel du nom de la table
    df$table_name <- table_name
    
    return(df)
  }
  
  city_choices <- reactive({
    tryCatch({
      conn_temp <- get_db_conn()
      on.exit(dbDisconnect(conn_temp), add = TRUE)
      dbGetQuery(conn_temp, "SELECT city_ID, fullname AS city_name FROM city")
    }, error = function(e) {
      showNotification("Erreur chargement villes", type = "error")
      data.frame(city_ID = character(0), city_name = character(0))
    })
  })
  
  available_plots <- reactive({
    req(input$city_select)
    tryCatch({
      conn_temp <- get_db_conn()
      on.exit(dbDisconnect(conn_temp), add = TRUE)
      df <- dbGetQuery(conn_temp, glue("
        SELECT plot_ID, ST_AsText(WKT) AS WKT
        FROM permanent_plot
        WHERE city_ID = '{input$city_select}'
        ORDER BY plot_ID
      "))
      st_as_sf(df, wkt = "WKT", crs = 4326)
    }, error = function(e) {
      showNotification("Erreur chargement placettes", type = "error")
      st_sf(plot_ID = character(0), geometry = st_sfc())
    })
  })
  
  # ---- Fusion multi-clés (à mettre en haut de server.R ou global.R si partagé) ----
  smart_merge <- function(tables_list) {
    merge_keys <- c("plot_ID", "tree_ID", "species_id", "genus_id")
    
    Reduce(function(x, y) {
      common_keys <- intersect(names(x), names(y))
      join_keys <- intersect(common_keys, merge_keys)
      
      if (length(join_keys) == 0) {
        bind_cols(x, y)
      } else {
        full_join(x, y, by = join_keys)
      }
    }, tables_list)
  }
  
  
  
  read_sf_from_db <- function(conn, query, wkt_col = "WKT", crs = 4326) {
    df <- dbGetQuery(conn, query)
    
    if (nrow(df) == 0 || !(wkt_col %in% names(df))) {
      return(st_sf(geometry = st_sfc(), crs = crs))
    }
    
    # S'assurer que la colonne WKT est en texte
    df[[wkt_col]] <- as.character(df[[wkt_col]])
    
    # Nettoyer WKT : remplacer vides par NA
    df[[wkt_col]][is.na(df[[wkt_col]]) | df[[wkt_col]] == ""] <- NA
    
    # Supprimer lignes sans géométrie
    df <- df[!is.na(df[[wkt_col]]), , drop = FALSE]
    
    if (nrow(df) == 0) {
      return(st_sf(geometry = st_sfc(), crs = crs))
    }
    
    # Conversion sécurisée
    sf_obj <- tryCatch({
      st_as_sf(df, wkt = wkt_col, crs = crs)
    }, error = function(e) {
      message("Erreur conversion WKT -> sf : ", e$message)
      st_sf(geometry = st_sfc(), crs = crs)
    })
    
    return(sf_obj)
  }
  
  
  drawn_polygon <- reactiveVal(NULL)
  
  # == 3. UI DYNAMIQUE ------------------------------------------------------
  
  # Choix des villes
  output$city_selector <- renderUI({
    cities <- city_choices()
    if (nrow(cities) > 0) {
      selectInput(
        "city_select",
        "Choisir une ville :",
        choices = setNames(cities$city_ID, cities$city_name),
        selected = "2"
      )
    } else {
      div("Aucune ville trouvée.")
    }
  })
  
  # Choix des tables
  output$table_selector <- renderUI({
    choices <- available_tables()
    if (length(choices) == 0)
      return(div("Aucune table disponible pour ces projets."))
    
    tagList(
      selectInput(
        "selected_table_name",
        "Choisir une ou plusieurs tables :",
        choices  = choices,
        selected = NULL,
        multiple = TRUE
      )
    )
  })
  
  # Choix des placettes
  output$plot_selector <- renderUI({
    plots <- available_plots()
    if (nrow(plots) > 0) {
      selectInput(
        "plot_select",
        "Choisir une ou plusieurs placettes :",
        choices = plots$plot_ID,
        selected = plots$plot_ID,
        multiple = TRUE
      )
    } else {
      div("Aucune placette disponible.")
    }
  })
  
  # Sélection des genres et essences d’arbres
  output$tree_selector <- renderUI({
    req("tree" %in% input$selected_projects)
    
    genus_choices <- tree_genus_choices()
    req(length(genus_choices) > 0)
    
    species_choices <- tree_species_choices()
    req(length(species_choices) > 0)
    
    tagList(
      tags$hr(),
      h4("Sélection des arbres"),
      
      selectInput(
        "tree_genus",
        label = sprintf("Genres disponibles (%d) :", length(genus_choices)),
        choices = genus_choices,
        selected = input$tree_genus,
        multiple = TRUE
      ),
      
      selectInput(
        "tree_species",
        label = sprintf("Espèces disponibles (%d) :", length(species_choices)),
        choices = species_choices,
        selected = input$tree_species,
        multiple = TRUE
      )
    )
  })
  
  # Panneau de gauche : ville, placette, arbres
  output$top_left <- renderUI({
    fluidRow(
      column(
        width = 6,
        h4("Sélection des parcelles"),
        uiOutput("city_selector"),
        tags$div(style = "margin-bottom: 10px;"),
        uiOutput("plot_selector"),
        tags$div(style = "margin-top: 5px;"),
        actionButton("select_all_plots", "Tout sélectionner"),
        actionButton("deselect_all_plots", "Désélectionner"),
        uiOutput("tree_selector")
      ),
      column(
        width = 6,
        h4("Sélection des projets"),
        checkboxGroupInput(
          "selected_projects",
          "Choisir les projets :",
          choices = c(bird = "bird", insect = "insect", tree = "tree"),
          selected = character(0)
        ),
        actionButton("select_all_projects", "Tout sélectionner projets"),
        actionButton("deselect_all_projects", "Désélectionner projets"),
        tags$hr(),
        h4("Tables disponibles"),
        uiOutput("table_selector")
      )
    )
  })
  
  # Panneau de droite : carte Leaflet
  output$top_right <- renderUI({
    withSpinner(leafletOutput("map"))
  })
  
  
  # == 4. OBSERVERS ---------------------------------------------------------
  observeEvent(input$select_all_plots, {
    plots <- available_plots()
    if (nrow(plots) > 0)
      updateSelectInput(session, "plot_select", selected = plots$plot_ID)
  })
  
  observeEvent(input$deselect_all_plots, {
    updateSelectInput(session, "plot_select", selected = character(0))
  })
  
  observeEvent(input$select_all_projects, {
    updateCheckboxGroupInput(session, "selected_projects", selected = c("bird", "insect","tree"))
  })
  
  observeEvent(input$deselect_all_projects, {
    updateCheckboxGroupInput(session, "selected_projects", selected = character(0))
  })
  
  observeEvent(input$zoom_plots, {
    req(input$plot_select)  # Assure qu’au moins une placette est sélectionnée
    
    plots_sf <- available_plots()
    req(inherits(plots_sf, "sf"), nrow(plots_sf) > 0)
    
    selected <- plots_sf %>% filter(plot_ID %in% input$plot_select)
    req(nrow(selected) > 0)
    
    bounds <- st_bbox(selected)
    
    leafletProxy("map") %>%
      fitBounds(
        lng1 = bounds$xmin,
        lat1 = bounds$ymin,
        lng2 = bounds$xmax,
        lat2 = bounds$ymax
      )
  })
  
  # == 5. LEAFLET : CARTE ---------------------------------------------------
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions()) %>%
      addMapPane("arrondissements", zIndex = 410) %>%
      addMapPane("placettes", zIndex = 420) %>%
      addMapPane("arbres", zIndex = 500) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -73.5673, lat = 45.5017, zoom = 11) %>%
      addLayersControl(
        overlayGroups = c("arrondissements", "placettes", "arbres"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      addDrawToolbar(  
        targetGroup = "drawn",
        polygonOptions = drawPolygonOptions(showArea = TRUE, repeatMode = FALSE),
        editOptions = editToolbarOptions(edit = TRUE, remove = TRUE),
        singleFeature = TRUE
      )
  })
  
  observe({
    leafletProxy("map") %>%
      clearGroup("arrondissements") %>%
      clearGroup("placettes") %>%
      clearGroup("arbres")
    
    # Arrondissements — toujours chargés
    arrondissements <- arrondissements_sf()
    if (nrow(arrondissements) > 0 && inherits(arrondissements, "sf")) {
      leafletProxy("map") %>%
        addPolygons(
          data = arrondissements,
          label = ~nom_arr,
          color = "#2171b5",
          fillColor = "#6baed6",
          fillOpacity = 0.2,
          weight = 1,
          group = "arrondissements",
          options = pathOptions(pane = "arrondissements"),
          highlightOptions = highlightOptions(color = "#08306b", weight = 2, bringToFront = FALSE)
        )
    }
    
    # Placettes
    if (!is.null(input$plot_select) && length(input$plot_select) > 0) {
      plots_sf <- available_plots()
      selected_plots <- plots_sf %>% filter(plot_ID %in% input$plot_select)
      
      if (nrow(selected_plots) > 0 && inherits(selected_plots, "sf")) {
        leafletProxy("map") %>%
          addPolygons(
            data = selected_plots,
            label = ~plot_ID,
            fillColor = "#74c476",
            color = "#238b45",
            weight = 2,
            fillOpacity = 0.5,
            group = "placettes",
            options = pathOptions(pane = "placettes"),
            highlightOptions = highlightOptions(color = "#00441b", weight = 5)
          )
      }
    }
    
    # Arbres 
    if ("tree" %in% input$selected_projects) {
      arbres_sf <- tree_points_sf()
      
      if (nrow(arbres_sf) > 0) {
        leafletProxy("map") %>%
          addCircleMarkers(
            data = arbres_sf,
            radius = 2,
            fillColor = "#004529",
            color = "#004529",
            stroke = TRUE,
            weight = 1,
            fillOpacity = 1,
            group = "arbres",
            label = ~glue("{Raw_Taxonomic_Identifier}"),
            options = pathOptions(pane = "arbres"),
            labelOptions = labelOptions(
              direction = "auto",
              style = list("font-weight" = "normal"),
              textsize = "12px",
              highlightOptions = highlightOptions(color = "#004529", weight = 5)
            )
          )
      }
    }
  })
  
  # == 6. ARRONDISSEMENTS ---------------------------------------------------
  arrondissements_sf <- reactive({
    tryCatch({
      conn_temp <- get_db_conn()
      on.exit(dbDisconnect(conn_temp), add = TRUE)
      
      # Pas besoin de ST_AsText car WKT est déjà du texte
      df <- dbGetQuery(conn_temp, "
      SELECT nom_arr, WKT FROM arron_pop_fixed
    ")
      
      # Convertir le WKT en objet sf
      st_as_sf(df, wkt = "WKT", crs = 4326)
    }, error = function(e) {
      showNotification("Erreur chargement arrondissements", type = "error")
      st_sf(nom_arr = character(0), geometry = st_sfc())
    })
  })
  
  # == 8. TABLEAU DE LA BD ---------------------------------------  
  output$selected_table <- renderDT({
    req(input$selected_table_name, input$plot_select)
    
    conn_temp <- get_db_conn()
    on.exit(dbDisconnect(conn_temp), add = TRUE)
    
    tryCatch({
      tables_list <- lapply(input$selected_table_name, function(tbl) {
        df <- if (tbl == "permanent_plot") {
          plots_str <- paste(shQuote(input$plot_select), collapse = ", ")
          dbGetQuery(conn_temp, glue("
          SELECT * FROM permanent_plot
          WHERE plot_ID IN ({plots_str})
        "))
        } else if (tbl == "tree_individuals") {
          filtered_tree_data() %>% st_drop_geometry()
        } else {
          dbReadTable(conn_temp, tbl)
        }
        
        # Filtrer par plot_ID si présent
        if ("plot_ID" %in% names(df)) {
          df$plot_ID <- as.character(df$plot_ID)
          df <- df[df$plot_ID %in% input$plot_select, , drop = FALSE]
        }
        
        clean_export_table(df, tbl)
      })
      
      # Fusion multi-clés
      df_joined <- smart_merge(tables_list)
      
      # Affichage final
      datatable(
        df_joined,
        extensions = c("Buttons", "ColReorder"),
        options = list(
          dom = 'Bfrtip',
          buttons = list(
            list(extend = 'colvis', text = 'Colonnes'),
            list(extend = 'csv', text = 'Exporter CSV', exportOptions = list(columns = ':visible'))
          ),
          scrollX = TRUE,
          colReorder = TRUE,
          pageLength = 10
        ),
        class = 'stripe hover row-border nowrap'
      )
      
    }, error = function(e) {
      showNotification("Erreur : affichage des données impossible.", type = "error")
      datatable(data.frame(Erreur = "Impossible de lire ou fusionner les tables sélectionnées"))
    })
  })
  
  
  # == 9. DESSIN POLYGONE DE SELECTION ET REINITIALISATION---------------------------------------
  observeEvent(input$map_draw_new_feature, {
    feat <- input$map_draw_new_feature
    if (!is.null(feat)) {
      tryCatch({
        # Conversion GeoJSON en sf via geojsonsf
        geojson_text <- jsonlite::toJSON(feat, auto_unbox = TRUE)
        poly <- geojsonsf::geojson_sf(geojson_text)
        drawn_polygon(poly)
      }, error = function(e) {
        showNotification("Erreur lors du traitement du polygone dessiné.", type = "error")
        print(e)
      })
    }
  })
  
  # Supprimer le polygone s’il est effacé
  observeEvent(input$map_draw_deleted_features, {
    drawn_polygon(NULL)
  })
  
  observeEvent(input$reset_drawn, {
    # Réinitialiser la valeur stockée
    drawn_polygon(NULL)
    
    # Effacer les dessins de la carte
    leafletProxy() %>%
      clearGroup("drawn")
    
    showNotification("Polygone de sélection réinitialisé.", type = "message", duration = 3)
  })
  
  # == 10. FILTRE ARBRE ------------------------------------------------------
  filtered_tree_data <- reactive({
    req(input$plot_select)
    tryCatch({
      conn <- get_db_conn()
      on.exit(dbDisconnect(conn), add = TRUE)
      
      plots_str <- paste(shQuote(input$plot_select), collapse = ", ")
      
      df_sf <- read_sf_from_db(conn, glue("
      SELECT 
        ti.tree_ID,
        ti.Object_ID,
        ti.Raw_Taxonomic_Identifier,
        tm.DBH,
        tm.is_estimated,
        tm.is_hedge,
        tm.rowid,
        tg.genus_latin_name AS Genus,
        ts.species_latin_name AS Species,
        ti.is_hybrid,
        ti.Cultivar,
        ti.Land_use_type,
        ti.is_private,
        ti.plot_ID,
        ti.oldplot_ID,
        ti.WKT
      FROM tree_individuals ti
      JOIN tree_species ts ON ti.species_id = ts.species_id
      JOIN tree_genus tg ON ts.genus_id = tg.genus_id
      LEFT JOIN tree_measurement tm 
        ON ti.tree_ID = tm.tree_id AND tm.year = 2023
      WHERE ti.plot_ID IN ({plots_str})
    "))
      
      # Filtres optionnels
      if (!is.null(input$tree_genus) && length(input$tree_genus) > 0) {
        df_sf <- df_sf %>% filter(Genus %in% input$tree_genus)
      }
      
      if (!is.null(input$tree_species) && length(input$tree_species) > 0) {
        df_sf <- df_sf %>% filter(Species %in% input$tree_species)
      }
      
      poly <- drawn_polygon()
      if (!is.null(poly)) {
        df_sf <- st_filter(df_sf, poly, .predicate = st_intersects)
      }
      
      df_sf
    }, error = function(e) {
      showNotification("Erreur chargement arbres filtrés", type = "error")
      st_sf(plot_ID = character(0), Genus = character(0), Species = character(0), geometry = st_sfc())
    })
  })
  
  
  tree_genus_choices <- reactive({
    df <- filtered_tree_data()
    req(nrow(df) > 0)
    
    sort(unique(df$Genus[!is.na(df$Genus)]))
  })
  
  
  tree_species_choices <- reactive({
    df <- filtered_tree_data()
    req(nrow(df) > 0)
    sort(unique(df$Species[!is.na(df$Species)]))
  })
  
  
  
  tree_points_sf <- reactive({
    df <- filtered_tree_data()
    req(nrow(df) > 0)
    df
  })
  
  # == 11. STATISTIQUES --------------------------------------------------------
  output$stat_plot_placeholder <- renderPlot({
    df <- filtered_tree_data() %>% st_drop_geometry()
    req(nrow(df) > 0)
    
    df <- df %>% filter(!is.na(Raw_Taxonomic_Identifier))
    
    # Calcul de la richesse spécifique
    richesse <- n_distinct(df$Raw_Taxonomic_Identifier)
    
    # Top 20 espèces
    top_species <- df %>%
      count(Genus, Raw_Taxonomic_Identifier, sort = TRUE) %>%
      head(20)
    
    # Palette naturelle
    natural_palette <- c(
      "#006400", "#228B22", "#66C2A5", "#A1D99B",  # verts
      "#8B4513", "#A0522D", "#D2B48C", "#DEB887"   # marrons
    )
    
    genres <- unique(top_species$Genus)
    genre_colors <- setNames(rep(natural_palette, length.out = length(genres)), genres)
    bar_colors <- genre_colors[top_species$Genus]
    
    par(mar = c(10, 1, 5, 6))
    
    # Hauteurs de barres
    bar_heights <- top_species$n
    y_max <- max(bar_heights) * 1.1
    
    bar_positions <- barplot(
      height = bar_heights,
      names.arg = top_species$Raw_Taxonomic_Identifier,
      col = bar_colors,
      border = NA,
      main = "les 20 espèces les plus abondantes",
      las = 2,
      cex.names = 0.8,
      ylim = c(0, y_max),
      axes = FALSE
    )
    
    # Ajouter valeurs au-dessus des barres
    text(
      x = bar_positions,
      y = bar_heights,
      labels = bar_heights,
      pos = 3,
      cex = 0.8,
      col = "black"
    )
    
    # Affichage de la richesse spécifique
    mtext(
      text = paste("Richesse spécifique :", richesse),
      side = 3, line = 3, font = 2, cex = 1.5
    )
    
    legend("topright", inset = c(-0.07, 0), xpd = TRUE,
           legend = names(genre_colors), fill = genre_colors,
           border = NA, cex = 0.8, title = "Genres")
    
  })
  
  #===================================================================
  output$stat_plot_placeholder_2 <- renderPlot({
    df <- filtered_tree_data() %>% st_drop_geometry()
    req(nrow(df) > 0)
    
    df <- df %>%
      filter(!is.na(Raw_Taxonomic_Identifier), !is.na(DBH)) %>%
      mutate(DBH = as.numeric(DBH)) %>%
      filter(DBH > 0)
    
    df <- df %>%
      mutate(
        surface_terr = (pi * (DBH^2)) / 2^2,  # surface en cm² vers m²
        genre = Genus
      )
    
    dominance_df <- df %>%
      group_by(Raw_Taxonomic_Identifier) %>%
      summarise(
        G_esp = sum(surface_terr, na.rm = TRUE),
        Genus = first(Genus),
        .groups = "drop"
      ) %>%
      mutate(
        G_total = sum(G_esp),
        dominance_relative = G_esp / G_total
      ) %>%
      arrange(desc(dominance_relative)) %>%
      slice_head(n = 20)
    
    natural_palette <- c("#006400", "#228B22", "#66C2A5", "#A1D99B", "#8B4513", "#A0522D", "#D2B48C", "#DEB887")
    
    genres <- unique(dominance_df$Genus)
    genre_colors <- setNames(rep(natural_palette, length.out = length(genres)), genres)
    bar_colors <- genre_colors[dominance_df$Genus]
    
    par(mar = c(10, 1, 5, 6))
    bar_heights <- dominance_df$dominance_relative * 100
    y_max <- max(bar_heights) * 1.1
    species_labels <- dominance_df$Raw_Taxonomic_Identifier
    
    bar_positions <- barplot(
      height = bar_heights,
      names.arg = species_labels,
      col = bar_colors,
      border = NA,
      main = "surface terrière relative des espèces (en fonction de la sélection)",
      las = 2,
      cex.names = 0.8,
      ylim = c(0, y_max),
      axes = FALSE
    )
    
    mtext(
      text = "Importance relative des espèces",
      side = 3, line = 3, font = 2, cex = 1.5
    )
    
    text(
      x = bar_positions,
      y = bar_heights,
      labels = paste0(round(bar_heights, 1), "%"),
      pos = 3,
      cex = 0.8,
      col = "black"
    )
    
    legend("topright", inset = c(-0.07, 0), xpd = TRUE,
           legend = names(genre_colors), fill = genre_colors,
           border = NA, cex = 0.8, title = "Genres")
  })
  
  
  
  # == 12. EXPORT CSV --------------------------------------------------------
  output$download_tables_csv <- downloadHandler(
    filename = function() {
      paste0("exported_tables_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(input$selected_table_name, input$plot_select)
      conn_temp <- get_db_conn()
      on.exit(dbDisconnect(conn_temp), add = TRUE)
      
      tables_list <- lapply(input$selected_table_name, function(tbl) {
        df <- if (tbl == "permanent_plot") {
          plots_str <- paste(shQuote(input$plot_select), collapse = ", ")
          dbGetQuery(conn_temp, glue("
          SELECT *
          FROM permanent_plot
          WHERE plot_ID IN ({plots_str})
        "))
        } else if (tbl == "tree_individuals") {
          df <- filtered_tree_data() %>% st_drop_geometry()
        } else {
          dbReadTable(conn_temp, tbl)
        }
        
        clean_export_table(df, tbl)
      })
      
      df_all <- bind_rows(tables_list)
      write.csv(df_all, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
}