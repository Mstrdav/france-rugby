library(shiny)
library(bslib)
library(tidyverse)
library(plotly)
library(DT)
library(FactoMineR)
library(factoextra)
library(zoo)

# --- 1. GLOBAL SETUP & DATA PROCESSING ---
# Chargement des données
df <- read_csv("data/rugby.csv", show_col_types = FALSE) %>%
  mutate(
    Joueur = as.factor(Joueur),
    Poste = as.factor(Poste),
    `Poste groupe` = as.factor(`Poste groupe`),
    Type = as.factor(Type),
    Date = as.Date(Date)
  )

# Correction des outliers de Fréquence Cardiaque (<100) et calcul du Wellness
df <- df %>%
  group_by(Joueur, Type) %>%
  mutate(
    `Fréquence cardiaque moyenne (bpm)` = ifelse(
      `Fréquence cardiaque moyenne (bpm)` < 100,
      median(`Fréquence cardiaque moyenne (bpm)`[`Fréquence cardiaque moyenne (bpm)` >= 100], na.rm = TRUE),
      `Fréquence cardiaque moyenne (bpm)`
    )
  ) %>%
  ungroup() %>%
  group_by(Joueur) %>%
  mutate(
    Wellness = (Emotion + `Bas du corps` + `Haut du corps` + Sommeil) / 4
  ) %>%
  ungroup()

# Calcul du ratio ACWR et métriques avancées (Wellness post-séance & m/min)
df <- df %>%
  arrange(Joueur, Date) %>%
  group_by(Joueur) %>%
  mutate(
    Charge_aigue_dist = rollapply(`Distance totale (m)`, width = 7, FUN = sum, 
                                  na.rm = TRUE, fill = NA, align = "right", partial = FALSE),
    Charge_chronique_dist = rollapply(`Distance totale (m)`, width = 28, FUN = sum, 
                                      na.rm = TRUE, fill = NA, align = "right", partial = FALSE),
    ACWR_dist = ifelse(Charge_chronique_dist > 0,
                       (Charge_aigue_dist / 7) / (Charge_chronique_dist / 28),
                       NA),
    # Fatigue J+1 et J+2
    W1 = lead(Wellness, 1),
    W2 = lead(Wellness, 2),
    Wellness_post_seance = rowMeans(cbind(W1, W2), na.rm = TRUE),
    Wellness_post_seance = ifelse(is.nan(Wellness_post_seance), NA, Wellness_post_seance)
  ) %>%
  ungroup() %>%
  mutate(
    # Normalisation par la durée pour le rapport post-séance
    Dist_m_min = ifelse(`Durée (min)` > 0, `Distance totale (m)` / `Durée (min)`, NA),
    Dist_HI_m_min = ifelse(`Durée (min)` > 0, `Distance haute intensité (m)` / `Durée (min)`, NA)
  ) %>%
  select(-W1, -W2)

# Thème bslib pour une esthétique moderne
custom_theme <- bs_theme(
  bootswatch = "yeti", 
  primary = "#1A5276",
  heading_font = font_google("Inter"),
  base_font = font_google("Inter")
)

# --- 2. UI DEFINITION ---
ui <- page_navbar(
  title = "🏉 France Rugby - Performance Lab",
  theme = custom_theme,
  fillable = FALSE,
  sidebar = sidebar(
    width = 300,
    h4("Filtres Globaux"),
    selectInput("joueur_sel", "Sélectionner un joueur :", 
                choices = c("Tous", sort(unique(as.character(df$Joueur)))), selected = "Tous"),
    hr(),
    p("Bienvenue sur France Rugby Data Performance Hub Pro New Edition, plateforme couteau suisse à destination des équipes de Rugby. Chacun peut y trouver les outils nécessaires pour leverage les assets afin d'appliquer un méthodologie d'entraînement assertive.", style = "font-size: 0.9em; color: #666;")
  ),
  
  # ------ ONGLET 1 : EDA ------
  nav_panel("EDA & Profil",
            layout_columns(
              uiOutput("ui_kpi_dist"),
              uiOutput("ui_kpi_contacts"),
              uiOutput("ui_kpi_well")
            ),
            layout_columns(
              col_widths = c(7, 5),
              # Colonne de gauche (Graphe ACWR + Profil Idéal)
              div(
                card(
                  card_header(
                    class = "bg-primary text-white",
                    "Suivi Longitudinal & Charge ACWR (Distance Totale)"
                  ),
                  card_body(plotlyOutput("plot_longitudinal", height = "400px"))
                ),
                card(
                  card_header(
                    class = "bg-success text-white",
                    "Profil d'Entraînement Idéal (Pré-Match)"
                  ),
                  card_body(
                    p("Moyennes d'entraînement quotidiennes (J-5 à J-1) observées lors des semaines précédant les matchs où le joueur a enregistré ses meilleurs scores de Wellness (top 50%).", style="font-size:0.85em;"),
                    DTOutput("table_profil_ideal")
                  )
                )
              ),
              # Colonne de droite (ACP Variables + Individus)
              card(
                card_header(
                  class = "bg-primary text-white",
                  "Analyse en Composantes Principales (ACP)"
                ),
                card_body(
                  p("L'ACP permet d'identifier les inter-relations entre métriques de charge interne et externe.", style="font-size:0.85em;"),
                  plotOutput("plot_pca", height = "300px"),
                  hr(),
                  p("Projection des profils de séances d'entraînement", style="font-size:0.85em; font-weight:bold;"),
                  plotOutput("plot_pca_ind", height = "300px")
                )
              )
            ),
            card(
              card_header(
                class = "bg-primary text-white",
                "Analyse Quotidienne : Distance, Wellness & Type d'entraînement"
              ),
              card_body(plotlyOutput("plot_quotidien", height = "450px"))
            )
  ),
  
  # ------ ONGLET 2 : HYPOTHÈSES ------
  nav_panel("Hypothèses",
            layout_columns(
              card(
                card_header("H1 : Profil de poste physique (Contacts vs Distance HI)"),
                card_body(
                  p("Les Avants cumulent plus de contacts physiques, tandis que les Arrières parcourent davantage de distance à Haute Intensité, impactant différemment les récupérations (haut vs bas du corps).", class = "text-muted", style="font-size:0.85em;"),
                  plotlyOutput("plot_h1")
                )
              ),
              card(
                card_header("H2 : Fatigue selon le type de séance"),
                card_body(
                  p("Les séances de type Match ou Rugby Collectif créent la fatigue musculo-squelettique la plus importante, se reflétant sur la moyenne du score Wellness des 2 jours suivants (échelle 1 à 5).", class = "text-muted", style="font-size:0.85em;"),
                  plotlyOutput("plot_h2")
                )
              )
            ),
            layout_columns(
              card(
                card_header("H3 : Vitesse & Sommeil"),
                card_body(
                  p("Les séances de sprint/vitesse peuvent générer de la fatigue nerveuse et perturber le sommeil par rapport aux entraînements séparés ou moins intenses.", class = "text-muted", style="font-size:0.85em;"),
                  plotlyOutput("plot_h3")
                )
              ),
              card(
                card_header("H4 : Impact de la 'Readiness' sur la Performance"),
                card_body(
                  p("Lorsqu'un joueur démarre sa séance avec un état de Wellness inférieur à son 1er Quartile habituel ('Fatigué'), la production de distance à Haute Intensité tend à baisser (autorégulation).", class = "text-muted", style="font-size:0.85em;"),
                  plotlyOutput("plot_h4")
                )
              )
            )
  ),
  
  # ------ ONGLET 3 : OUTILS ------
  nav_panel("Outils & Décision",
            navset_card_tab(
              id = "tab_outils",
              full_screen = TRUE,
              nav_panel("📈 Readiness du Jour",
                        p(strong(paste("Date de la simulation :", max(df$Date, na.rm = TRUE)))),
                        p("Code couleur basé sur le ratio individuel du joueur (Vert = Au-dessus de sa médiane, Orange = Sous la médiane, Rouge = Sous son 1er Quartile)."),
                        DTOutput("table_readiness")
              ),
              nav_panel("🚨 Alertes ACWR",
                        p("Identifier les joueurs en zone de danger (>1.5) ou de sous-entraînement (<0.8) selon l'ACWR Distance de la dernière semaine enregistrée."),
                        DTOutput("table_alertes")
              ),
              nav_panel("📊 Rapport Post-Séance (Intensité m/min)",
                        p("Mesurer l'intensité de la dernière séance par rapport à la moyenne individuelle du joueur pour ce TYPE de séance exact. La distance est normalisée par la durée du joueur (m/min)."),
                        DTOutput("table_post_seance")
              )
            )
  )
)

# --- 3. SERVER DEFINITION ---
server <- function(input, output, session) {
  
  # Filtrage reactif
  df_filtered <- reactive({
    if(input$joueur_sel == "Tous") {
      df
    } else {
      df %>% filter(Joueur == input$joueur_sel)
    }
  })
  
  # ---- ONGLET 1 : EDA ----
  create_kpi_box <- function(title, val_7d, val_rest, icon_name, reverse_color = FALSE) {
    if (is.na(val_rest) || val_rest == 0) {
      pct <- 0
    } else {
      pct <- ((val_7d - val_rest) / val_rest) * 100
    }
    
    delta <- val_7d - val_rest
    is_positive <- delta > 0
    
    color_class <- if (is_positive) {
      if (reverse_color) "text-danger" else "text-success"
    } else if (delta < 0) {
      if (reverse_color) "text-success" else "text-danger"
    } else {
      "text-muted"
    }
    
    icon_arrow <- if (is_positive) "arrow-up" else if (delta < 0) "arrow-down" else "dash"
    
    delta_ui <- p(
      class = paste("mb-0 ms-2", color_class),
      style = "font-size: 0.6em; align-self: baseline;",
      bsicons::bs_icon(icon_arrow),
      sprintf(" %.1f%%", abs(pct))
    )
    
    value_ui <- div(
      class = "d-flex align-items-baseline",
      strong(format(round(val_7d, 1), nsmall = 1), style="font-size: 1.1em;"),
      delta_ui
    )
    
    value_box(
      title = title,
      value = value_ui,
      showcase = bsicons::bs_icon(icon_name),
      theme = "light"
    )
  }
  
  kpi_data <- reactive({
    df_f <- df_filtered()
    if(nrow(df_f) == 0) return(NULL)
    max_date <- max(df_f$Date, na.rm = TRUE)
    df_7d <- df_f %>% filter(Date >= (max_date - 6))
    df_rest <- df_f %>% filter(Date < (max_date - 6))
    list(recent = df_7d, rest = df_rest)
  })
  
  output$ui_kpi_dist <- renderUI({
    res <- kpi_data()
    if(is.null(res)) return(value_box("Distance (7j)", "0", theme="light"))
    v_7d <- mean(res$recent$`Distance totale (m)`, na.rm = TRUE)
    v_r <- mean(res$rest$`Distance totale (m)`, na.rm = TRUE)
    if(is.nan(v_7d) || is.na(v_7d)) v_7d <- 0
    if(is.nan(v_r) || is.na(v_r)) v_r <- 0
    create_kpi_box("Dist. Moyenne (7j)", v_7d, v_r, "rulers", reverse_color = FALSE)
  })
  
  output$ui_kpi_contacts <- renderUI({
    res <- kpi_data()
    if(is.null(res)) return(value_box("Contacts (7j)", "0", theme="light"))
    v_7d <- mean(res$recent$`Nombre de contacts`, na.rm = TRUE)
    v_r <- mean(res$rest$`Nombre de contacts`, na.rm = TRUE)
    if(is.nan(v_7d) || is.na(v_7d)) v_7d <- 0
    if(is.nan(v_r) || is.na(v_r)) v_r <- 0
    create_kpi_box("Contacts Moyens (7j)", v_7d, v_r, "people-fill", reverse_color = TRUE)
  })
  
  output$ui_kpi_well <- renderUI({
    res <- kpi_data()
    if(is.null(res)) return(value_box("Wellness (7j)", "0", theme="light"))
    v_7d <- mean(res$recent$Wellness, na.rm = TRUE)
    v_r <- mean(res$rest$Wellness, na.rm = TRUE)
    if(is.nan(v_7d) || is.na(v_7d)) v_7d <- 0
    if(is.nan(v_r) || is.na(v_r)) v_r <- 0
    create_kpi_box("Score Wellness (7j)", v_7d, v_r, "heart-pulse", reverse_color = FALSE)
  })
  
  # --- NOUVEAU : Profil d'entraînement idéal ---
  output$table_profil_ideal <- renderDT({
    d <- df_filtered()
    
    # Identifier les dates de matchs et le wellness associé
    matches <- d %>% filter(Type == "Match") %>% 
      select(Joueur, Date_Match = Date, Wellness_Match = Wellness) %>% 
      drop_na()
    
    if(nrow(matches) == 0) {
      return(datatable(data.frame(Message="Pas de données de match trouvées."), options = list(dom='t'), rownames=FALSE))
    }
    
    # Récupérer les moyennes d'entraînement des 5 jours précédents
    pre_match_list <- lapply(1:nrow(matches), function(i) {
      m <- matches[i, ]
      sub_d <- d %>% 
        filter(Joueur == m$Joueur, 
               Date >= (m$Date_Match - 5), 
               Date < m$Date_Match)
      
      if(nrow(sub_d) == 0) return(NULL)
      
      sub_d %>% summarise(
        Dist_moy = mean(`Distance totale (m)`, na.rm=TRUE),
        Dist_HI_moy = mean(`Distance haute intensité (m)`, na.rm=TRUE),
        Contacts_moy = mean(`Nombre de contacts`, na.rm=TRUE),
        Wellness_Match = m$Wellness_Match
      )
    })
    
    pre_match_data <- bind_rows(pre_match_list)
    
    if(nrow(pre_match_data) == 0) {
      return(datatable(data.frame(Message="Pas de données d'entraînement précédant les matchs."), options = list(dom='t'), rownames=FALSE))
    }
    
    # Filtrer les meilleures préparations (Wellness au-dessus de la médiane)
    seuil <- median(pre_match_data$Wellness_Match, na.rm=TRUE)
    
    profil_ideal <- pre_match_data %>%
      filter(Wellness_Match >= seuil) %>%
      summarise(
        `Dist/jour ciblée (m)` = round(mean(Dist_moy, na.rm=TRUE), 0),
        `Dist HI/jour ciblée (m)` = round(mean(Dist_HI_moy, na.rm=TRUE), 0),
        `Contacts/jour ciblés` = round(mean(Contacts_moy, na.rm=TRUE), 1),
        `Wellness Match espéré` = round(mean(Wellness_Match, na.rm=TRUE), 2)
      )
    
    datatable(profil_ideal, options = list(dom = 't', scrollX = TRUE), rownames = FALSE) %>%
      formatStyle(columns = names(profil_ideal), textAlign = 'center', color = '#196F3D', fontWeight = 'bold')
  })
  
  output$plot_quotidien <- renderPlotly({
    d <- df_filtered() %>% filter(!is.na(Date))
    if(nrow(d) == 0) return(plotly_empty() %>% layout(title="Pas de données"))
    
    if(input$joueur_sel == "Tous") {
      d <- d %>%
        group_by(Date, Type) %>%
        summarise(
          `Distance totale (m)` = mean(`Distance totale (m)`, na.rm = TRUE),
          `Distance haute intensité (m)` = mean(`Distance haute intensité (m)`, na.rm = TRUE),
          Wellness = mean(Wellness, na.rm = TRUE),
          .groups = "drop"
        )
    }
    
    d <- d %>% arrange(Date) %>% mutate(
      Dist_Basse = pmax(0, `Distance totale (m)` - `Distance haute intensité (m)`)
    )
    y_baseline <- -max(d$`Distance totale (m)`, na.rm=T) * 0.05
    if (is.na(y_baseline) || y_baseline == 0) y_baseline <- -100
    
    plot_ly(d, x = ~Date) %>%
      add_bars(y = ~Dist_Basse, name = "Dist. Basse/Moy", marker = list(color = "#3498DB", line=list(color='#2980B9', width=1)), 
               hoverinfo = "text", text = ~paste0("Date: ", Date, "<br>Dist Basse: ", round(Dist_Basse,0), "m\n<br>Séance: ", Type)) %>%
      add_bars(y = ~`Distance haute intensité (m)`, name = "Dist. HI", marker = list(color = "#E74C3C", line=list(color='#C0392B', width=1)),
               hoverinfo = "text", text = ~paste0("Dist HI: ", round(`Distance haute intensité (m)`,0), "m")) %>%
      add_markers(y = y_baseline, color = ~Type, colors = "Dark2",
                  marker = list(symbol = "square", size = 12, line=list(color="white", width=1)),
                  name = ~Type, hoverinfo = "text", text = ~paste0("Séance: ", Type)) %>%
      add_lines(y = ~Wellness, name = "Score Wellness", yaxis = "y2",
                line = list(color = "#F1C40F", width = 3, shape = "spline"),
                marker = list(color = "#F39C12", size = 8, symbol = "circle"),
                hoverinfo = "text", text = ~paste0("Wellness: ", round(Wellness, 2))) %>%
      layout(
        barmode = 'stack',
        hovermode = "x unified",
        xaxis = list(title = "", type = "date"),
        yaxis = list(title = "Distance (m)", side = "left"),
        yaxis2 = list(title = "Wellness (échelle 1-5)", side = "right", overlaying = "y", range = c(1, 5), showgrid = FALSE),
        legend = list(orientation = "h", x = 0.5, y = -0.2, xanchor = "center"),
        margin = list(r = 50, b = 60)
      )
  })
  
  output$plot_longitudinal <- renderPlotly({
    data_plot <- df_filtered() %>% filter(!is.na(ACWR_dist))
    if(nrow(data_plot) == 0) return(plotly_empty() %>% layout(title = "Pas de données d'ACWR suffisantes"))
    
    p <- ggplot(data_plot, aes(x = Date, y = ACWR_dist, color = Joueur)) +
      geom_line(alpha = 0.8) +
      geom_hline(yintercept = 1.5, linetype = "dashed", color = "red", alpha = 0.7) +
      geom_hline(yintercept = 0.8, linetype = "dashed", color = "orange", alpha = 0.7) +
      annotate("rect", xmin = min(data_plot$Date), xmax = max(data_plot$Date),
               ymin = 0.8, ymax = 1.3, alpha = 0.1, fill = "green") +
      theme_minimal() +
      labs(y = "ACWR Distance", x = "") +
      theme(legend.position = if(input$joueur_sel == "Tous") "none" else "right")
    
    ggplotly(p)
  })
  
  output$plot_pca <- renderPlot({
    df_pca <- df %>% select(`Durée (min)`, `Distance totale (m)`, `Distance haute intensité (m)`, 
                            `Fréquence cardiaque moyenne (bpm)`, `Nombre de contacts`, Wellness) %>%
      drop_na()
    if(nrow(df_pca) > 5) {
      res.pca <- PCA(df_pca, scale.unit = TRUE, graph = FALSE)
      fviz_pca_var(res.pca, col.var = "contrib",
                   gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                   repel = TRUE, title = "Cercle des Corrélations (Variables)") +
        theme_minimal()
    }
  })
  
  output$plot_pca_ind <- renderPlot({
    df_pca_full <- df %>% select(Type, `Durée (min)`, `Distance totale (m)`, `Distance haute intensité (m)`, 
                                 `Fréquence cardiaque moyenne (bpm)`, `Nombre de contacts`, Wellness) %>%
      drop_na()
    df_pca <- df_pca_full %>% select(-Type)
    if(nrow(df_pca) > 5) {
      res.pca <- PCA(df_pca, scale.unit = TRUE, graph = FALSE)
      fviz_pca_ind(res.pca, geom = "point", habillage = df_pca_full$Type,
                   addEllipses = TRUE, ellipse.level = 0.95, title = "Profils de Séances") +
        theme_minimal()
    }
  })
  
  # ---- ONGLET 2 : HYPOTHÈSES ----
  output$plot_h1 <- renderPlotly({
    df_match <- df %>% filter(Type %in% c("Match", "Entrainement - Rugby Collectif")) %>% drop_na(`Nombre de contacts`, `Poste groupe`)
    if(nrow(df_match) == 0) return(plotly_empty())
    p <- ggplot(df_match, aes(x = `Poste groupe`, y = `Nombre de contacts`, fill = `Poste groupe`)) +
      geom_boxplot(alpha=0.8) +
      scale_fill_manual(values = c("Avant" = "#E74C3C", "Arrière" = "#3498DB")) +
      theme_minimal() + labs(x = "", y = "Nb Contacts (Matchs/Collectif)")
    ggplotly(p) %>% layout(showlegend = FALSE)
  })
  
  output$plot_h2 <- renderPlotly({
    df_plot2 <- df %>% drop_na(Wellness_post_seance, Type)
    p <- ggplot(df_plot2, aes(x = Type, y = Wellness_post_seance, fill = Type)) +
      geom_boxplot(alpha=0.8) +
      theme_minimal() + 
      theme(axis.text.x = element_blank()) +
      labs(x = "Type de Séance", y = "Score Wellness (Moy. J+1 & J+2)")
    ggplotly(p)
  })
  
  output$plot_h3 <- renderPlotly({
    df_vit <- df %>% filter(Type %in% c("Entrainement - Vitesse", "Entrainement - Séparé")) %>% drop_na(Sommeil, Type)
    if(nrow(df_vit)==0) return(plotly_empty())
    p <- ggplot(df_vit, aes(x = Type, y = Sommeil, fill = Type)) +
      geom_boxplot(alpha=0.8) +
      scale_fill_manual(values = c("Entrainement - Vitesse" = "#F39C12", "Entrainement - Séparé" = "#27AE60")) +
      theme_minimal() + labs(x = "", y = "Score Sommeil J+1 (1 = Mauvais)")
    ggplotly(p) %>% layout(showlegend = FALSE)
  })
  
  output$plot_h4 <- renderPlotly({
    df_read <- df %>%
      group_by(Joueur) %>%
      mutate(Q1 = quantile(Wellness, 0.25, na.rm = TRUE),
             Etat_Readiness = ifelse(Wellness < Q1, "Fatigué (< Q1)", "Normal")) %>%
      ungroup() %>% drop_na(`Distance haute intensité (m)`, Etat_Readiness)
    
    p <- ggplot(df_read, aes(x = Etat_Readiness, y = `Distance haute intensité (m)`, fill = Etat_Readiness)) +
      geom_boxplot(alpha=0.8) +
      scale_fill_manual(values = c("Fatigué (< Q1)" = "#E74C3C", "Normal" = "#2ECC71")) +
      theme_minimal() + labs(x = "État de fraîcheur initial", y = "Distance HI Produite (m)")
    ggplotly(p) %>% layout(showlegend = FALSE)
  })
  
  # ---- ONGLET 3 : OUTILS ----
  output$table_readiness <- renderDT({
    derniere_date <- max(df$Date, na.rm=TRUE)
    df_dash <- df %>% filter(Date == derniere_date) %>%
      group_by(Joueur) %>% slice_tail(n=1) %>% ungroup() %>%
      left_join(
        df %>% group_by(Joueur) %>% summarise(Q1 = quantile(Wellness, 0.25, na.rm=TRUE), Med = median(Wellness, na.rm=TRUE)),
        by = "Joueur"
      ) %>%
      mutate(
        Statut = case_when(Wellness <= Q1 ~ "ROUGE", Wellness <= Med ~ "ORANGE", TRUE ~ "VERT"),
        Recommandation = case_when(Wellness <= Q1 ~ "Repos / Entraînement léger", Wellness <= Med ~ "Adapter la charge", TRUE ~ "Disponible / Charge normale")
      ) %>%
      select(Joueur, Poste, Sommeil, Emotion, `Bas du corps`, `Haut du corps`, Wellness, Statut, Recommandation) %>%
      arrange(Wellness) %>% mutate(across(where(is.numeric), ~round(., 1)))
    
    datatable(df_dash, rownames = FALSE, options = list(pageLength = 15, dom = 't', scrollX = TRUE)) %>%
      formatStyle('Statut', backgroundColor = styleEqual(c('ROUGE', 'ORANGE', 'VERT'), c('#F2D7D5', '#FAE5D3', '#D5F5E3')), fontWeight = 'bold') %>%
      formatRound(c('Wellness', 'Sommeil', 'Emotion', 'Bas du corps', 'Haut du corps'), 1)
  })
  
  output$table_alertes <- renderDT({
    alertes <- df %>% filter(!is.na(ACWR_dist)) %>%
      group_by(Joueur) %>% slice_tail(n=1) %>% ungroup() %>%
      mutate(Zone = case_when(ACWR_dist > 1.5 ~ "DANGER (> 1.5)", ACWR_dist > 1.3 ~ "ATTENTION (1.3 - 1.5)", ACWR_dist >= 0.8 ~ "OPTIMAL", TRUE ~ "SOUS-CHARGE (< 0.8)")) %>%
      select(Joueur, `Poste groupe`, Date, ACWR_dist, Zone) %>% arrange(desc(ACWR_dist)) %>%
      mutate(ACWR_dist = round(ACWR_dist, 2))
    
    datatable(alertes, rownames = FALSE, options = list(pageLength = 10, dom = 't', scrollX = TRUE)) %>%
      formatStyle('Zone', backgroundColor = styleEqual(c('DANGER (> 1.5)', 'ATTENTION (1.3 - 1.5)', 'OPTIMAL', 'SOUS-CHARGE (< 0.8)'), c('#F2D7D5', '#FAE5D3', '#D5F5E3', '#EAECEE')), fontWeight = 'bold')
  })
  
  output$table_post_seance <- renderDT({
    moyennes <- df %>% group_by(Joueur, Type) %>%
      summarise(Dist_m_min_moy = mean(Dist_m_min, na.rm=TRUE),
                Dist_HI_m_min_moy = mean(Dist_HI_m_min, na.rm=TRUE),
                .groups = 'drop')
    
    derniere <- df %>% group_by(Joueur) %>% filter(Date == max(Date, na.rm=TRUE)) %>% slice_tail(n=1) %>% ungroup()
    
    rapport <- derniere %>%
      left_join(moyennes, by = c("Joueur", "Type")) %>%
      mutate(Ecart_Dist_Pct = round((Dist_m_min - Dist_m_min_moy) / Dist_m_min_moy * 100, 1),
             Synthèse = case_when(Ecart_Dist_Pct > 10 ~ "📈 Sur-performance relative", 
                                  Ecart_Dist_Pct < -10 ~ "📉 Sous-performance relative", 
                                  TRUE ~ "➡️ Dans la norme")) %>%
      select(Joueur, Type, `Durée (min)`, Dist_m_min, Ecart_Dist_Pct, Synthèse) %>%
      mutate(Dist_m_min = round(Dist_m_min, 1))
    
    datatable(rapport, rownames = FALSE, 
              colnames = c("Joueur", "Type de séance", "Durée (min)", "Dist (m/min)", "Écart Dist (%)", "Synthèse Globale"),
              options = list(pageLength = 10, dom = 't', scrollX = TRUE)) %>%
      formatStyle('Ecart_Dist_Pct', color = styleInterval(c(-10, 10), c('red', 'black', 'darkgreen')))
  })
  
}

shinyApp(ui, server)