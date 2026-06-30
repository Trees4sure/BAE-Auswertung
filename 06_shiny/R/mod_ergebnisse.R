#################################################
#                                               #
# R/mod_ergebnisse.R                           #
# Tab 2: Ergebnisse                             #
#                                              #
# BZT-Ergebnistabellen,                        #
# Baumartenanteile-Tabellen,                   #
# BZT-Grafiken (Min/Max, Prozent, Mischung),   #
# KA5-Grafiken (WASSER/BODTYP/BODART)          #
#                                              #
# @reads-shared: B5, B6, data_water,           #
#   data_bodtyp, data_bodart                   #
# @reads-rv: (keine)                           #
# @writes-rv: (keine)                          #
# @returns: (nichts)                           #
# @toggle: Baumartenanteile_Tabelle,           #
#   BZT_Grafiken, KA5_Grafiken,               #
#   toggle_WASSER, toggle_BODTYP,             #
#   toggle_BODART                              #
# @leaflet-group: (keine)                      #
# @depends-on: standort_results (mod_standort) #
#                                              #
#################################################
#
# ---- OBSERVER-INVENTAR ----
# | Nr | Typ            | Trigger                      | Aktion                                    | ignoreInit |
# |----|----------------|------------------------------|-------------------------------------------|------------|
# | O1 | observeEvent   | wasserstandort               | Aktualisiert wasserinput Choices           | FALSE      |
# | O2 | observeEvent   | bodtypstandort               | Aktualisiert bodtypinput Choices           | FALSE      |
# | O3 | observeEvent   | bodartstandort               | Aktualisiert bodartinput Choices           | FALSE      |
# | O4 | observeEvent   | filter4 (aus mod_standort)   | Rendert BZT-Ergebnis-Text + Tabelle       | FALSE      |
# | O5 | observeEvent   | Baumartenanteile_Tabelle     | Rendert B6-Tabellen mit Highlight          | TRUE       |
# | O6 | observeEvent   | BZT_Grafiken                 | Rendert 9 Plots (Min/Max, Prc, Mischung)  | TRUE       |
# | O7 | observeEvent   | wasserinput                  | Rendert 3 WASSER-KA5-Plots                | FALSE      |
# | O8 | observeEvent   | bodtypinput                  | Rendert 3 BODTYP-KA5-Plots                | FALSE      |
# | O9 | observeEvent   | bodartinput                  | Rendert 3 BODART-KA5-Plots                | FALSE      |

# ---- UI ----

mod_ergebnisse_ui <- function(id) {
  ns <- NS(id)
  
  f7Tab(
    title   = "Ergebnisse",
    tabName = "Results",
    icon    = f7Icon("doc_chart"),
    active  = TRUE,
    hidden  = FALSE,
    
    # ---- Gefilterte BZT-Tabelle ----
    f7Card(
      title = "Gefilterte Bestockungszieltypen nach gewaehlter STGR und Klimastufe",
      
      tags$div(
        style = paste(
          "background:#e8f4e8; border-left:4px solid #2d6a4f;",
          "border-radius:4px; padding:8px 12px; margin-bottom:8px;",
          "font-size:12px; color:#1a3d2b;"
        ),
        tags$strong("\u26a0 Wichtige Hinweise:"),
        tags$br(),
        "- Die Zuweisung erfolgt auf Basis der STAO-Karte.",
        tags$br(),
        "- Bitte beachten Sie die kleinstand\u00f6rtlichen Gegebenheiten vor Ort!",
        tags$br(),
        "- Die Aktualit\u00e4t der Standortskartierung muss beachtet werden.",
        tags$br(),
        "- Humus-sensitive Standorte und gekennzeichnete Moore beachten!"
      ),
      br(),
      uiOutput(ns("filtered_output"))
    ),
    
    f7Card(
      title = "Baumarten innerhalb der BZT-Auswahl",
      DTOutput(ns("baumart_bzt_nr"))
    ),
    
    # ---- Baumartenanteile-Tabelle (Toggle) ----
    f7Toggle(inputId = ns("Baumartenanteile_Tabelle"),
             label   = "Baumartenanteile anzeigen",
             checked = FALSE),
    
    conditionalPanel(
      condition = sprintf("input['%s'] == true", ns("Baumartenanteile_Tabelle")),
      f7Card(
        title = "Uebersicht der Baumartenanteile nach gewaelter STGR und Klimastufe und Baumartengruppen",
        f7Card(title = "Baumart 1", tableOutput(ns("filtered_data1"))),
        f7Card(title = "Baumart 2", tableOutput(ns("filtered_data2"))),
        f7Card(title = "Baumart 3", tableOutput(ns("filtered_data3")))
      )
    ),
    
    # ---- BZT-Grafiken (Toggle) ----
    f7Toggle(inputId = ns("BZT_Grafiken"),
             label   = "Grafiken anzeigen zu Baumarten",
             checked = FALSE),
    
    conditionalPanel(
      condition = sprintf("input['%s'] == true", ns("BZT_Grafiken")),
      
      # f7Grid(
      #   cols = 3,
      #   f7Card("Baumartenanteile 1", plotOutput(ns("Baumartenanteile_1"))),
      #   f7Card("Baumartenanteile 2", plotOutput(ns("Baumartenanteile_2"))),
      #   f7Card("Baumartenanteile 3", plotOutput(ns("Baumartenanteile_3")))
      # ),
      
      # 2026-04-22 Anpassung alle Baumarten ->
      
      f7Card(
        title = "Mindest- und Maximal-Baumartenanteile (alle Baumarten)",
        
        tags$div(
          class = "hinweis-gruen",
          f7Accordion(
            f7AccordionItem(
              title = tagList(
                f7Icon("info_circle", color = "black", size = "20px"),
                span("Mehr Info", style = "margin-left:6px")
              ),
              "Werte aus dem BZT-Regelwerk (Tabelle B5). Kein Felddatenbezug. ",
              "Die Balken zeigen den zulaessigen Mindest- und Maximalanteil je Baumart ",
              "fuer die gewaehlte Standortsgruppe und Klimastufe."
            )
          )
        ),
        
        tags$div(
          class = "hinweis-gruen",
          "Filter wird automatisch aus der Standortauswahl (Tab 1) uebernommen.
     Manuelle Anpassung moeglich."
        ),
        tags$br(),
        
        fluidRow(
          column(6,
                 selectInput(
                   inputId  = ns("plot_stgr_filter"),
                   label    = "Standortsgruppe (STGR)",
                   choices  = c("Bitte waehlen" = ""),
                   selected = "",
                   width    = "100%"
                 )
          ),
          column(6,
                 selectInput(
                   inputId  = ns("plot_klima_filter"),
                   label    = "Klimastufe",
                   choices  = c("Bitte waehlen" = ""),
                   selected = "",
                   width    = "100%"
                 )
          )
        ),
        
        plotlyOutput(ns("Baumartenanteile_alle"))
      )
      
      
      # AUSKOMMENTIERT 2026-04-22 - irreführend, schwer erklärbar
      # Reaktivierung nur wenn Nutzer explizit Bedarf melden
      #
      # h3("Prozentwert zeigt die Wahrscheinlichkeit der Art am Standort auf"),
      # f7Grid(
      #   cols = 3,
      #   f7Card("Baumartenprozente 1", plotlyOutput(ns("Baumartenanteile_prc_1"))),
      #   f7Card("Baumartenprozente 2", plotlyOutput(ns("Baumartenanteile_prc_2"))),
      #   f7Card("Baumartenprozente 3", plotlyOutput(ns("Baumartenanteile_prc_3")))
      # ),
      #
      # h3("Prozentwert zeigt die Mischbarkeit der Baumart mit anderen Arten"),
      # f7Grid(
      #   cols = 3,
      #   f7Card("Baumartenmischungsprozente 1", plotlyOutput(ns("Baumartenmischung_prc_1"))),
      #   f7Card("Baumartenmischungsprozente 2", plotlyOutput(ns("Baumartenmischung_prc_2"))),
      #   f7Card("Baumartenmischungsprozente 3", plotlyOutput(ns("Baumartenmischung_prc_3")))
      # )
    ),
    
    # ---- KA5-Grafiken (Toggle) ----
    f7Toggle(inputId = ns("KA5_Grafiken"),
             label   = "Grafiken zu KA5-Standorten",
             checked = FALSE),
    
    conditionalPanel(
      condition = sprintf("input['%s'] == true", ns("KA5_Grafiken")),
      
      f7Card(
        title = "Vergleich Baumartenempfehlung mit KA5",
        tags$div(
          class = "hinweis-gruen",
          f7Accordion(
            f7AccordionItem(
              title = tagList(
                f7Icon("info_circle", color = "black", size = "20px"),
                span("Mehr Info", style = "margin-left:6px")
              ),
              "MultiRiskSuit - Projektdaten: An BWI-Punkten in MV wurde nach einer
              Standorts-Synopse eine BZT-Zuweisung durchgefuehrt.",
              tags$br(), tags$br(),
              "Die Grafiken zeigen die Wahrscheinlichkeit, mit der der BZT-Erlass an
              diesen Standorten Baumarten vorschlagen wuerde."
            )
          )
        ),
        br(), br(),
        
        # WASSER-Toggle
        f7Toggle(ns("toggle_WASSER"), "Wasserstufen", checked = FALSE),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == true", ns("toggle_WASSER")),
          h2(class = "wasser", "Wasserstufe nach KA5"),
          tags$div(
            class = "hinweis-gruen",
            f7Accordion(
              f7AccordionItem(
                title = tagList(f7Icon("info_circle", color = "black", size = "20px"),
                                span("Hinweise zu Wasserstufen", style = "margin-left:6px")),
                "G0 - schwach bis G6 - sehr stark von Grundwasser beeinflusst.",
                br(), "S1 - schwach bis S6 - sehr stark von Stauwasser beeinflusst",
                br(), "T1 - trocken bis T4 - frisch terrestrischer Standort",
                br(), br(),
                "Berechnet ist das Verhaeltnis aller Maximalanteile empfohlener Baumarten auf den gewaehlten Wasserstufen."
              )
            )
          ),
          f7Select_Alt(ns("wasserstandort"),    "Standort eingeben:",  choices = c(" ", unique(data.water$Standort)), selected = "Grundwasser", width = "50%"),
          f7Select_Alt(ns("wasserinput"),        "Wasserstufe eingeben:", choices = NULL, width = "50%"),
          f7Select_Alt(ns("wasserinputklima"),   "Klimastufe eingeben:", choices = unique(data.water$Klimastufe), width = "50%")
        ),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == true", ns("toggle_WASSER")),
          h3("Baumartenanteile des Standorts nach den Wassereinteilungen der KA5"),
          f7Grid(
            cols = 3,
            f7Card("WASSER Baumart 1", plotlyOutput(ns("WASSER_BA_1"))),
            f7Card("WASSER Baumart 2", plotlyOutput(ns("WASSER_BA_2"))),
            f7Card("WASSER Baumart 3", plotlyOutput(ns("WASSER_BA_3")))
          )
        ),
        
        # BODTYP-Toggle
        f7Toggle(ns("toggle_BODTYP"), "Bodentypen", checked = FALSE),
        conditionalPanel(
          condition = sprintf("input['%s'] == true", ns("toggle_BODTYP")),
          h2(class = "bodtyp", "BODTYP nach KA5"),
          h4("Bodentypen-Kuerzel nach der Standortskundlichen Kartieranleitung"),
          f7Card("Berechnet ist das Verhaeltnis aller Maximalanteile empfohlener Baumarten."),
          f7Select_Alt(ns("bodtypstandort"),  "Standort eingeben:", choices = c(" ", unique(data.bodtyp$Standort)), selected = "Braunerde", width = "50%"),
          f7Select_Alt(ns("bodtypinput"),     "BODTYP eingeben:",   choices = NULL, width = "50%"),
          f7Select_Alt(ns("bodtypinputklima"), "Klimastufe eingeben:", choices = unique(data.bodtyp$Klimastufe), width = "50%")
        ),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == true", ns("toggle_BODTYP")),
          h3("Baumartenanteile nach Bodentypen der KA5"),
          f7Grid(
            cols = 3,
            f7Card("BODTYP Baumart 1", plotlyOutput(ns("BODTYP_BA_1"))),
            f7Card("BODTYP Baumart 2", plotlyOutput(ns("BODTYP_BA_2"))),
            f7Card("BODTYP Baumart 3", plotlyOutput(ns("BODTYP_BA_3")))
          )
        ),
        
        # BODART-Toggle
        f7Toggle(ns("toggle_BODART"), "Bodenarten", checked = FALSE),
        conditionalPanel(
          condition = sprintf("input['%s'] == true", ns("toggle_BODART")),
          h2(class = "bodart", "BODART nach KA5"),
          h4("Bodenarten nach der Standortskundlichen Kartieranleitung"),
          f7Card("Berechnet ist das Verhaeltnis aller Maximalanteile empfohlener Baumarten."),
          f7Select_Alt(ns("bodartstandort"),  "Standort eingeben:", choices = c(" ", unique(data.bodart$Standort)), selected = "Sande", width = "50%"),
          f7Select_Alt(ns("bodartinput"),     "BODART eingeben:",   choices = NULL, width = "50%"),
          f7Select_Alt(ns("bodartinputklima"), "Klimastufe eingeben:", choices = unique(data.bodart$Klimastufe), width = "50%")
        ),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == true", ns("toggle_BODART")),
          h3("Baumartenanteile nach Bodenarten der KA5"),
          f7Grid(
            cols = 3,
            f7Card("BODART Baumart 1", plotlyOutput(ns("BODART_BA_1"))),
            f7Card("BODART Baumart 2", plotlyOutput(ns("BODART_BA_2"))),
            f7Card("BODART Baumart 3", plotlyOutput(ns("BODART_BA_3")))
          )
        )
      )
    )
  )
}


# ---- Server ----

mod_ergebnisse_server <- function(id, shared_data, standort_results) {
  moduleServer(id, function(input, output, session) {
    
    # ---- VERTRAG ----
    # LIEST shared_data: B5, B6, data_water, data_bodtyp, data_bodart
    # LIEST standort_results: filtered_data, filter1..filter5 (aus mod_standort)
    # LIEST rv:          (keine)
    # SCHREIBT rv:       (keine)
    # OUTPUTS:           filtered_output, baumart_bzt_nr, filtered_data1/2/3,
    #                    Baumartenanteile_1/2/3, Baumartenanteile_prc_1/2/3,
    #                    Baumartenmischung_prc_1/2/3,
    #                    WASSER_BA_1/2/3, BODTYP_BA_1/2/3, BODART_BA_1/2/3
    # GIBT ZURUECK:      nichts (NULL)
    
    # Lokale Referenzen auf Datentabellen
    B5_local         <- shared_data$B5
    B6_local         <- shared_data$B6
    
    # ---- Filter-Choices befuellen (einmalig beim Start) ----
    observe({
      req(B5_local)
      updateSelectInput(session, "plot_stgr_filter",
                        choices  = c("Bitte waehlen" = "",
                                     sort(unique(B5_local$STGR_Kart))))
      updateSelectInput(session, "plot_klima_filter",
                        choices  = c("Bitte waehlen" = "",
                                     sort(unique(B5_local$Klimastufe))))
    })
    
   
    data_water_local <- shared_data$data_water
    data_bodtyp_local <- shared_data$data_bodtyp
    data_bodart_local <- shared_data$data_bodart
    
    # Gefilterte Daten aus Tab 1 (reaktiv)
    filtered_data         <- standort_results$filtered_data
    filter1_Klimastufe    <- standort_results$filter1_Klimastufe
    filter2_Standort      <- standort_results$filter2_Standort
    filter4_Standortsgruppe <- standort_results$filter4_Standortsgruppe
    filter5_Baumart       <- standort_results$filter5_Baumart
    
        # ---- Filter auto-aktualisieren wenn Standortauswahl sich aendert ----
    # Uebernimmt die Wahl aus Tab 1, User kann aber manuell ueberschreiben
    observeEvent(filter4_Standortsgruppe(), ignoreNULL = TRUE, {
      req(filter4_Standortsgruppe())
      updateSelectInput(session, "plot_stgr_filter",
                        selected = filter4_Standortsgruppe())
    })
    
    observeEvent(filter1_Klimastufe(), ignoreNULL = TRUE, {
      req(filter1_Klimastufe())
      updateSelectInput(session, "plot_klima_filter",
                        selected = filter1_Klimastufe())
    })
    
    
    
    # ---- Kaskadierte Selects fuer KA5 ----
    observeEvent(input$wasserstandort, {
      filtered <- data_water_local %>% filter(Standort == input$wasserstandort)
      updateSelectInput(session, "wasserinput",
                        choices = c("Alle", unique(filtered$WASSER)))
    })
    
    observeEvent(input$bodtypstandort, {
      filtered <- data_bodtyp_local %>% filter(Standort == input$bodtypstandort)
      updateSelectInput(session, "bodtypinput",
                        choices = c("Alle", unique(filtered$BODTYP)))
    })
    
    observeEvent(input$bodartstandort, {
      filtered <- data_bodart_local %>% filter(Standort == input$bodartstandort)
      updateSelectInput(session, "bodartinput",
                        choices = c("Alle", unique(filtered$BODART)))
    })
    
    # ---- Text-Output: Gefilterte BZT-Ergebnisse ----
    
    observeEvent(filter4_Standortsgruppe(), {
      output$filtered_output <- renderUI({
        req(filtered_data())
        d <- filtered_data()
        validate(need(nrow(d) > 0, "Keine Daten fuer diese Auswahl."))
        HTML(paste0(
          "<b>Standortsbeschreibung:</b> ", paste(unique(d$Standortbeschreibung), collapse = ", "), "<br>",
          "<b>BZT-NR:</b> ",               paste(unique(d$BZT_Nr), collapse = ", "), "<br>",
          "<b>Baumarten 1:</b> ",           paste(unique(d$Baumart_1), collapse = ", "), "<br>",
          "<b>BZT_Bezeichnung:</b> ",       paste(unique(d$BZT_Bezeichnung), collapse = ", "), "<br>",
          "<b>Anteile BA1:</b> ",           paste(unique(d$Anteil_BA1), collapse = ", "), "<br>",
          "<b>Baumarten 2:</b> ",           paste(unique(d$Baumart_2), collapse = ", "), "<br>",
          "<b>Anteile BA2:</b> ",           paste(unique(d$Anteil_BA2), collapse = ", "), "<br>",
          "<b>Baumarten 3:</b> ",           paste(unique(d$Baumart_3), collapse = ", "), "<br>",
          "<b>Anteile BA3:</b> ",           paste(unique(d$Anteil_BA3), collapse = ", ")
        ))
      })
      
      output$baumart_bzt_nr <- DT::renderDT({
        req(filtered_data())
        DT::datatable(
          filtered_data()[, c("BZT_Nr", "Baumart_1", "Baumart_2", "Baumart_3")],
          options = list(
            scrollX  = TRUE,
            pageLength = 10,
            dom      = "tp"
          ),
          rownames = FALSE
        )
      })
    })
    
    # ---- Baumartenanteile-Tabellen ----
    
    filtered_data1 <- reactive({
      req(filter4_Standortsgruppe(), filter1_Klimastufe())
      B6_local %>%
        filter(STGR_Kart == filter4_Standortsgruppe() &
                 Klimastufe == filter1_Klimastufe()) %>%
        select(STGR_Kart, Klimastufe, BZT_Nr, Baumart_1:Max_Anteil_BA1,
               LH_Anteil, BA_Anzahl) %>%
        distinct()
    })
    
    filtered_data2 <- reactive({
      req(filter4_Standortsgruppe(), filter1_Klimastufe())
      B6_local %>%
        filter(STGR_Kart == filter4_Standortsgruppe() &
                 Klimastufe == filter1_Klimastufe()) %>%
        select(STGR_Kart, Klimastufe, BZT_Nr, Baumart_2:Max_Anteil_BA2,
               LH_Anteil, BA_Anzahl) %>%
        distinct()
    })
    
    filtered_data3 <- reactive({
      req(filter4_Standortsgruppe(), filter1_Klimastufe())
      B6_local %>%
        filter(STGR_Kart == filter4_Standortsgruppe() &
                 Klimastufe == filter1_Klimastufe()) %>%
        select(STGR_Kart, Klimastufe, BZT_Nr, Baumart_3:Max_Anteil_BA3,
               LH_Anteil, BA_Anzahl) %>%
        distinct()
    })
    
    observeEvent(input$Baumartenanteile_Tabelle, ignoreInit = TRUE, {
      # Baumartenanteile Tabellen mit optionaler Baumart-Hervorhebung
      render_table_with_highlight <- function(reactive_data, ba_filter) {
        if (!is.null(ba_filter) && ba_filter != "") {
          df <- reactive_data()
          df[] <- lapply(df, function(x) {
            if (is.character(x)) {
              str_replace_all(x, ba_filter,
                              function(m) paste0("<b>", m, "</b>"))
            } else { x }
          })
          df
        } else {
          reactive_data()
        }
      }
      
      output$filtered_data1 <- renderTable({
        render_table_with_highlight(filtered_data1, filter5_Baumart())
      }, sanitize.text.function = function(x) x)
      
      output$filtered_data2 <- renderTable({
        render_table_with_highlight(filtered_data2, filter5_Baumart())
      }, sanitize.text.function = function(x) x)
      
      output$filtered_data3 <- renderTable({
        render_table_with_highlight(filtered_data3, filter5_Baumart())
      }, sanitize.text.function = function(x) x)
    })
    
    
    # ---- BZT-Grafiken ----
    
    observeEvent(input$BZT_Grafiken, ignoreInit = TRUE, {
      req(input$BZT_Grafiken)
      
      # VERALTET: gg_plot + drei renderPlot (BA1/BA2/BA3 getrennt)
      # Ersetzt durch render_maxanteil_plot_alle() - 2026-04-22
      # gg_plot <- reactive({
      #   req(filter4_Standortsgruppe(), filter1_Klimastufe())
      #   plot_data <- B5_local %>%
      #     filter(STGR_Kart == filter4_Standortsgruppe() &
      #              Klimastufe == filter1_Klimastufe())
      #   validate(need(nrow(plot_data) > 0,
      #                 "Keine Daten fuer diese STGR/Klimastufe-Kombination."))
      #   plot_data %>%
      #     ggplot() +
      #     labs(title  = "Mindest- und Maximal-Baumartenanteile",
      #          x = "Baumart", y = "Anteil") +
      #     theme_minimal() +
      #     theme(axis.text.x = element_text(angle = 0, vjust = 0.5,
      #                                      hjust = 0.5, size = 12)) +
      #     annotate("text", x = Inf, y = Inf,
      #              label = paste("STGR:", filter4_Standortsgruppe(),
      #                            "\nKlimastufe:", filter1_Klimastufe()),
      #              hjust = 1, vjust = 1, size = 5, color = "black") +
      #     scale_y_continuous(
      #       labels = scales::percent_format(scale = 1),
      #       breaks = c(10, 20, 30, 40, 50, 60, 80, 100),
      #       limits = c(0, 100)
      #     )
      # })
      # 
      # output$Baumartenanteile_1 <- renderPlot(
      #   gg_plot() + aes(x = Baumart_1) +
      #     geom_bar(aes(y = Max_Anteil_BA1), stat = "identity", fill = "green4") +
      #     geom_bar(aes(y = Min_Anteil_BA1), stat = "identity", fill = "grey") +
      #     theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))
      # )
      # output$Baumartenanteile_2 <- renderPlot(
      #   gg_plot() + aes(x = Baumart_2) +
      #     geom_bar(aes(y = Max_Anteil_BA2), stat = "identity", fill = "green4") +
      #     geom_bar(aes(y = Min_Anteil_BA2), stat = "identity", fill = "grey") +
      #     theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))
      # )
      # output$Baumartenanteile_3 <- renderPlot(
      #   gg_plot() + aes(x = Baumart_3) +
      #     geom_bar(aes(y = Max_Anteil_BA3), stat = "identity", fill = "green4") +
      #     geom_bar(aes(y = Min_Anteil_BA3), stat = "identity", fill = "grey") +
      #     theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))
      # )
      
      output$Baumartenanteile_alle <- renderPlotly({
        req(input$plot_stgr_filter, input$plot_klima_filter)
        req(input$plot_stgr_filter  != "")
        req(input$plot_klima_filter != "")
        
        plot_data <- B5_local %>%
          filter(STGR_Kart  == input$plot_stgr_filter &
                   Klimastufe == input$plot_klima_filter)
        
        render_maxanteil_plot_alle(
          plot_data = plot_data,
          stgr_val  = input$plot_stgr_filter,
          klima_val = input$plot_klima_filter
        )
      })
      
    })
      
      
      # AUSKOMMENTIERT 2026-04-22
      
      # Prozentuale Baumartenanteile
    #   render_prc_plot <- function(ba_nr, baumart_col, prozent_col_max,
    #                               farben_col, mischung = FALSE) {
    #     renderPlotly({
    #       req(filter4_Standortsgruppe(), filter1_Klimastufe())
    #       prozent_col <- if (mischung) paste0("Prozentanteil_BA", ba_nr)
    #       else paste0("Prozentanteil_BA", ba_nr, "_max")
    #       
    #       fds <- B6_local %>%
    #         filter(STGR_Kart == filter4_Standortsgruppe() &
    #                  Klimastufe == filter1_Klimastufe()) %>%
    #         group_by(!!sym(baumart_col)) %>%
    #         mutate(prc_s = sum(!!sym(prozent_col)))
    #       
    #       validate(need(nrow(fds) > 0, "Keine Daten."))
    #       
    #       titel <- if (mischung) {
    #         paste("Mischungsprozente BA", ba_nr, "innerhalb BZT-Empfehlung")
    #       } else {
    #         paste("Max-Prozentanteile BA", ba_nr, "innerhalb BZT-Empfehlung")
    #       }
    #       
    #       p <- ggplot(fds) +
    #         aes(x = !!sym(baumart_col),
    #             fill = !!sym(farben_col),
    #             text = paste("Baumart:", !!sym(baumart_col),
    #                          "<br>Prozentsatz:", round(prc_s, 0))) +
    #         geom_bar(aes(y = !!sym(prozent_col)), stat = "identity",
    #                  show.legend = FALSE, color = NA) +
    #         labs(x = baumart_col, y = "Prozentsatz", title = titel) +
    #         theme_minimal() +
    #         theme(axis.text.x = element_text(angle = 45, vjust = 0.5,
    #                                          hjust = 0.5, size = 8)) +
    #         scale_fill_identity() +
    #         scale_y_continuous(
    #           labels = scales::percent_format(scale = 1),
    #           breaks = c(10, 20, 30, 40, 50, 60, 80, 100), limits = c(0, 100)
    #         )
    #       
    #       ggplotly(p, tooltip = "text") %>%
    #         layout(showlegend = FALSE,
    #                xaxis = list(title = baumart_col),
    #                yaxis = list(title = "Prozentsatz"),
    #                font  = list(size = 8)) %>%
    #         add_annotations(
    #           text      = paste("STGR:", filter4_Standortsgruppe(),
    #                             "<br>Klimastufe:", filter1_Klimastufe(),
    #                             "<br>Standort:", filter2_Standort()),
    #           x = 1, y = 1, xref = "paper", yref = "paper",
    #           showarrow = FALSE, xanchor = "right", yanchor = "top",
    #           align = "left", font = list(size = 8, color = "black")
    #         )
    #     })
    #   }
    #   
    #   output$Baumartenanteile_prc_1 <- render_prc_plot(1, "Baumart_1", "Prozentanteil_BA1_max", "BA.Farben1")
    #   output$Baumartenanteile_prc_2 <- render_prc_plot(2, "Baumart_2", "Prozentanteil_BA2_max", "BA.Farben2")
    #   output$Baumartenanteile_prc_3 <- render_prc_plot(3, "Baumart_3", "Prozentanteil_BA3_max", "BA.Farben3")
    #   
    #   output$Baumartenmischung_prc_1 <- render_prc_plot(1, "Baumart_1", "Prozentanteil_BA1", "BA.Farben1", mischung = TRUE)
    #   output$Baumartenmischung_prc_2 <- render_prc_plot(2, "Baumart_2", "Prozentanteil_BA2", "BA.Farben2", mischung = TRUE)
    #   output$Baumartenmischung_prc_3 <- render_prc_plot(3, "Baumart_3", "Prozentanteil_BA3", "BA.Farben3", mischung = TRUE)
  
    
    
    # ---- KA5-Plots: WASSER ----
    
    observeEvent(input$wasserinput, {
      req(input$wasserinput != " ")
      for (ba_nr in 1:3) {
        local({
          ba <- ba_nr
          output_id <- paste0("WASSER_BA_", ba)
          output[[output_id]] <- renderPlotly({
            render_KA5_plot(
              data_all     = data_water_local,
              filter_col   = "WASSER",
              filter_val   = input$wasserinput,
              standort_val = input$wasserstandort,
              klima_val    = input$wasserinputklima,
              ba_nr        = ba,
              titel_prefix = "WASSER",
              filter_label = "Wasserstufe"
            )
          })
        })
      }
    })
    
    
    # ---- KA5-Plots: BODTYP ----
    
    observeEvent(input$bodtypinput, {
      req(input$bodtypinput != " ")
      for (ba_nr in 1:3) {
        local({
          ba <- ba_nr
          output_id <- paste0("BODTYP_BA_", ba)
          output[[output_id]] <- renderPlotly({
            render_KA5_plot(
              data_all     = data_bodtyp_local,
              filter_col   = "BODTYP",
              filter_val   = input$bodtypinput,
              standort_val = input$bodtypstandort,
              klima_val    = input$bodtypinputklima,
              ba_nr        = ba,
              titel_prefix = "BODTYP",
              filter_label = "Bodentyp"
            )
          })
        })
      }
    })
    
    
    # ---- KA5-Plots: BODART ----
    
    observeEvent(input$bodartinput, {
      req(input$bodartinput != " ")
      for (ba_nr in 1:3) {
        local({
          ba <- ba_nr
          output_id <- paste0("BODART_BA_", ba)
          output[[output_id]] <- renderPlotly({
            render_KA5_plot(
              data_all     = data_bodart_local,
              filter_col   = "BODART",
              filter_val   = input$bodartinput,
              standort_val = input$bodartstandort,
              klima_val    = input$bodartinputklima,
              ba_nr        = ba,
              titel_prefix = "BODART",
              filter_label = "Bodenart"
            )
          })
        })
      }
    })
    
  })
}