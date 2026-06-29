# R/mod_standort.R ----
# Vollanalyse eines einzelnen BWI-Standorts ueber alle Klimalaeufe.
# Abhaengigkeiten: klima_meta, kat_palette, cache-Funktionen aus cache.R

# ── Bundesland-Mapping (aus BWI MASTER_ID-Kuerzel) ───────────
# MASTER_ID-Schema: BWI_{BL-Code}_{Nr}_{Sub}
# BL-Code -> Bundesland (Quelle: BWI-Auswertung)
bundesland_map <- c(
  "10"  = "Schleswig-Holstein",
  "20"  = "Hansestadt Hamburg",
  "30"  = "Niedersachsen",
  "40"  = "Hansestadt Bremen",
  "50"  = "Nordrhein-Westfalen",
  "60"  = "Hessen",
  "70"  = "Rheinland-Pfalz",
  "80"  = "Baden-W\u00fcrttemberg",
  "90"  = "Bayern",
  "100" = "Saarland",
  "110" = "Berlin",
  "120" = "Brandenburg",
  "130" = "Mecklenburg-Vorpommern",
  "140" = "Sachsen",
  "150" = "Sachsen-Anhalt",
  "160" = "Th\u00fcringen"
)

# Bundesland-Code aus MASTER_ID extrahieren
# z.B. "BWI_130_35672_2" -> "130"
get_bl_code <- function(master_id) {
  stringr::str_match(as.character(master_id),
                     "^(?:BWI|BZE)_(\\d+)_")[, 2]
}

# MASTER_ID-Liste aufbauen fuer Selector
# Gibt data.frame mit MASTER_ID, Region, BL_Code, Bundesland, NR_ID zurueck.
# NR-Standorte kommen aus NR_GEO_ALL (Shapefile), nicht aus BWI_GEO – ohne
# diesen Teil ist der Region-Filter "NR" in der Standortanalyse immer leer.
build_master_id_table <- function() {
  teile <- list()

  geo <- get0("BWI_GEO", envir = .GlobalEnv)
  if (!is.null(geo)) {
    ids   <- as.character(geo$MASTER_ID)
    regio <- dplyr::case_when(
      startsWith(ids, "BZE") ~ "BZE",
      TRUE                    ~ "BWI"
    )
    bl    <- get_bl_code(ids)
    bl_nm <- bundesland_map[bl]
    bl_nm[is.na(bl_nm)] <- "Unbekannt"
    teile$bwi <- data.frame(
      MASTER_ID  = ids,
      Region     = regio,
      BL_Code    = bl,
      Bundesland = unname(bl_nm),
      NR_ID      = NA_character_,
      stringsAsFactors = FALSE
    )
  }

  nr_geo <- get0("NR_GEO_ALL", envir = .GlobalEnv)
  if (!is.null(nr_geo) && nrow(nr_geo) > 0) {
    teile$nr <- data.frame(
      MASTER_ID  = as.character(nr_geo$MASTER_ID),
      Region     = "NR",
      BL_Code    = NA_character_,
      Bundesland = "–",
      NR_ID      = as.character(nr_geo$NR_ID),
      stringsAsFactors = FALSE
    )
  }

  if (length(teile) == 0) return(data.frame())
  dplyr::bind_rows(teile)
}

# ── NR-CSV-Inventar fuer eine Nachbarschaftsregion ───────────
# Findet alle BAE-CSVs einer NR unter BAE_WM_DIR und parst TV, Szenario,
# Modell und Zeitraum aus dem Dateinamen.
# Schema: BAE_{TV}_{Stufe}_{NR}_{Szenario}_{Modell}_{Zeitraum}_{Baumart}.csv
nr_csv_inventar <- function(nr_id) {
  basis <- get0("BAE_WM_DIR", envir = .GlobalEnv)
  if (is.null(nr_id) || is.null(basis) || !dir.exists(basis)) return(data.frame())

  files <- list.files(basis,
                      pattern    = paste0("^BAE_.*_", nr_id, "_.*\\.csv(\\.gz)?$"),
                      full.names = TRUE, recursive = TRUE,
                      ignore.case = TRUE)
  if (length(files) == 0) return(data.frame())

  meta <- lapply(files, function(f) {
    bn     <- sub("\\.csv(\\.gz)?$", "", basename(f))
    teile  <- strsplit(bn, "_", fixed = TRUE)[[1]]
    pos_nr <- which(tolower(teile) == tolower(nr_id))[1]
    if (is.na(pos_nr) || pos_nr < 3) return(NULL)
    zr_pos <- grep("^\\d{4}-\\d{4}$", teile)
    zr_pos <- zr_pos[zr_pos > pos_nr]
    if (length(zr_pos) == 0) return(NULL)
    zr_pos <- zr_pos[length(zr_pos)]
    if (zr_pos >= length(teile)) return(NULL)   # Baumart muss noch folgen
    modell <- if (zr_pos - pos_nr >= 2)
      paste(teile[(pos_nr + 2):(zr_pos - 1)], collapse = "_") else "(OBS)"
    data.frame(file     = f,
               TV       = teile[2],
               Szenario = teile[pos_nr + 1],
               Modell   = modell,
               Zeitraum = teile[zr_pos],
               Baumart  = paste(teile[(zr_pos + 1):length(teile)], collapse = "_"),
               stringsAsFactors = FALSE)
  })
  meta <- Filter(Negate(is.null), meta)
  if (length(meta) == 0) return(data.frame())
  do.call(rbind, meta)
}

# ── Alle Klimalaeufe fuer eine MASTER_ID laden ───────────────
# Gibt einen tibble zurueck mit:
# MASTER_ID, Baumart, TV, Zeitlauf, Szenario, Modell, Zeitraum,
# BAE_3ST, BAE_4ST, BAE_5ST, BAE_7ST (soweit vorhanden)

lade_standort_alle_laeufe <- function(master_id,
                                      baumarten = NULL,
                                      tvs       = NULL,
                                      modelle   = NULL,
                                      szenarien = NULL,
                                      zeitraeume = NULL,
                                      region    = "BWI",
                                      nr_id     = NULL) {
  master_id_chr <- as.character(master_id)

  # 1. Cache pruefen
  cached <- cache_standort_get(master_id_chr)
  if (!is.null(cached)) {
    message("Standort aus Cache geladen: ", master_id_chr)
    message("  >> Cache-TVs: ", paste(sort(unique(cached$TV)), collapse=", "))  # NEU
    df <- cached
  } else if (identical(region, "NR")) {
    inv <- nr_csv_inventar(nr_id)
    message("  >> inv-TVs (Dateinamen): ", paste(sort(unique(inv$TV)), collapse=", "))  # NEU
    if (nrow(inv) == 0) {
      message("Standortanalyse NR: keine CSVs fuer ", nr_id %||% "(keine NR)",
              " unter ", get0("BAE_WM_DIR", envir = .GlobalEnv) %||% "(BAE_WM_DIR fehlt)")
      return(data.frame())
    }
    message("Standortanalyse NR: ", nrow(inv), " CSVs fuer ", nr_id,
            " | Standort ", master_id_chr)
    df <- dplyr::bind_rows(lapply(seq_len(nrow(inv)), function(i) {
      d <- tryCatch(data.table::fread(inv$file[i], data.table = FALSE),
                    error = function(e) NULL)
      if (is.null(d)) return(NULL)
      names(d) <- toupper(names(d))
      if (!"MASTER_ID" %in% names(d)) return(NULL)
      row <- d[as.character(d$MASTER_ID) == master_id_chr, , drop = FALSE]
      if (nrow(row) == 0) return(NULL)
      row <- row[1, , drop = FALSE]
      bae_cols <- intersect(c("BAE_3ST","BAE_4ST","BAE_5ST","BAE_7ST"), names(row))
      out <- data.frame(MASTER_ID = master_id_chr, stringsAsFactors = FALSE)
      out$Baumart  <- if ("BAUMART" %in% names(row))
        as.character(row$BAUMART) else inv$Baumart[i]
      out$TV       <- as.character(as.integer(inv$TV[i]))
      out$Zeitlauf <- paste(inv$Szenario[i], inv$Modell[i], inv$Zeitraum[i], sep = "_")
      out$Szenario <- inv$Szenario[i]
      out$Modell   <- inv$Modell[i]
      out$Zeitraum <- inv$Zeitraum[i]
      for (bc in bae_cols) out[[bc]] <- as.character(row[[bc]])
      out
    }))
    if (nrow(df) > 0) {
      # Gleiche Kombination kann aus mehreren Stufen-Ordnern (3ST/4ST/5ST)
      # kommen – BAE-Spalten zusammenfuehren statt Zeilen zu verlieren.
      df <- df %>%
        dplyr::group_by(MASTER_ID, Baumart, TV, Zeitlauf,
                        Szenario, Modell, Zeitraum) %>%
        dplyr::summarise(
          dplyr::across(dplyr::any_of(c("BAE_3ST","BAE_4ST","BAE_5ST","BAE_7ST")),
                        ~ { v <- .x[!is.na(.x)]; if (length(v)) v[1] else NA_character_ }),
          .groups = "drop") %>%
        as.data.frame()
      df$TV <- paste0("TV", df$TV)
      cache_standort_set(df)
      message("NR-Standort geladen + gecacht: ", master_id_chr,
              " (", nrow(df), " Zeilen)")
    }
  } else {
    # 2b. BWI/BZE: alle 38 CSVs lesen
    df <- dplyr::bind_rows(lapply(seq_len(nrow(klima_meta)), function(i) {
      d <- tryCatch(data.table::fread(klima_meta$file[i], data.table = FALSE),
                    error = function(e) NULL)
      if (is.null(d)) return(NULL)
      names(d) <- toupper(names(d))
      row <- d[d$MASTER_ID == master_id_chr, , drop = FALSE]
      if (nrow(row) == 0) return(NULL)
      
      # BAE-Spalten normalisieren
      bae_cols <- intersect(c("BAE_3ST","BAE_4ST","BAE_5ST","BAE_7ST"), names(row))
      row <- row[, c("MASTER_ID", "BAUMART", "TV", bae_cols), drop = FALSE]
      row$Zeitlauf <- paste(klima_meta$Szenario[i],
                            klima_meta$Modell[i],
                            klima_meta$Zeitraum[i], sep = "_")
      row$Szenario <- klima_meta$Szenario[i]
      row$Modell   <- klima_meta$Modell[i]
      row$Zeitraum <- klima_meta$Zeitraum[i]
      dplyr::rename(row, Baumart = BAUMART)
    }))
    
    if (nrow(df) > 0) {
      df$TV <- paste0("TV", df$TV)
      # In Cache schreiben fuer spaetere Aufrufe
      cache_standort_set(df)
      message("Standort geladen + gecacht: ", master_id_chr,
              " (", nrow(df), " Zeilen)")
    }
  }
  
  if (nrow(df) == 0) return(df)
  
  message("tvs-Filter: ", if(is.null(tvs)) "NULL" else paste(tvs, collapse=", "))
  
  # 3. Optionale Filter
  if (!is.null(baumarten) && length(baumarten) > 0)
    df <- df[df$Baumart %in% baumarten, ]
  if (!is.null(tvs) && length(tvs) > 0)
    df <- df[df$TV %in% paste0("TV", tvs), ]
  if (!is.null(modelle) && length(modelle) > 0)
    df <- df[df$Modell %in% modelle, ]
  if (!is.null(szenarien) && length(szenarien) > 0)
    df <- df[df$Szenario %in% szenarien, ]
  if (!is.null(zeitraeume) && length(zeitraeume) > 0)
    df <- df[df$Zeitraum %in% zeitraeume, ]

  df
}

# ── ggplot-Heatmap: Baumart x TV ─────────────────────────────
# Gibt ein ggplot-Objekt zurueck (kein ggsave).
# stufe: "BAE_3ST" | "BAE_4ST" | "BAE_5ST"
# klimalauf_filter: optionaler Vektor von Zeitlauf-Strings

heatmap_standort_ggplot <- function(df, stufe = "BAE_4ST",
                                    klimalauf_filter = NULL) {
  kat_col <- toupper(stufe)
  if (!kat_col %in% names(df)) {
    return(ggplot2::ggplot() +
             ggplot2::annotate("text", x=0.5, y=0.5,
                               label=paste0("Spalte '", kat_col, "' nicht vorhanden"),
                               size=5) + ggplot2::theme_void())
  }
  
  stufe_maps <- list(
    BAE_3ST = c("1"="sehr empfohlen","2"="m\u00e4\u00dfig empfohlen","3"="nicht empfohlen"),
    BAE_4ST = c("1"="sehr empfohlen","2"="empfohlen","3"="m\u00e4\u00dfig empfohlen","4"="nicht empfohlen"),
    BAE_5ST = c("1"="sehr empfohlen","2"="empfohlen","3"="m\u00e4\u00dfig empfohlen",
                "4"="wenig empfohlen","5"="nicht empfohlen"),
    BAE_7ST = c("1"="sehr empfohlen","2"="sehr empfohlen","3"="empfohlen",
                "4"="m\u00e4\u00dfig empfohlen","5"="wenig empfohlen",
                "6"="nicht empfohlen","7"="nicht empfohlen")
  )
  m <- stufe_maps[[kat_col]]
  
  d <- df
  if (!is.null(klimalauf_filter))
    d <- d[d$Zeitlauf %in% klimalauf_filter, ]
  if (nrow(d) == 0)
    return(ggplot2::ggplot() +
             ggplot2::annotate("text",x=0.5,y=0.5,label="Keine Daten",size=5) +
             ggplot2::theme_void())
  
  d$Kat <- dplyr::case_when(
    as.character(d[[kat_col]]) %in% names(m) ~ unname(m[as.character(d[[kat_col]])]),
    as.character(d[[kat_col]]) == "pBv"       ~ "pBv",
    TRUE                                       ~ "Keine Datengrundlage"
  )
  d$Kat <- factor(d$Kat, levels = names(kat_palette))
  d$TV  <- factor(d$TV,  levels = sort(unique(d$TV), decreasing = TRUE))
  d$Baumart <- factor(d$Baumart, levels = sort(unique(d$Baumart)))
  d$Zeitraum_kurz <- sub(".*_", "", d$Zeitlauf)
  
  ggplot2::ggplot(d, ggplot2::aes(x = Baumart, y = TV, fill = Kat)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.5) +
    ggplot2::scale_fill_manual(values = kat_palette, na.value = "#B0B0B0",
                               breaks = names(kat_palette), drop = TRUE) +
    ggplot2::facet_grid(Szenario ~ Zeitraum) +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::labs(
      title    = paste0("BAE-Heatmap \u2013 ", unique(d$MASTER_ID)[1]),
      subtitle = paste0(gsub("BAE_","",kat_col), "-stufig  |  alle Klimalaeufe"),
      x = NULL, y = "TV", fill = "Empfehlung") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      strip.text       = ggplot2::element_text(face = "bold", size = 9),
      axis.text.x      = ggplot2::element_text(angle = 0, hjust = 0.5,
                                               face = "bold", size = 9),
      axis.text.y      = ggplot2::element_text(size = 9),
      panel.grid       = ggplot2::element_blank(),
      legend.position  = "bottom",
      legend.direction = "horizontal",
      legend.text      = ggplot2::element_text(size = 9),
      plot.background  = ggplot2::element_rect(fill = "white", color = NA)
    )
}

# ── Standalone: Zukunfts-Heatmap (RCP45 + RCP85_MPICLM_2071-2100) ──
# Erzeugt EINE kombinierte, gefacettete Heatmap nur fuer die Zukunfts-
# Klimalaeufe: facet_grid(Szenario ~ Zeitraum), Baumart auf der x-Achse,
# TV auf der y-Achse – entspricht der "blau umkreisten" Auswahl in der App.
#
# Die Funktion kann solo verwendet werden (wie heatmap_bae_function):
#   - liefert IMMER ein ggplot-Objekt zurueck
#   - speichert zusaetzlich ein PNG, wenn out_dir gesetzt ist
#     (out_dir = NULL unterdrueckt das Speichern, z.B. fuer die App).
#
# data    : data.frame wie aus lade_standort_alle_laeufe() – benoetigt
#           MASTER_ID, Baumart, TV, Zeitlauf, Szenario, Zeitraum, BAE_*ST.
# stufe   : "BAE_3ST" | "BAE_4ST" | "BAE_5ST" | "BAE_7ST"
# zukunft : Vektor regulaerer Ausdruecke, die gegen Zeitlauf gematcht werden.
#           Default = alle RCP45-Laeufe (inkl. Varianten RCP45-v2/-v3 bzw.
#           Modellvarianten ECECMO-v2/MPICLM-v2) + RCP85_MPICLM_2071-2100
#           (inkl. MPICLM-Varianten). Hinweis: "^RCP45" ohne abschliessenden
#           Unterstrich, damit auch "RCP45-v2_..." (Bindestrich) gematcht wird.

heatmap_bae_zukunft_function <- function(
    data, master_id,
    stufe   = "BAE_4ST",
    zukunft = c("^RCP45", "^RCP85_MPICLM.*2071-2100$"),
    out_dir = "04_results/BAE_Auswertung/heatmap") {

  kat_col <- toupper(stufe)

  leer <- function(txt) ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0.5, y = 0.5, label = txt, size = 5) +
    ggplot2::theme_void()

  stufe_maps <- list(
    BAE_3ST = c("1"="sehr empfohlen","2"="mäßig empfohlen","3"="nicht empfohlen"),
    BAE_4ST = c("1"="sehr empfohlen","2"="empfohlen","3"="mäßig empfohlen","4"="nicht empfohlen"),
    BAE_5ST = c("1"="sehr empfohlen","2"="empfohlen","3"="mäßig empfohlen",
                "4"="wenig empfohlen","5"="nicht empfohlen"),
    BAE_7ST = c("1"="sehr empfohlen","2"="sehr empfohlen","3"="empfohlen",
                "4"="mäßig empfohlen","5"="wenig empfohlen",
                "6"="nicht empfohlen","7"="nicht empfohlen")
  )
  m <- stufe_maps[[kat_col]]
  if (is.null(m) || !kat_col %in% names(data))
    return(leer(paste0("Stufe '", kat_col, "' nicht verfügbar")))

  # 1. Auf MASTER_ID filtern
  d <- data[as.character(data$MASTER_ID) == as.character(master_id), , drop = FALSE]
  if (nrow(d) == 0) {
    message("Keine Daten für MASTER_ID: ", master_id)
    return(leer(paste0("Keine Daten: ", master_id)))
  }

  # 2. Auf Zukunfts-Klimalaeufe filtern (Regex gegen Zeitlauf)
  treffer <- Reduce(`|`, lapply(zukunft, function(p) grepl(p, d$Zeitlauf)))
  d <- d[treffer, , drop = FALSE]
  if (nrow(d) == 0) {
    message("Keine Zukunfts-Klimalaeufe (", paste(zukunft, collapse = ", "),
            ") für ", master_id)
    return(leer("Keine Zukunfts-Klimalaeufe"))
  }

  # 3. Kategorien mappen
  d$Kat <- dplyr::case_when(
    as.character(d[[kat_col]]) %in% names(m) ~ unname(m[as.character(d[[kat_col]])]),
    as.character(d[[kat_col]]) == "pBv"       ~ "pBv",
    TRUE                                       ~ "Keine Datengrundlage"
  )
  d$Kat     <- factor(d$Kat, levels = names(kat_palette))
  d$TV      <- factor(d$TV,  levels = sort(unique(d$TV), decreasing = TRUE))
  d$Baumart <- factor(d$Baumart, levels = sort(unique(d$Baumart)))

  # 4. Plot: Szenario (Zeilen) x Zeitraum (Spalten), Baumart x TV je Block
  p <- ggplot2::ggplot(d, ggplot2::aes(x = Baumart, y = TV, fill = Kat)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.5) +
    ggplot2::scale_fill_manual(values = kat_palette, na.value = "#B0B0B0",
                               breaks = names(kat_palette), drop = TRUE) +
    ggplot2::facet_grid(Szenario ~ Zeitraum) +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::labs(
      title    = paste0("BAE-Heatmap Zukunft – ", as.character(master_id)),
      subtitle = paste0(gsub("BAE_", "", kat_col), "-stufig  |  ",
                        paste(zukunft, collapse = "  |  ")),
      x = NULL, y = "TV", fill = "Empfehlung") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      strip.text       = ggplot2::element_text(face = "bold", size = 9),
      axis.text.x      = ggplot2::element_text(angle = 0, hjust = 0.5,
                                               face = "bold", size = 9),
      axis.text.y      = ggplot2::element_text(size = 9),
      panel.grid       = ggplot2::element_blank(),
      legend.position  = "bottom",
      legend.direction = "horizontal",
      legend.text      = ggplot2::element_text(size = 9),
      plot.background  = ggplot2::element_rect(fill = "white", color = NA)
    )

  # 5. Optional als PNG speichern (Solo-Nutzung wie heatmap_bae_function)
  if (!is.null(out_dir)) {
    mid_dir <- file.path(out_dir, as.character(master_id))
    dir.create(mid_dir, showWarnings = FALSE, recursive = TRUE)
    datei <- file.path(mid_dir,
                       paste0("Heatmap_Zukunft_", master_id, "_",
                              gsub("BAE_", "", kat_col), ".png"))
    ggplot2::ggsave(datei, plot = p, device = "png",
                    width = 5400, height = 3000, units = "px", dpi = 300)
    message("Gespeichert: ", datei)
  }

  p
}

# ── ggplot-Balken: Anteil je Kategorie ueber Zeit/Szenario ───
# x_var: "Szenario" | "Zeitraum" | "Modell" | "Zeitlauf"

auswertung_standort_ggplot <- function(df, stufe = "BAE_4ST",
                                       x_var = "Szenario") {
  kat_col <- toupper(stufe)
  if (!kat_col %in% names(df))
    return(ggplot2::ggplot() +
             ggplot2::annotate("text",x=0.5,y=0.5,
                               label=paste0("Spalte '",kat_col,"' nicht vorhanden"),size=5)+
             ggplot2::theme_void())
  
  stufe_levels <- list(
    BAE_3ST = c("nicht empfohlen","m\u00e4\u00dfig empfohlen","sehr empfohlen","Keine Datengrundlage","pBv"),
    BAE_4ST = c("nicht empfohlen","m\u00e4\u00dfig empfohlen","empfohlen","sehr empfohlen","Keine Datengrundlage","pBv"),
    BAE_5ST = c("nicht empfohlen","wenig empfohlen","m\u00e4\u00dfig empfohlen","empfohlen","sehr empfohlen","Keine Datengrundlage","pBv"),
    BAE_7ST = c("nicht empfohlen","wenig empfohlen","m\u00e4\u00dfig empfohlen","empfohlen","sehr empfohlen","Keine Datengrundlage","pBv")
  )
  lvls <- stufe_levels[[kat_col]] %||% names(kat_palette)
  
  m <- list(
    BAE_3ST=c("1"="sehr empfohlen","2"="m\u00e4\u00dfig empfohlen","3"="nicht empfohlen"),
    BAE_4ST=c("1"="sehr empfohlen","2"="empfohlen","3"="m\u00e4\u00dfig empfohlen","4"="nicht empfohlen"),
    BAE_5ST=c("1"="sehr empfohlen","2"="empfohlen","3"="m\u00e4\u00dfig empfohlen","4"="wenig empfohlen","5"="nicht empfohlen"),
    BAE_7ST=c("1"="sehr empfohlen","2"="sehr empfohlen","3"="empfohlen","4"="m\u00e4\u00dfig empfohlen","5"="wenig empfohlen","6"="nicht empfohlen","7"="nicht empfohlen")
  )[[kat_col]]
  
  d <- df %>%
    dplyr::mutate(Kat = dplyr::case_when(
      as.character(.data[[kat_col]]) %in% names(m) ~ unname(m[as.character(.data[[kat_col]])]),
      as.character(.data[[kat_col]]) == "pBv"       ~ "pBv",
      TRUE                                           ~ "Keine Datengrundlage"
    ),
    Kat = factor(Kat, levels = rev(lvls)),
    x   = .data[[x_var]])
  
  d %>%
    dplyr::group_by(Baumart, x, Kat, .drop = FALSE) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    dplyr::group_by(Baumart, x) %>%
    dplyr::mutate(Pct = n / sum(n) * 100) %>%
    dplyr::ungroup() %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = Pct, fill = Kat)) +
    ggplot2::geom_col(position = "stack", width = 0.7) +
    ggplot2::scale_fill_manual(values = kat_palette,
                               breaks = rev(lvls), drop = TRUE) +
    ggplot2::scale_y_continuous(
      labels = scales::label_percent(scale = 1),
      expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::facet_wrap(~ Baumart) +
    ggplot2::labs(
      title    = paste0("BAE \u2013 ", unique(df$MASTER_ID)[1]),
      subtitle = paste0(gsub("BAE_","",kat_col), "-stufig  |  nach ", x_var),
      x = x_var, y = "Anteil (%)", fill = "Kategorie") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      axis.text.x     = ggplot2::element_text(angle=45, hjust=1, size=8),
      strip.text      = ggplot2::element_text(face="bold"),
      legend.position = "top", legend.direction = "horizontal",
      legend.text     = ggplot2::element_text(size=9),
      plot.background = ggplot2::element_rect(fill="white", color=NA)
    )
}

# ── ggplot-Grid: Szenario x Zeitraum ─────────────────────────

auswertung_grid_ggplot <- function(df, stufe = "BAE_4ST") {
  kat_col <- toupper(stufe)
  m <- list(
    BAE_3ST=c("1"="sehr empfohlen","2"="m\u00e4\u00dfig empfohlen","3"="nicht empfohlen"),
    BAE_4ST=c("1"="sehr empfohlen","2"="empfohlen","3"="m\u00e4\u00dfig empfohlen","4"="nicht empfohlen"),
    BAE_5ST=c("1"="sehr empfohlen","2"="empfohlen","3"="m\u00e4\u00dfig empfohlen","4"="wenig empfohlen","5"="nicht empfohlen")
  )[[kat_col]]
  if (is.null(m) || !kat_col %in% names(df))
    return(ggplot2::ggplot() +
             ggplot2::annotate("text",x=0.5,y=0.5,label="Nicht verfügbar",size=5)+
             ggplot2::theme_void())
  
  df %>%
    dplyr::mutate(Kat = dplyr::case_when(
      as.character(.data[[kat_col]]) %in% names(m) ~ unname(m[as.character(.data[[kat_col]])]),
      TRUE ~ "Keine Datengrundlage"),
      Kat = factor(Kat, levels = rev(names(kat_palette)))) %>%
    dplyr::group_by(Baumart, Szenario, Zeitraum, Kat, .drop=FALSE) %>%
    dplyr::summarise(n = dplyr::n(), .groups="drop") %>%
    dplyr::group_by(Baumart, Szenario, Zeitraum) %>%
    dplyr::mutate(Pct = n/sum(n)*100) %>%
    dplyr::ungroup() %>%
    ggplot2::ggplot(ggplot2::aes(x=Zeitraum, y=Pct, fill=Kat)) +
    ggplot2::geom_col(position="stack", width=0.7) +
    ggplot2::scale_fill_manual(values=kat_palette, breaks=rev(names(kat_palette)), drop=TRUE) +
    ggplot2::scale_y_continuous(labels=scales::label_percent(scale=1),
                                expand=ggplot2::expansion(mult=c(0,0.05))) +
    ggplot2::facet_grid(rows=ggplot2::vars(Szenario), cols=ggplot2::vars(Baumart)) +
    ggplot2::labs(
      title    = paste0("BAE-Grid \u2013 ", unique(df$MASTER_ID)[1]),
      subtitle = paste0(gsub("BAE_","",kat_col), "-stufig  |  Szenario \u00d7 Zeitraum"),
      x="Zeitraum", y="Anteil (%)", fill="Kategorie") +
    ggplot2::theme_minimal(base_size=10) +
    ggplot2::theme(
      strip.text      = ggplot2::element_text(face="bold", size=8),
      axis.text.x     = ggplot2::element_text(angle=45, hjust=1, size=7),
      legend.position = "top", legend.direction = "horizontal",
      panel.spacing   = ggplot2::unit(0.4,"lines"),
      plot.background = ggplot2::element_rect(fill="white", color=NA)
    )
}