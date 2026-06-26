# =====================================================================
#  Klimaübung 2026 - Gruppe 8: Temperatur und Feuchte (Messstation)
#  Messturm Horstmarer Landweg
#
#  Schritte 1-7:
#   1. Datenimport & Bereinigung
#   2. Absolute Feuchte berechnen (Magnus-Formel)
#   3. Temperatur-Auswertung
#   4. Feuchte-Auswertung
#   5. Zusammenhang Temperatur <-> Feuchte (inkl. Korrelations-Abbildung)
#   6. Interessante Ereignisse + Strahlung & Niederschlag (begruendet)
#   7. Grafiken exportieren (PNG, 300 dpi, PowerPoint-tauglich)
#
#  Hinweis: Es werden NUR die Daten der Messstation verwendet.
# =====================================================================

# --- Arbeitsverzeichnis ---------------------------------------------
setwd("c:/Users/nilsg/repos/analysis-measurent-tower-UEKlim2026/")

# --- Pakete laden ---------------------------------------------------
pakete <- c("tidyverse", "lubridate", "scales", "patchwork")
fehlend <- pakete[!pakete %in% rownames(installed.packages())]
if (length(fehlend) > 0) install.packages(fehlend)
invisible(lapply(pakete, library, character.only = TRUE))

# --- Pfade ----------------------------------------------------------
csv_pfad <- "data/metMastHorstmarerLandweg.CSV"
fig_dir  <- "output/figures"
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

# --- Einheitliches, PowerPoint-taugliches Theme ---------------------
# Grosse Schrift, kraeftige Linien, ruhiges Raster -> auch projiziert lesbar.
theme_klim <- theme_bw(base_size = 18) +
  theme(
    plot.title    = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(size = 14, colour = "grey30"),
    axis.title    = element_text(size = 16),
    axis.text     = element_text(size = 14, colour = "black"),
    legend.position = "top",
    legend.text   = element_text(size = 15),
    panel.grid.minor = element_blank(),
    plot.margin   = margin(10, 14, 10, 10)
  )
theme_set(theme_klim)

# Einheitliche Farben
col_temp <- "#C0392B"   # rot   - Temperatur
col_rh   <- "#2471A3"   # blau  - relative Feuchte
col_ah   <- "#1E8449"   # gruen - absolute Feuchte
col_sw   <- "#E67E22"   # orange- Strahlung

# Standard-Exportgroessen (16:9-tauglich)
W <- 12; H <- 6.5            # volle Folienbreite (Querformat)
ggsave_pp <- function(name, plot, w = W, h = H)
  ggsave(file.path(fig_dir, name), plot, width = w, height = h,
         dpi = 300, bg = "white")

# =====================================================================
# SCHRITT 1: Datenimport & Bereinigung
# =====================================================================

# Spaltennamen (24 Spalten; die 3 leeren Endspalten wurden in der CSV entfernt)
spalten <- c(
  "zeit", "id", "niederschlag",
  "t_min", "t_avg", "t_max",
  "rh_min", "rh_avg", "rh_max",
  "p_avg",
  "sw_up", "sw_down", "lw_up", "lw_down", "t_strahl",
  "wind_us", "wdir_us", "wdir_us_sd",
  "wind_prop", "wdir_prop", "wdir_prop_sd",
  "batt_min", "tlog_min", "tlog_max"
)

# Datei: ISO-8859-1, ';'-getrennt, Dezimalpunkt, 3 Kopfzeilen
roh <- read.table(
  csv_pfad,
  header = FALSE, sep = ";", dec = ".",
  skip = 3,
  col.names = spalten,
  na.strings = c("NAN", "NaN", "NA", ""),
  fileEncoding = "latin1",
  stringsAsFactors = FALSE,
  fill = TRUE
)

dat <- roh %>%
  mutate(zeit = ymd_hms(zeit, tz = "UTC")) %>%
  arrange(zeit) %>%
  filter(!is.na(zeit))

# --- Qualitaetskontrolle (QC) ---------------------------------------
# Strahlung physikalisch nie negativ; rel. Feuchte auf 100 % deckeln.
dat <- dat %>%
  mutate(
    sw_up   = ifelse(sw_up   < 0, NA, sw_up),
    sw_down = ifelse(sw_down < 0, NA, sw_down),
    rh_avg  = pmin(rh_avg, 100),
    rh_min  = pmin(rh_min, 100),
    rh_max  = pmin(rh_max, 100)
  )

# Zeitliche Luecken erkennen (Sollintervall = 10 min)
luecken <- dat %>%
  mutate(diff_min = as.numeric(difftime(zeit, lag(zeit), units = "mins"))) %>%
  filter(diff_min > 10) %>%
  select(zeit, diff_min)

cat("=== Datenueberblick ===\n")
cat("Zeilen:", nrow(dat), "\n")
cat("Zeitraum:", format(min(dat$zeit)), "bis", format(max(dat$zeit)), "\n")
cat("Anzahl Luecken (>10 min):", nrow(luecken), "\n")
if (nrow(luecken) > 0) print(luecken)

# --- Linien an Luecken unterbrechen ---------------------------------
# Volles 10-min-Raster erzeugen und fehlende Zeitpunkte mit NA auffuellen.
# ueber NA zeichnet ggplot keine Linie -> Luecken bleiben sichtbar offen,
# ohne dass Werte erfunden oder Nachbarpunkte verbunden werden.
voll_raster <- tibble(zeit = seq(min(dat$zeit), max(dat$zeit), by = 600))
dat <- voll_raster %>% left_join(dat, by = "zeit") %>% arrange(zeit)

# Groesste Luecke fuer die graue Markierung in den Gesamtverlaeufen
if (nrow(luecken) > 0) {
  haupt <- luecken %>% slice_max(diff_min, n = 1, with_ties = FALSE)
  luecke_ende  <- haupt$zeit
  luecke_start <- haupt$zeit - dminutes(haupt$diff_min)
  markiere_luecke <- list(
    annotate("rect", xmin = luecke_start, xmax = luecke_ende,
             ymin = -Inf, ymax = Inf, fill = "grey75", alpha = 0.45),
    annotate("text", x = luecke_start + (luecke_ende - luecke_start) / 2,
             y = Inf, label = "Datenlücke", vjust = 1.5, size = 4.5,
             colour = "grey25")
  )
} else {
  markiere_luecke <- NULL
}

# =====================================================================
# SCHRITT 2: Absolute Feuchte berechnen (Magnus-Formel)
# =====================================================================
# Saettigungsdampfdruck e_s [hPa] ueber Wasser (Magnus, WMO):
#   e_s(T) = 6.112 * exp(17.62 * T / (243.12 + T))   , T in degC
# Tatsaechlicher Dampfdruck:   e = RH/100 * e_s
# Absolute Feuchte [g/m3]:     AH = 216.7 * e / (T + 273.15)
#   (aus rho_v = e/(R_w*T_K), R_w = 461.5 J/(kg K), e in Pa, *1000 -> g/m3)

es_magnus <- function(t_c) 6.112 * exp(17.62 * t_c / (243.12 + t_c))
abs_feuchte <- function(t_c, rh) {
  e <- (rh / 100) * es_magnus(t_c)        # Dampfdruck [hPa]
  216.7 * e / (t_c + 273.15)              # absolute Feuchte [g/m3]
}

dat <- dat %>%
  mutate(
    e_s    = es_magnus(t_avg),
    e_akt  = (rh_avg / 100) * e_s,
    ah_avg = abs_feuchte(t_avg, rh_avg),
    ah_min = abs_feuchte(t_min, rh_min),   # konsistente Extrema
    ah_max = abs_feuchte(t_max, rh_max),
    datum  = as_date(zeit)
  )

# =====================================================================
# SCHRITT 3: Temperatur-Auswertung
# =====================================================================

temp_kennz <- dat %>% summarise(
  T_Mittel = round(mean(t_avg, na.rm = TRUE), 2),
  T_Min    = round(min(t_min,  na.rm = TRUE), 2),
  T_Max    = round(max(t_max,  na.rm = TRUE), 2)
)
cat("\n=== Temperatur-Kennzahlen (degC) ===\n"); print(temp_kennz)

# 3a) Gesamtverlauf Min/Avg/Max
p_temp_gesamt <- ggplot(dat, aes(x = zeit)) +
  markiere_luecke +
  geom_ribbon(aes(ymin = t_min, ymax = t_max, fill = "Min-Max-Spanne"), alpha = 0.25) +
  geom_line(aes(y = t_avg, colour = "Mittel"), linewidth = 0.6) +
  scale_colour_manual(values = c("Mittel" = col_temp)) +
  scale_fill_manual(values = c("Min-Max-Spanne" = col_temp)) +
  scale_x_datetime(date_breaks = "3 days", labels = label_date("%d.%m")) +
  labs(title = "Lufttemperatur - Gesamtzeitraum (20.05.-14.06.2026)",
       subtitle = "Messstation Horstmarer Landweg, 10-min-Werte",
       x = NULL, y = "Lufttemperatur (°C)", colour = NULL, fill = NULL)

ggsave_pp("01_temp_gesamt.png", p_temp_gesamt)

# 3b) Mittlerer Tagesgang Temperatur
tagesgang <- dat %>%
  mutate(stunde = hour(zeit) + minute(zeit) / 60) %>%
  group_by(stunde) %>%
  summarise(t = mean(t_avg, na.rm = TRUE),
            rh = mean(rh_avg, na.rm = TRUE),
            ah = mean(ah_avg, na.rm = TRUE), .groups = "drop")

p_temp_tagesgang <- ggplot(tagesgang, aes(stunde, t)) +
  geom_line(colour = col_temp, linewidth = 1.4) +
  scale_x_continuous(breaks = seq(0, 24, 3)) +
  labs(title = "Mittlerer Tagesgang der Lufttemperatur",
       x = "Uhrzeit (h)", y = "Lufttemperatur (°C)")

ggsave_pp("02_temp_tagesgang.png", p_temp_tagesgang, w = 9, h = 5.5)

# =====================================================================
# SCHRITT 4: Feuchte-Auswertung (relativ + absolut)
# =====================================================================

feuchte_kennz <- dat %>% summarise(
  RH_Mittel = round(mean(rh_avg, na.rm = TRUE), 1),
  RH_Min    = round(min(rh_min,  na.rm = TRUE), 1),
  RH_Max    = round(max(rh_max,  na.rm = TRUE), 1),
  AH_Mittel = round(mean(ah_avg, na.rm = TRUE), 2),
  AH_Min    = round(min(ah_min,  na.rm = TRUE), 2),
  AH_Max    = round(max(ah_max,  na.rm = TRUE), 2)
)
cat("\n=== Feuchte-Kennzahlen ===\n"); print(feuchte_kennz)

# 4a) Relative Feuchte Gesamtverlauf
p_rh_gesamt <- ggplot(dat, aes(x = zeit)) +
  markiere_luecke +
  geom_ribbon(aes(ymin = rh_min, ymax = rh_max, fill = "Min-Max-Spanne"), alpha = 0.25) +
  geom_line(aes(y = rh_avg, colour = "Mittel"), linewidth = 0.6) +
  scale_colour_manual(values = c("Mittel" = col_rh)) +
  scale_fill_manual(values = c("Min-Max-Spanne" = col_rh)) +
  scale_x_datetime(date_breaks = "3 days", labels = label_date("%d.%m")) +
  labs(title = "Relative Luftfeuchte - Gesamtzeitraum (20.05.-14.06.2026)",
       x = NULL, y = "Relative Feuchte (%)", colour = NULL, fill = NULL)

ggsave_pp("03_rh_gesamt.png", p_rh_gesamt)

# 4b) Absolute Feuchte Gesamtverlauf
p_ah_gesamt <- ggplot(dat, aes(x = zeit)) +
  markiere_luecke +
  geom_ribbon(aes(ymin = ah_min, ymax = ah_max, fill = "Min-Max-Spanne"), alpha = 0.25) +
  geom_line(aes(y = ah_avg, colour = "Mittel"), linewidth = 0.6) +
  scale_colour_manual(values = c("Mittel" = col_ah)) +
  scale_fill_manual(values = c("Min-Max-Spanne" = col_ah)) +
  scale_x_datetime(date_breaks = "3 days", labels = label_date("%d.%m")) +
  labs(title = "Absolute Luftfeuchte - Gesamtzeitraum (20.05.-14.06.2026)",
       subtitle = "berechnet aus Temperatur und rel. Feuchte (Magnus-Formel)",
       x = NULL, y = expression("Absolute Feuchte (g/m"^3*")"),
       colour = NULL, fill = NULL)

ggsave_pp("04_ah_gesamt.png", p_ah_gesamt)

# =====================================================================
# SCHRITT 5: Zusammenhang Temperatur <-> Feuchte
# =====================================================================

# Korrelationskoeffizienten (Pearson)
r_rh <- cor(dat$t_avg, dat$rh_avg, use = "complete.obs")
r_ah <- cor(dat$t_avg, dat$ah_avg, use = "complete.obs")
cat("\n=== Korrelationen ===\n")
cat("Temp vs. rel. Feuchte: r =", round(r_rh, 3), "\n")
cat("Temp vs. abs. Feuchte: r =", round(r_ah, 3), "\n")

# 5a) Gemeinsamer Verlauf Temp & rel. Feuchte (zweite Achse) -> Antikorrelation
faktor <- max(dat$rh_avg, na.rm = TRUE) / max(dat$t_avg, na.rm = TRUE)
p_temp_rh <- ggplot(dat, aes(x = zeit)) +
  markiere_luecke +
  geom_line(aes(y = t_avg, colour = "Temperatur"), linewidth = 0.6) +
  geom_line(aes(y = rh_avg / faktor, colour = "rel. Feuchte"), linewidth = 0.6) +
  scale_y_continuous(name = "Lufttemperatur (°C)",
                     sec.axis = sec_axis(~ . * faktor, name = "Relative Feuchte (%)")) +
  scale_colour_manual(values = c("Temperatur" = col_temp, "rel. Feuchte" = col_rh)) +
  scale_x_datetime(date_breaks = "3 days", labels = label_date("%d.%m")) +
  labs(title = "Temperatur und relative Feuchte im Vergleich",
       subtitle = "deutliche Antikorrelation im Tagesgang",
       x = NULL, colour = NULL)

ggsave_pp("05_temp_vs_rh_verlauf.png", p_temp_rh)

# 5b) Mittlerer Tagesgang Temp + rel. Feuchte gemeinsam
faktor2 <- max(tagesgang$rh, na.rm = TRUE) / max(tagesgang$t, na.rm = TRUE)
p_tagesgang_kombi <- ggplot(tagesgang, aes(x = stunde)) +
  geom_line(aes(y = t, colour = "Temperatur"), linewidth = 1.4) +
  geom_line(aes(y = rh / faktor2, colour = "rel. Feuchte"), linewidth = 1.4) +
  scale_y_continuous(name = "Lufttemperatur (°C)",
                     sec.axis = sec_axis(~ . * faktor2, name = "Relative Feuchte (%)")) +
  scale_colour_manual(values = c("Temperatur" = col_temp, "rel. Feuchte" = col_rh)) +
  scale_x_continuous(breaks = seq(0, 24, 3)) +
  labs(title = "Mittlerer Tagesgang: Temperatur vs. rel. Feuchte",
       x = "Uhrzeit (h)", colour = NULL)

ggsave_pp("06_tagesgang_temp_rh.png", p_tagesgang_kombi, w = 9, h = 5.5)

# 5c) KORRELATIONS-ABBILDUNG: Streudiagramme mit Regressionsgerade + r-Wert
beschr <- function(r) paste0("r = ", formatC(r, format = "f", digits = 2))

p_kor_rh <- ggplot(dat, aes(t_avg, rh_avg)) +
  geom_point(alpha = 0.12, size = 0.8, colour = col_rh) +
  geom_smooth(method = "lm", se = FALSE, colour = "black", linewidth = 1.3) +
  annotate("label", x = Inf, y = Inf, hjust = 1.1, vjust = 1.4,
           label = beschr(r_rh), size = 6, fontface = "bold") +
  labs(title = "Relative Feuchte vs. Temperatur",
       subtitle = "starke negative Korrelation",
       x = "Lufttemperatur (°C)", y = "Relative Feuchte (%)")

p_kor_ah <- ggplot(dat, aes(t_avg, ah_avg)) +
  geom_point(alpha = 0.12, size = 0.8, colour = col_ah) +
  geom_smooth(method = "lm", se = FALSE, colour = "black", linewidth = 1.3) +
  annotate("label", x = Inf, y = Inf, hjust = 1.1, vjust = 1.4,
           label = beschr(r_ah), size = 6, fontface = "bold") +
  labs(title = "Absolute Feuchte vs. Temperatur",
       subtitle = "praktisch kein linearer Zusammenhang",
       x = "Lufttemperatur (°C)", y = expression("Absolute Feuchte (g/m"^3*")"))

p_korrelation <- (p_kor_rh | p_kor_ah) +
  plot_annotation(
    title = "Abhängigkeit Temperatur - Feuchte",
    subtitle = "rel. Feuchte sinkt mit steigender Temperatur; abs. Feuchte ist davon weitgehend entkoppelt",
    theme = theme(plot.title = element_text(face = "bold", size = 22),
                  plot.subtitle = element_text(size = 14, colour = "grey30")))

ggsave_pp("07_korrelation_temp_feuchte.png", p_korrelation, w = 13, h = 6.5)

# =====================================================================
# SCHRITT 6: Interessante Ereignisse + Strahlung & Niederschlag
# =====================================================================
# Auswahl bewusst kontrastierend und meteorologisch begruendet:
#  (A) Strahlungsreicher Schoenwettertag  -> Strahlung als Antrieb des Tagesgangs
#  (B) Niederschlagsereignis/Frontdurchgang -> Bewoelkung/Regen daempfen Tagesgang
# Der letzte Messtag (fehlerhafte Strahlung) wird ausgeschlossen.
sauber <- dat %>% filter(datum < as_date("2026-06-13"))

tagesstat <- sauber %>%
  group_by(datum) %>%
  summarise(t_tagesmax = max(t_max, na.rm = TRUE),
            regen_summe = sum(niederschlag, na.rm = TRUE),
            sw_summe    = sum(sw_up, na.rm = TRUE), .groups = "drop")

# (A) Strahlungstag: hoechste Tageseinstrahlung unter den NIEDERSCHLAGSFREIEN Tagen
tag_strahlung <- tagesstat %>%
  filter(regen_summe == 0) %>%
  slice_max(sw_summe, n = 1) %>% pull(datum)

# (B) Regenereignis: groesste Niederschlagssumme
tag_regen <- tagesstat %>% slice_max(regen_summe, n = 1) %>% pull(datum)

cat("\n=== Interessante Tage (begruendet) ===\n")
cat("Strahlungsreicher Schoenwettertag:", format(tag_strahlung),
    "(Einstrahlungssumme max., 0 mm Niederschlag)\n")
cat("Niederschlagsereignis:", format(tag_regen),
    "(Summe =", round(max(tagesstat$regen_summe), 1), "mm)\n")

# Hilfsfunktion: Mehrfachpanel fuer ein Zeitfenster
plot_ereignis <- function(daten, start, ende, titel, erklaerung, datei) {
  d <- daten %>% filter(zeit >= start & zeit <= ende)
  f <- max(d$rh_avg, na.rm = TRUE) / max(d$ah_avg, na.rm = TRUE)  # dyn. Skalierung
  x_scale <- scale_x_datetime(date_breaks = "6 hours",
                              labels = label_date("%d.%m\n%H:%M"),
                              limits = c(start, ende))

  p1 <- ggplot(d, aes(zeit, t_avg)) +
    geom_line(colour = col_temp, linewidth = 1.1) +
    x_scale + labs(y = "T (°C)", x = NULL)

  p2 <- ggplot(d, aes(zeit)) +
    geom_line(aes(y = rh_avg, colour = "rel. Feuchte (%)"), linewidth = 1.1) +
    geom_line(aes(y = ah_avg * f, colour = "abs. Feuchte (g/m³)"), linewidth = 1.1) +
    scale_y_continuous(name = "rel. Feuchte (%)",
                       sec.axis = sec_axis(~ . / f, name = "abs. Feuchte (g/m³)")) +
    scale_colour_manual(values = c("rel. Feuchte (%)" = col_rh,
                                   "abs. Feuchte (g/m³)" = col_ah)) +
    x_scale + labs(x = NULL, colour = NULL)

  p3 <- ggplot(d, aes(zeit, sw_up)) +
    geom_line(colour = col_sw, linewidth = 1.1) +
    x_scale + labs(y = "kurzw.\nStrahlung\n(W/m²)", x = NULL)

  p4 <- ggplot(d, aes(zeit, niederschlag)) +
    geom_col(fill = col_rh, width = 600) +
    x_scale + labs(y = "Nieder-\nschlag\n(mm)", x = NULL)

  kombi <- (p1 / p2 / p3 / p4) +
    plot_annotation(
      title = titel, subtitle = erklaerung,
      theme = theme(plot.title = element_text(face = "bold", size = 20),
                    plot.subtitle = element_text(size = 14, colour = "grey30")))
  ggsave(file.path(fig_dir, datei), kombi, width = 10, height = 11,
         dpi = 300, bg = "white")
}

# 6a) Strahlungsreicher Schoenwettertag (+/- 1 Tag Kontext)
plot_ereignis(
  sauber,
  start = as_datetime(paste(tag_strahlung - 1, "00:00:00"), tz = "UTC"),
  ende  = as_datetime(paste(tag_strahlung + 1, "00:00:00"), tz = "UTC"),
  titel = paste0("Strahlungsreicher Schönwettertag (", format(tag_strahlung, "%d.%m.%Y"), ")"),
  erklaerung = paste("Hohe Einstrahlung treibt den Tagesgang: T-Maximum am",
                     "Mittag, rel. Feuchte synchron im Minimum - kein Niederschlag."),
  datei = "08_ereignis_strahlungstag.png"
)

# 6b) Niederschlagsereignis / Frontdurchgang (+/- 1 Tag Kontext)
plot_ereignis(
  sauber,
  start = as_datetime(paste(tag_regen - 1, "00:00:00"), tz = "UTC"),
  ende  = as_datetime(paste(tag_regen + 1, "00:00:00"), tz = "UTC"),
  titel = paste0("Niederschlagsereignis (", format(tag_regen, "%d.%m.%Y"), ")"),
  erklaerung = paste("Bewölkung bricht die Einstrahlung ein, Regen sättigt die Luft:",
                     "rel. Feuchte nahe 100 %, gedämpfter Temperaturgang."),
  datei = "09_ereignis_regen.png"
)

# =====================================================================
# SCHRITT 7: Abschluss / Export-Uebersicht
# =====================================================================
write.csv2(dat, file.path("output", "aufbereitete_daten.csv"), row.names = FALSE)

cat("\n=== Fertig ===\n")
cat("Grafiken gespeichert in:", normalizePath(fig_dir), "\n")
print(list.files(fig_dir))
