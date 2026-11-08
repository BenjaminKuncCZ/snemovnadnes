# ── 12_stats.R ────────────────────────────────────────────────────────────────
# "Statistiky" tab: summary statistics and charts for the whole dataset.
# Needs: shiny, plotly, dplyr, DBI, and get_all_bills() from 04_database.R.
# Wiring into app.R: see the bottom of this file.

library(shiny)
library(plotly)
library(dplyr)
library(DBI)

# ── Palette (matches app.R CSS variables) ─────────────────────────────────────
ST_INK     <- "#1a1a2e"
ST_RED     <- "#c0392b"
ST_GREEN   <- "#2e7d52"
ST_YELLOW  <- "#d4a017"
ST_ORANGE  <- "#e65100"
ST_GREY    <- "#b8b2a6"
ST_MUTED   <- "#7f8c8d"

# ── Subject normalisation ─────────────────────────────────────────────────────
# `subjekt` is free text produced by the LLM, so map it to a fixed set of
# categories (first matching pattern wins); anything else becomes "jiné".
ST_SUBJECT_PATTERNS <- c(
  "státní rozpočet"   = "rozpočt|státní|^stát\\b",
  "obce"              = "obc|kraj|samospráv",
  "podnikatelé"       = "podnikat|firm|podnik|msp",
  "domácnosti"        = "domácnost|občan|obyvatel|rodin",
  "zaměstnavatelé"    = "zaměstnavatel",
  "zaměstnanci"       = "zaměstnanc",
  "spotřebitelé"      = "spotřebitel",
  "životní prostředí" = "životní|prostřed|ekosyst"
)

normalise_subject <- function(x) {
  x   <- tolower(trimws(ifelse(is.na(x), "", x)))
  out <- rep("jiné", length(x))
  done <- rep(FALSE, length(x))
  for (nm in names(ST_SUBJECT_PATTERNS)) {
    hit <- !done & grepl(ST_SUBJECT_PATTERNS[[nm]], x, perl = TRUE)
    out[hit]  <- nm
    done[hit] <- TRUE
  }
  out
}

# ── Data ──────────────────────────────────────────────────────────────────────
# Returns a list of ready-to-plot data frames. `bills` = get_all_bills(db).
load_stats <- function(db, bills) {

  bills <- bills |>
    mutate(
      doc_group = case_when(
        typ_dokumentu == "RIA"            ~ "Závěrečná zpráva RIA",
        typ_dokumentu == "prehled_dopadu" ~ "Přehled dopadů / důvodová zpráva",
        TRUE                              ~ "Bez hodnocení dopadů"
      ),
      # `predkladatel` is filled only for ministry-level documents (vyhlášky,
      # nařízení, ...). Parliamentary bills have it empty, so derive the
      # proposer from the title prefix.
      resort = case_when(
        !is.na(predkladatel) & predkladatel != ""                    ~ predkladatel,
        grepl("^Vládní návrh", title, ignore.case = TRUE)            ~ "Vláda (vládní návrhy)",
        grepl("^Návrh poslan|^Poslanecký návrh", title, ignore.case = TRUE) ~ "Poslanci",
        grepl("^Senátní návrh|^Návrh senát", title, ignore.case = TRUE)     ~ "Senát",
        grepl("^Návrh zastupitelstva", title, ignore.case = TRUE)    ~ "Zastupitelstva krajů",
        TRUE                                                          ~ "Ostatní (rozpočty, plány)"
      ),
      has_komise = !is.na(komise_verdict) & komise_verdict != ""
    )

  costs <- dbGetQuery(db, "SELECT DISTINCT pid, subjekt FROM costs")
  bens  <- dbGetQuery(db, "SELECT DISTINCT pid, subjekt FROM benefits")

  by_subject <- function(d) {
    if (nrow(d) == 0) return(data.frame(subject = character(), n = integer()))
    d |>
      filter(pid %in% bills$pid) |>
      # some values list several groups ("Domácnosti, podnikatelé", "a / b")
      tidyr::separate_rows(subjekt, sep = "\\s*[,/;]\\s*") |>
      mutate(subject = normalise_subject(subjekt)) |>
      distinct(pid, subject) |>          # count each bill once per subject
      count(subject, name = "n")
  }

  subj_costs <- by_subject(costs) |> rename(costs = n)
  subj_bens  <- by_subject(bens)  |> rename(benefits = n)
  subjects <- full_join(subj_costs, subj_bens, by = "subject") |>
    mutate(across(c(costs, benefits), ~ coalesce(.x, 0L)))

  ria <- bills |> filter(doc_group == "Závěrečná zpráva RIA")

  list(
    bills      = bills,
    n_bills    = nrow(bills),
    n_assessed = sum(bills$doc_group != "Bez hodnocení dopadů"),
    n_ria      = nrow(ria),
    n_ria_komise = sum(ria$has_komise),
    n_komise   = sum(bills$has_komise),
    n_costed   = length(unique(costs$pid[costs$pid %in% bills$pid])),
    subjects   = subjects
  )
}

# ── Plot helpers ──────────────────────────────────────────────────────────────

st_layout <- function(p, ...) {
  p |>
    layout(
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)",
      font   = list(family = "Source Sans 3, sans-serif", color = ST_INK, size = 14),
      margin = list(l = 10, r = 30, t = 10, b = 10),
      showlegend = FALSE,
      ...
    ) |>
    config(displayModeBar = FALSE)
}

st_empty <- function(msg = "Zatím nejsou k dispozici žádná data.") {
  plot_ly() |>
    layout(
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
      annotations = list(list(text = msg, showarrow = FALSE,
                              font = list(color = ST_MUTED, size = 15)))
    ) |>
    config(displayModeBar = FALSE)
}

# Horizontal bar chart, largest at top.
st_hbar <- function(labels, values, color = ST_INK) {
  if (length(values) == 0 || sum(values) == 0) return(st_empty())
  o <- order(values)
  labels <- labels[o]; values <- values[o]
  plot_ly(
    x = values, y = factor(labels, levels = labels),
    type = "bar", orientation = "h",
    marker = list(color = color),
    text = values, textposition = "outside", cliponaxis = FALSE,
    hovertemplate = "%{y}: %{x}<extra></extra>"
  ) |>
    st_layout(
      xaxis = list(visible = FALSE),
      yaxis = list(title = "", automargin = TRUE, tickfont = list(size = 14))
    )
}

# ── UI ────────────────────────────────────────────────────────────────────────

stat_card <- function(title, subtitle = NULL, output) {
  div(class = "stat-card",
      div(class = "stat-card-title", title),
      if (!is.null(subtitle)) div(class = "stat-card-sub", subtitle),
      plotlyOutput(output, height = "320px")
  )
}

stats_ui <- function() {
  div(class = "stats-view", style = "display:none;",
      div(class = "stats-inner",
          div(class = "intro-eyebrow", style = "font-size:1.1rem;", "Přehled dat"),
          h2(class = "stats-h2", "Statistiky datasetu"),
          uiOutput("st_kpis"),

          div(class = "stats-grid",
              stat_card("Kdo materiály předkládá",
                        "Počet materiálů podle předkladatele", "st_resorts"),
              stat_card("Jak často je provedena RIA",
                        "Typ hodnocení dopadů u všech materiálů", "st_doctypes"),
              stat_card("Kdo nese náklady a kdo má přínosy",
                        "Počet materiálů, u kterých je daný subjekt uveden",
                        "st_subjects"),
              stat_card("Hodnocení dopadů podle předkladatele",
                        "Podíl materiálů s RIA, přehledem dopadů a bez hodnocení",
                        "st_resort_docs"),
              stat_card("Komise RIA: pokrytí",
                        "Kolik materiálů se ZZ RIA Komise již posoudila", "st_komise_cov"),
              stat_card("Komise RIA: známky",
                        "A doporučeno · B s připomínkami · C přepracovat · D nedoporučeno",
                        "st_verdicts")
          ),

          div(class = "stats-note",
              "Subjekty jsou přiřazeny z textu RIA jazykovým modelem a sloučeny do osmi kategorií ",
              "(ostatní jako „jiné“); každý materiál se v dané kategorii počítá jednou. ",
              "Komise RIA posuzuje jen část materiálů, proto je pokrytí neúplné.")
      )
  )
}

# ── Server ────────────────────────────────────────────────────────────────────
# `bills` is the reactive from app.R (get_all_bills(db)).

stats_server <- function(input, output, db, bills) {

  # compute only while the Statistiky view is open (charts then get a real width)
  st <- reactive({
    req(identical(input$active_view, "stats"))
    b <- bills()
    req(nrow(b) > 0)
    load_stats(db, b)
  })

  pct <- function(a, b) if (b == 0) "–" else paste0(round(100 * a / b), " %")

  output$st_kpis <- renderUI({
    s <- st()
    kpi <- function(value, label, sub = NULL) {
      div(class = "kpi",
          div(class = "kpi-value", value),
          div(class = "kpi-label", label),
          if (!is.null(sub)) div(class = "kpi-sub", sub))
    }
    div(class = "kpi-row",
        kpi(s$n_bills, "materiálů v datasetu"),
        kpi(s$n_assessed, "s hodnocením dopadů", pct(s$n_assessed, s$n_bills)),
        kpi(s$n_ria, "se Závěrečnou zprávou RIA", pct(s$n_ria, s$n_bills)),
        kpi(s$n_costed, "s vyčleněnými náklady", pct(s$n_costed, s$n_bills)),
        kpi(s$n_ria_komise, "RIA posouzených Komisí", pct(s$n_ria_komise, s$n_ria))
    )
  })

  output$st_resorts <- renderPlotly({
    d <- st()$bills |> count(resort, name = "n") |> arrange(desc(n))
    st_hbar(d$resort, d$n, ST_INK)
  })

  output$st_doctypes <- renderPlotly({
    d <- st()$bills |> count(doc_group, name = "n")
    if (nrow(d) == 0) return(st_empty())
    cols <- c("Závěrečná zpráva RIA" = ST_GREEN,
              "Přehled dopadů / důvodová zpráva" = ST_YELLOW,
              "Bez hodnocení dopadů" = ST_GREY)
    plot_ly(d, labels = ~doc_group, values = ~n, type = "pie", hole = 0.6,
            marker = list(colors = unname(cols[d$doc_group]),
                          line = list(color = "#f5f0e8", width = 2)),
            textinfo = "value", sort = FALSE,
            hovertemplate = "%{label}: %{value} (%{percent})<extra></extra>") |>
      st_layout(showlegend = TRUE,
                legend = list(orientation = "h", y = -0.05, font = list(size = 13)))
  })

  output$st_subjects <- renderPlotly({
    d <- st()$subjects
    if (nrow(d) == 0) return(st_empty())
    d <- d |> arrange(costs + benefits)
    lv <- d$subject
    plot_ly(d, y = ~factor(subject, levels = lv)) |>
      add_bars(x = ~costs, name = "Náklady", orientation = "h",
               marker = list(color = ST_RED),
               hovertemplate = "%{y} – náklady: %{x}<extra></extra>") |>
      add_bars(x = ~benefits, name = "Přínosy", orientation = "h",
               marker = list(color = ST_GREEN),
               hovertemplate = "%{y} – přínosy: %{x}<extra></extra>") |>
      st_layout(barmode = "group", showlegend = TRUE,
                legend = list(orientation = "h", y = 1.08, x = 0),
                xaxis = list(title = "", showgrid = TRUE, gridcolor = "#e6e0d4",
                             zeroline = FALSE),
                yaxis = list(title = "", automargin = TRUE))
  })

  output$st_resort_docs <- renderPlotly({
    d <- st()$bills |> count(resort, doc_group, name = "n")
    if (nrow(d) == 0) return(st_empty())
    order_r <- d |> group_by(resort) |> summarise(t = sum(n)) |> arrange(t)
    wide <- d |>
      tidyr::pivot_wider(names_from = doc_group, values_from = n, values_fill = 0L)
    for (col in c("Závěrečná zpráva RIA", "Přehled dopadů / důvodová zpráva",
                  "Bez hodnocení dopadů"))
      if (!col %in% names(wide)) wide[[col]] <- 0L
    wide <- wide[match(order_r$resort, wide$resort), ]
    y <- factor(wide$resort, levels = order_r$resort)
    plot_ly(y = y, orientation = "h") |>
      add_bars(x = wide[["Závěrečná zpráva RIA"]], name = "RIA",
               marker = list(color = ST_GREEN)) |>
      add_bars(x = wide[["Přehled dopadů / důvodová zpráva"]], name = "Přehled",
               marker = list(color = ST_YELLOW)) |>
      add_bars(x = wide[["Bez hodnocení dopadů"]], name = "Bez hodnocení",
               marker = list(color = ST_GREY)) |>
      st_layout(barmode = "stack", showlegend = TRUE,
                legend = list(orientation = "h", y = 1.08, x = 0),
                xaxis = list(title = "", showgrid = TRUE, gridcolor = "#e6e0d4",
                             zeroline = FALSE),
                yaxis = list(title = "", automargin = TRUE))
  })

  output$st_komise_cov <- renderPlotly({
    s <- st()
    if (s$n_ria == 0) return(st_empty())
    labels <- c("Zatím neposouzeno", "Posouzeno Komisí")
    values <- c(s$n_ria - s$n_ria_komise, s$n_ria_komise)
    plot_ly(x = values, y = factor(labels, levels = labels),
            type = "bar", orientation = "h",
            marker = list(color = c(ST_GREY, ST_INK)),
            text = values, textposition = "outside", cliponaxis = FALSE,
            hovertemplate = "%{y}: %{x}<extra></extra>") |>
      st_layout(xaxis = list(visible = FALSE),
                yaxis = list(title = "", automargin = TRUE))
  })

  output$st_verdicts <- renderPlotly({
    b <- st()$bills
    lv <- c("A", "B", "C", "D")
    n  <- sapply(lv, function(v) sum(b$komise_verdict == v, na.rm = TRUE))
    if (sum(n) == 0) return(st_empty("Zatím žádná stanoviska Komise RIA."))
    plot_ly(x = lv, y = unname(n), type = "bar",
            marker = list(color = c(ST_GREEN, ST_YELLOW, ST_ORANGE, ST_RED)),
            text = unname(n), textposition = "outside", cliponaxis = FALSE,
            hovertemplate = "Známka %{x}: %{y}<extra></extra>") |>
      st_layout(xaxis = list(title = "", tickfont = list(size = 18)),
                yaxis = list(visible = FALSE))
  })

  for (id in c("st_kpis", "st_resorts", "st_doctypes", "st_subjects",
               "st_resort_docs", "st_komise_cov", "st_verdicts"))
    outputOptions(output, id, suspendWhenHidden = FALSE)
}


# ── CSS (injected in <head> of app.R; view switching reuses app.R's setView()) ─

STATS_CSS <- r"(
.stats-view { display: none; }
body.view-stats .app-body { display: none; }
body.view-stats .stats-view {
  display: block; height: calc(100vh - var(--header-h)); overflow-y: auto;
  background: var(--paper);
}
.stats-inner { max-width: 1180px; margin: 0 auto; padding: 32px 32px 64px; }
.stats-h2 {
  font-family: 'Playfair Display', serif; font-weight: 900;
  font-size: 2.4rem; margin: 4px 0 24px;
}
.kpi-row {
  display: grid; grid-template-columns: repeat(5, 1fr); gap: 14px; margin-bottom: 28px;
}
.kpi {
  background: white; border: 1px solid var(--border); border-top: 3px solid var(--ink);
  padding: 16px 18px;
}
.kpi-value {
  font-family: 'Playfair Display', serif; font-size: 2.4rem; font-weight: 900; line-height: 1;
}
.kpi-label { font-size: 1rem; color: var(--muted); margin-top: 8px; line-height: 1.3; }
.kpi-sub   { font-size: 0.95rem; font-weight: 600; color: var(--accent); margin-top: 4px; }
.stats-grid { display: grid; grid-template-columns: 1fr 1fr; gap: 18px; }
.stat-card  { background: white; border: 1px solid var(--border); padding: 18px 20px 8px; }
.stat-card-title {
  font-size: 0.95rem; font-weight: 600; text-transform: uppercase;
  letter-spacing: 1.5px; color: var(--ink);
}
.stat-card-sub { font-size: 0.95rem; color: var(--muted); margin: 4px 0 8px; }
.stats-note {
  font-size: 0.9rem; color: var(--muted); line-height: 1.6; margin-top: 24px;
  border-top: 1px solid var(--border); padding-top: 14px; max-width: 760px;
}
@media (max-width: 900px) {
  .kpi-row { grid-template-columns: 1fr 1fr; }
  .stats-grid { grid-template-columns: 1fr; }
  .stats-inner { padding: 20px 16px 48px; }
  .stats-h2 { font-size: 1.7rem; }
}
)"
