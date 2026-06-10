library(shiny)
library(plotly)
library(DBI)
library(RSQLite)
library(dplyr)
library(stringr)

source("R/04_database.R")
source("R/10_alerts.R")

# ── CSS ───────────────────────────────────────────────────────────────────────

app_css <- "
@import url('https://fonts.googleapis.com/css2?family=Playfair+Display:wght@700;900&family=Source+Sans+3:wght@300;400;600&family=Source+Code+Pro:wght@400;600&display=swap');

:root {
  --ink:       #1a1a2e;
  --paper:     #f5f0e8;
  --accent:    #c0392b;
  --muted:     #7f8c8d;
  --border:    #d5cfc4;
  --cost-bar:  #c0392b;
  --benefit:   #2e7d52;
  --warn:      #d4a017;
  --header-h:  90px;
}

* { box-sizing: border-box; margin: 0; padding: 0; }

body {
  background: var(--paper);
  color: var(--ink);
  font-family: 'Source Sans 3', sans-serif;
  font-weight: 300;
  font-size: 18px;
}

/* ── Header ── */
.app-header {
  background: var(--ink);
  color: var(--paper);
  padding: 18px 48px;
  border-bottom: 4px solid var(--accent);
  height: var(--header-h);
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 24px;
}

.header-left { flex: 1; min-width: 0; }

.app-header h1 {
  font-family: 'Playfair Display', serif;
  font-size: 2.6rem;
  font-weight: 900;
  letter-spacing: -1px;
  line-height: 1;
  cursor: pointer;
  transition: opacity 0.15s;
}

.app-header h1:hover { opacity: 0.8; }

.app-header .subtitle {
  font-size: 0.85rem;
  font-weight: 300;
  color: #a0a0b0;
  margin-top: 6px;
  letter-spacing: 0.5px;
  text-transform: uppercase;
}

.header-website-btn {
  flex-shrink: 0;
  display: inline-block;
  padding: 8px 18px;
  border: 1px solid rgba(255,255,255,0.25);
  color: var(--paper);
  text-decoration: none;
  font-size: 0.85rem;
  font-weight: 600;
  letter-spacing: 1px;
  text-transform: uppercase;
  transition: background 0.15s, border-color 0.15s;
  white-space: nowrap;
}

.header-website-btn:hover {
  background: rgba(255,255,255,0.1);
  border-color: rgba(255,255,255,0.5);
}

/* ── Welcome / intro screen ── */
.intro-panel {
  max-width: 660px;
  padding: 16px 0 48px;
}

.intro-panel .intro-eyebrow {
  font-size: 1.6rem;
  font-weight: 600;
  text-transform: uppercase;
  letter-spacing: 2px;
  color: var(--accent);
  margin-bottom: 16px;
}

.intro-panel h2 {
  font-family: 'Playfair Display', serif;
  font-size: 4.4rem;
  font-weight: 900;
  line-height: 1.25;
  color: var(--ink);
  margin-bottom: 20px;
}

.intro-panel .intro-lead {
  font-size: 2.4rem;
  font-weight: 300;
  line-height: 1.8;
  color: #3a3a4a;
  margin-bottom: 32px;
  border-left: 3px solid var(--accent);
  padding-left: 20px;
}

.intro-cards {
  display: grid;
  grid-template-columns: 1fr 1fr;
  gap: 16px;
  margin-bottom: 36px;
}

.intro-card {
  background: white;
  border: 1px solid var(--border);
  border-top: 3px solid var(--ink);
  padding: 18px 20px;
}

.intro-card .card-icon {
  font-size: 1.4rem;
  margin-bottom: 8px;
}

.intro-card .card-title {
  font-size: 1.7rem;
  font-weight: 600;
  text-transform: uppercase;
  letter-spacing: 1px;
  color: var(--muted);
  margin-bottom: 6px;
}

.intro-card .card-body {
  font-size: 1.9rem;
  line-height: 1.6;
  color: var(--ink);
}

.intro-how {
  background: var(--ink);
  color: var(--paper);
  padding: 20px 24px;
  font-size: 2rem;
  line-height: 1.7;
}

.intro-how strong {
  color: #f0b8b0;
}

/* ── Layout ── */
.app-body {
  display: flex;
  height: calc(100vh - var(--header-h));
  overflow: hidden;
  position: relative;
}

.sidebar {
  width: 380px;
  min-width: 380px;
  background: white;
  border-right: 1px solid var(--border);
  padding: 24px 0;
  overflow-y: auto;
  height: 100%;
  transition: transform 0.28s cubic-bezier(0.4, 0, 0.2, 1);
}

.main-panel {
  flex: 1;
  padding: 36px 48px;
  overflow-y: auto;
  height: 100%;
  transition: transform 0.28s cubic-bezier(0.4, 0, 0.2, 1);
}

/* ── Back button (hidden on desktop) ── */
.back-btn {
  display: none;
  align-items: center;
  gap: 8px;
  margin-bottom: 20px;
  padding: 8px 0;
  font-size: 0.85rem;
  font-weight: 600;
  color: var(--muted);
  cursor: pointer;
  text-transform: uppercase;
  letter-spacing: 1.5px;
  background: none;
  border: none;
  border-bottom: 1px solid var(--border);
  width: 100%;
  text-align: left;
}

.back-btn:hover { color: var(--ink); }

/* ── Sidebar bill list ── */
.sidebar-label {
  font-size: 1rem;
  font-weight: 600;
  text-transform: uppercase;
  letter-spacing: 1.5px;
  color: var(--muted);
  padding: 0 24px 12px;
  border-bottom: 1px solid var(--border);
  margin-bottom: 8px;
}

.bill-item {
  padding: 14px 24px;
  cursor: pointer;
  border-left: 3px solid transparent;
  transition: all 0.15s;
  border-bottom: 1px solid var(--border);
}

.bill-item:hover {
  background: var(--paper);
  border-left-color: var(--muted);
}

.bill-item.active {
  background: var(--paper);
  border-left-color: var(--accent);
}

.bill-item .bill-title {
  font-size: 1.05rem;
  font-weight: 600;
  line-height: 1.4;
  margin-bottom: 6px;
}

.bill-item .bill-meta {
  font-size: 0.9rem;
  color: var(--muted);
  display: flex;
  gap: 10px;
  align-items: center;
  flex-wrap: wrap;
}

.pill {
  display: inline-block;
  padding: 2px 8px;
  border-radius: 2px;
  font-size: 1rem;
  font-weight: 600;
  text-transform: uppercase;
  letter-spacing: 0.5px;
}

.pill-ria    { background: #e8f5e9; color: #2e7d32; }
.pill-noria  { background: #fff3e0; color: #e65100; }
.pill-none   { background: #eeeeee; color: #757575; }

/* ── Main content ── */
.bill-header {
  margin-bottom: 24px;
  padding-bottom: 12px;
  border-bottom: 2px solid var(--ink);
}

.bill-header .predkladatel {
  font-size: 1.5rem;
  font-weight: 600;
  text-transform: uppercase;
  letter-spacing: 2px;
  color: var(--accent);
  margin-bottom: 10px;
}

.bill-header h2 {
  font-family: 'Playfair Display', serif;
  font-size: 2rem;
  font-weight: 700;
  line-height: 1.3;
  margin-bottom: 14px;
}

.bill-header .shrnutí {
  font-size: 1.3rem;
  font-weight: 300;
  line-height: 1.7;
  color: #3a3a4a;
  max-width: 680px;
}

/* ── Status badges ── */
.status-row {
  display: flex;
  gap: 12px;
  margin-bottom: 32px;
  flex-wrap: wrap;
}

.status-badge {
  display: flex;
  align-items: center;
  gap: 8px;
  padding: 8px 16px;
  border: 1px solid var(--border);
  background: white;
  font-size: 1.5rem;
}

.status-badge .dot {
  width: 8px; height: 8px;
  border-radius: 50%;
  flex-shrink: 0;
}

.dot-green  { background: var(--benefit); }
.dot-red    { background: var(--accent); }
.dot-yellow { background: var(--warn); }
.dot-grey   { background: var(--muted); }

/* ── Section headings ── */
.section-heading {
  font-size: 1.5rem;
  font-weight: 600;
  text-transform: uppercase;
  letter-spacing: 2px;
  color: var(--muted);
  margin-bottom: 16px;
  margin-top: 32px;
  display: flex;
  align-items: center;
  gap: 12px;
}

.section-heading::after {
  content: '';
  flex: 1;
  height: 1px;
  background: var(--border);
}

/* ── Cost table ── */
.cost-row {
  display: flex;
  align-items: flex-start;
  padding: 14px 0;
  border-bottom: 1px solid var(--border);
  gap: 16px;
}

.cost-row:last-child { border-bottom: none; }

.cost-subjekt {
  flex: 0 0 220px;
  font-size: 1.5rem;
  font-weight: 600;
}

.cost-bar-wrap {
  flex: 1;
  display: flex;
  flex-direction: column;
  gap: 4px;
}

.cost-bar-outer {
  height: 6px;
  background: var(--border);
  border-radius: 2px;
  overflow: hidden;
}

.cost-bar-inner {
  height: 100%;
  background: var(--cost-bar);
  border-radius: 2px;
  transition: width 0.6s cubic-bezier(0.4,0,0.2,1);
}

.cost-amount {
  font-family: 'Source Code Pro', monospace;
  font-size: 1.5rem;
  font-weight: 600;
  color: var(--accent);
}

.cost-popis {
  font-size: 1.5rem;
  color: var(--muted);
  line-height: 1.5;
}

.cost-tags {
  display: flex;
  gap: 6px;
  margin-top: 4px;
  flex-wrap: wrap;
}

.cost-tag {
  font-size: 1.5rem;
  padding: 4px 12px;
  background: var(--paper);
  border: 1px solid var(--border);
  color: var(--muted);
  border-radius: 2px;
}

/* ── Benefits ── */
.benefit-item {
  padding: 12px 16px;
  background: #f0f7f3;
  border-left: 3px solid var(--benefit);
  margin-bottom: 8px;
  font-size: 1.5rem;
  line-height: 1.6;
}

/* ── Empty state ── */
.empty-state {
  text-align: center;
  padding: 80px 40px;
  color: var(--muted);
}

.empty-state .icon {
  font-size: 3rem;
  margin-bottom: 16px;
  opacity: 0.4;
}

.empty-state p {
  font-size: 1.5rem;
  line-height: 1.7;
}

/* ── No selection ── */
.no-selection {
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  height: 60vh;
  color: var(--muted);
  text-align: center;
}

.no-selection h3 {
  font-family: 'Playfair Display', serif;
  font-size: 1.4rem;
  font-weight: 700;
  margin-bottom: 10px;
  color: var(--ink);
  opacity: 0.4;
}

.no-selection p { font-size: 1.4rem; opacity: 0.6; }

/* ════════════════════════════════════════════
   MOBILE  (≤ 768px)
   Panels stack — sidebar fills screen, detail
   slides in from the right when a bill is picked.
   ════════════════════════════════════════════ */
@media (max-width: 768px) {

  :root { --header-h: 68px; }

  .app-header {
    padding: 12px 16px;
    gap: 12px;
  }

  .app-header h1 {
    font-size: 1.4rem;
    letter-spacing: -0.5px;
  }

  .app-header .subtitle {
    display: none;
  }

  .header-website-btn {
    font-size: 0.75rem;
    padding: 6px 10px;
    letter-spacing: 0.5px;
  }

  .intro-cards {
    grid-template-columns: 1fr;
  }

  .intro-panel h2 {
    font-size: 1.5rem;
  }

  .intro-panel .intro-lead {
    font-size: 1rem;
  }

  /* Both panels fill the viewport; JS toggles .detail-open on app-body */
  .app-body {
    overflow: hidden;
  }

  .sidebar {
    position: absolute;
    top: 0; left: 0;
    width: 100%;
    min-width: unset;
    height: 100%;
    border-right: none;
    padding: 16px 0;
    transform: translateX(0);
    z-index: 2;
  }

  .main-panel {
    position: absolute;
    top: 0; left: 0;
    width: 100%;
    height: 100%;
    padding: 20px 18px 32px;
    transform: translateX(100%);
    z-index: 3;
    background: var(--paper);
  }

  /* When a bill is selected, slide sidebar out and panel in */
  .app-body.detail-open .sidebar {
    transform: translateX(-100%);
  }

  .app-body.detail-open .main-panel {
    transform: translateX(0);
  }

  /* Show back button on mobile */
  .back-btn {
    display: flex;
  }

  /* Shrink fonts */
  .status-badge {
    font-size: 1rem;
    padding: 6px 10px;
  }

  .bill-header .predkladatel {
    font-size: 1rem;
    letter-spacing: 1.5px;
  }

  .bill-header h2 {
    font-size: 1.3rem;
  }

  .bill-header .shrnutí {
    font-size: 1rem;
  }

  .section-heading {
    font-size: 1rem;
    letter-spacing: 1.5px;
    margin-top: 24px;
  }

  /* Stack cost rows vertically on narrow screens */
  .cost-row {
    flex-direction: column;
    gap: 6px;
    padding: 12px 0;
  }

  .cost-subjekt {
    flex: unset;
    font-size: 1rem;
  }

  .cost-amount {
    font-size: 1rem;
  }

  .cost-popis {
    font-size: 0.95rem;
  }

  .cost-tag {
    font-size: 0.85rem;
    padding: 3px 8px;
  }

  .empty-state {
    padding: 40px 20px;
  }

  .empty-state p {
    font-size: 1rem;
  }

  /* Sidebar filters compact */
  .sidebar-label {
    font-size: 0.85rem;
    padding: 0 16px 10px;
  }

  .bill-item {
    padding: 12px 16px;
  }

  .bill-item .bill-title {
    font-size: 0.95rem;
  }

  .bill-item .bill-meta {
    font-size: 0.8rem;
  }
}

/* ── Alert modal ── */
.alert-form label {
  display: block;
  font-size: 0.82rem;
  font-weight: 600;
  text-transform: uppercase;
  letter-spacing: 1px;
  color: var(--muted);
  margin-bottom: 6px;
}
.alert-form input[type='email'],
.alert-form input[type='text'],
.alert-form select {
  width: 100%;
  padding: 9px 12px;
  border: 1px solid var(--border);
  font-size: 1rem;
  font-family: 'Source Sans 3', sans-serif;
  background: var(--paper);
  color: var(--ink);
  outline: none;
  box-sizing: border-box;
}
.alert-form .form-group { margin-bottom: 18px; }
.alert-note {
  font-size: 0.82rem;
  color: var(--muted);
  margin-top: 16px;
  line-height: 1.6;
  border-top: 1px solid var(--border);
  padding-top: 14px;
}
.btn-alert {
  background: var(--ink);
  color: var(--paper);
  border: none;
  padding: 10px 24px;
  font-size: 1rem;
  font-family: 'Source Sans 3', sans-serif;
  font-weight: 600;
  cursor: pointer;
  width: 100%;
  margin-top: 4px;
}
.btn-alert:hover { background: #2d2d4a; }
.bell-btn {
  flex-shrink: 0;
  display: inline-block;
  padding: 8px 14px;
  border: 1px solid rgba(255,255,255,0.25);
  color: var(--paper);
  background: none;
  font-size: 1.1rem;
  cursor: pointer;
  transition: background 0.15s;
}
.bell-btn:hover { background: rgba(255,255,255,0.1); }

/* ── Alert modal ── */
.alert-form .section-label {
  font-size: 0.78rem;
  font-weight: 600;
  text-transform: uppercase;
  letter-spacing: 1.5px;
  color: var(--muted);
  margin: 0 0 8px;
  padding-bottom: 6px;
  border-bottom: 1px solid var(--border);
}
.alert-form .form-group { margin-bottom: 20px; }
.alert-form input[type='email'],
.alert-form input[type='text'] {
  width: 100%;
  padding: 9px 12px;
  border: 1px solid var(--border);
  font-size: 1rem;
  font-family: 'Source Sans 3', sans-serif;
  background: var(--paper);
  color: var(--ink);
  outline: none;
  box-sizing: border-box;
}
/* Checkbox grid */
.cb-grid {
  display: grid;
  grid-template-columns: 1fr 1fr;
  gap: 4px 16px;
  max-height: 180px;
  overflow-y: auto;
  padding: 10px 12px;
  background: var(--paper);
  border: 1px solid var(--border);
}
.cb-grid label {
  display: flex;
  align-items: center;
  gap: 7px;
  font-size: 0.92rem;
  font-weight: 400;
  text-transform: none;
  letter-spacing: 0;
  color: var(--ink);
  cursor: pointer;
  padding: 2px 0;
}
.cb-grid input[type='checkbox'] { cursor: pointer; accent-color: var(--accent); }
.cb-grid-single { grid-template-columns: 1fr 1fr 1fr; max-height: none; }
.alert-note {
  font-size: 0.82rem;
  color: var(--muted);
  margin-top: 16px;
  line-height: 1.6;
  border-top: 1px solid var(--border);
  padding-top: 14px;
}
.btn-alert {
  background: var(--ink);
  color: var(--paper);
  border: none;
  padding: 10px 24px;
  font-size: 1rem;
  font-family: 'Source Sans 3', sans-serif;
  font-weight: 600;
  cursor: pointer;
  width: 100%;
  margin-top: 4px;
}
.btn-alert:hover { background: #2d2d4a; }
.bell-btn {
  flex-shrink: 0;
  display: inline-block;
  padding: 8px 14px;
  border: 1px solid rgba(255,255,255,0.25);
  color: var(--paper);
  background: none;
  font-size: 1.05rem;
  cursor: pointer;
  transition: background 0.15s;
  font-family: 'Source Sans 3', sans-serif;
}
.bell-btn:hover { background: rgba(255,255,255,0.1); }
@media (max-width: 768px) {
  .cb-grid { grid-template-columns: 1fr 1fr; }
  .cb-grid-single { grid-template-columns: 1fr 1fr; }
}

.shiny-options-group {
  display: grid;
  grid-template-columns: 1fr 1fr;
  gap: 2px 12px;
  padding: 10px 12px;
  background: var(--paper);
  border: 1px solid var(--border);
}
#alert_subjekty .shiny-options-group {
  display: grid;
  grid-template-columns: 1fr 1fr 1fr;
  gap: 2px 8px;
  padding: 10px 12px;
  background: var(--paper);
  border: 1px solid var(--border);
}
.alert-form .shiny-input-checkboxgroup { margin-bottom: 0; }
.alert-form .checkbox { margin: 2px 0; }
.alert-form .checkbox label {
  font-size: 0.92rem;
  font-weight: 400;
  text-transform: none;
  letter-spacing: 0;
  color: var(--ink);
  cursor: pointer;
}
.alert-form .checkbox input[type='checkbox'] {
  accent-color: var(--accent);
  cursor: pointer;
}
@media (max-width: 768px) {
  #alert_predkladatele .shiny-options-group { grid-template-columns: 1fr 1fr; }
  #alert_subjekty      .shiny-options-group { grid-template-columns: 1fr 1fr; }
}
"

# ── Mobile JS ─────────────────────────────────────────────────────────────────

mobile_js <- HTML("
<script>
function showDetail() {
  if (window.innerWidth <= 768) {
    document.querySelector('.app-body').classList.add('detail-open');
    // Scroll main panel back to top on each new selection
    var mp = document.querySelector('.main-panel');
    if (mp) mp.scrollTop = 0;
  }
}

function showList() {
  document.querySelector('.app-body').classList.remove('detail-open');
}
</script>
")

# ── UI ────────────────────────────────────────────────────────────────────────

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML(app_css)),
    tags$title("Sněmovna dnes"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    mobile_js
  ),
  
  # Header
  div(class = "app-header",
      div(class = "header-left",
          h1("Sněmovna dnes",
             onclick = "Shiny.setInputValue('go_home', Date.now(), {priority: 'event'})"),
          div(class = "subtitle",
              "Dopady regulace · Náklady a přínosy · Hodnocení RIA")
      ),
      tags$button(
        class   = "bell-btn",
        title   = "Nastavit e-mailové upozornění",
        onclick = "Shiny.setInputValue('open_alert_modal', Date.now(), {priority: 'event'})",
        "\U0001F514 Upozornění"
      )
  ),
  
  # Body
  div(class = "app-body",
      
      # ── Sidebar ──────────────────────────────────────────────────────────
      div(class = "sidebar",
          div(class = "sidebar-label", "Legislativní materiály"),
          
          # Search box
          div(style = "padding: 8px 24px 12px;",
              tags$input(
                id          = "search_text",
                type        = "text",
                placeholder = "Hledat zákon...",
                oninput     = "Shiny.setInputValue('search_text', this.value)",
                style       = paste(
                  "width: 100%",
                  "padding: 8px 12px",
                  "border: 1px solid var(--border)",
                  "font-size: 1.3rem",
                  "font-family: 'Source Sans 3', sans-serif",
                  "outline: none",
                  "background: var(--paper)",
                  sep = ";"
                )
              )
          ),
          
          # Ministry filter
          div(style = "padding: 0 24px 12px;",
              tags$select(
                id       = "filter_predkladatel",
                onchange = "Shiny.setInputValue('filter_predkladatel', this.value)",
                style    = paste(
                  "width: 100%",
                  "padding: 8px 12px",
                  "border: 1px solid var(--border)",
                  "font-size: 1.3rem",
                  "font-family: 'Source Sans 3', sans-serif",
                  "background: var(--paper)",
                  "outline: none",
                  sep = ";"
                ),
                tags$option(value = "", "Všichni předkladatelé"),
                tags$option(value = "MF",   "MF — Ministerstvo financí"),
                tags$option(value = "MPSV", "MPSV — Ministerstvo práce"),
                tags$option(value = "MZD",  "MZD — Ministerstvo zdravotnictví"),
                tags$option(value = "MPO",  "MPO — Ministerstvo průmyslu"),
                tags$option(value = "MV",   "MV — Ministerstvo vnitra"),
                tags$option(value = "MZE",  "MZE — Ministerstvo zemědělství"),
                tags$option(value = "MŽP",  "MŽP — Ministerstvo životního prostředí"),
                tags$option(value = "MS",   "MS — Ministerstvo spravedlnosti"),
                tags$option(value = "MO",   "MO — Ministerstvo obrany"),
                tags$option(value = "MŠMT", "MŠMT — Ministerstvo školství"),
                tags$option(value = "MK", "MK — Ministerstvo kultury"),
                tags$option(value = "KML",  "KML — Poslanecký/senátní návrh")
              )
          ),
          
          # RIA filter
          div(style = "padding: 0 24px 12px;",
              tags$select(
                id       = "filter_ria",
                onchange = "Shiny.setInputValue('filter_ria', this.value)",
                style    = paste(
                  "width: 100%",
                  "padding: 8px 12px",
                  "border: 1px solid var(--border)",
                  "font-size: 1.3rem",
                  "font-family: 'Source Sans 3', sans-serif",
                  "background: var(--paper)",
                  "outline: none",
                  sep = ";"
                ),
                tags$option(value = "",              "Všechny materiály"),
                tags$option(value = "s_hodnocenim",  "S hodnocením dopadů"),
                tags$option(value = "ria_only",      "— pouze Závěrečná zpráva RIA"),
                tags$option(value = "prehled_only",  "— pouze Přehled / Důvodová zpráva"),
                tags$option(value = "bez_hodnoceni", "Bez hodnocení dopadů")
              )
          ),
          
          uiOutput("bill_list")
      ),
      
      # ── Main panel ───────────────────────────────────────────────────────
      div(class = "main-panel",
          
          # Back button — only visible on mobile via CSS
          tags$button(
            class   = "back-btn",
            onclick = "showList()",
            "← Zpět na seznam"
          ),
          
          uiOutput("bill_detail")
      )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {
  
  db <- get_db()
  
  # DEBUG — remove after
  test <- get_all_bills(db)
  message("Columns: ", paste(names(test), collapse = ", "))
  message("First status_name: ", test$status_name[1])
  message("First description: ", test$description[1])
  
  
  onStop(function() dbDisconnect(db))
  
  bills <- reactive({
    get_all_bills(db)
  })
  
  selected_pid <- reactiveVal(NULL)
  
  # ── Sidebar bill list ──────────────────────────────────────────────────────
  output$bill_list <- renderUI({
    df <- bills()
    if (nrow(df) == 0) {
      return(div(class = "empty-state",
                 div(class = "icon", "📋"),
                 p("Zatím žádné materiály.", br(),
                   "Spusťte pipeline pro načtení dat.")
      ))
    }
    
    # Search filter
    search <- input$search_text
    if (!is.null(search) && nchar(trimws(search)) > 0) {
      df <- df[str_detect(tolower(df$title), tolower(trimws(search))), ]
    }
    
    # Ministry filter
    pred <- input$filter_predkladatel
    if (!is.null(pred) && nchar(pred) > 0) {
      df <- df[!is.na(df$predkladatel) & df$predkladatel == pred, ]
    }
    
    # RIA filter
    ria_filter <- input$filter_ria
    if (!is.null(ria_filter) && nchar(ria_filter) > 0) {
      has_assessment <- !is.na(df$typ_dokumentu) &
        df$typ_dokumentu != "" &
        df$typ_dokumentu != "zadne"
      if (ria_filter == "s_hodnocenim") {
        df <- df[has_assessment, ]
      } else if (ria_filter == "ria_only") {
        df <- df[!is.na(df$typ_dokumentu) & df$typ_dokumentu == "RIA", ]
      } else if (ria_filter == "prehled_only") {
        df <- df[!is.na(df$typ_dokumentu) & df$typ_dokumentu == "prehled_dopadu", ]
      } else if (ria_filter == "bez_hodnoceni") {
        df <- df[!has_assessment, ]
      }
    }
    
    if (nrow(df) == 0) {
      return(div(style = "padding: 24px; color: var(--muted); font-size: 0.9rem;",
                 "Žádné výsledky."))
    }
    
    lapply(seq_len(nrow(df)), function(i) {
      row <- df[i, ]
      
      doc_type <- if (!is.na(row$typ_dokumentu)) row$typ_dokumentu else ""
      
      pill_class <- if (doc_type == "RIA") {
        "pill pill-ria"
      } else if (doc_type == "prehled_dopadu") {
        "pill pill-noria"
      } else {
        "pill pill-none"
      }
      
      pill_text <- if (doc_type == "RIA") {
        "Závěrečná zpráva RIA"
      } else if (doc_type == "prehled_dopadu") {
        "Shrnutí dopadů"
      } else {
        "Bez hodnocení"
      }
      
      is_active <- !is.null(selected_pid()) && selected_pid() == row$pid
      
      div(
        class = paste("bill-item", if (is_active) "active" else ""),
        # showDetail() triggers mobile panel slide on selection
        onclick = sprintf(
          "Shiny.setInputValue('selected_pid', '%s', {priority: 'event'}); showDetail();",
          row$pid
        ),
        div(class = "bill-title", str_trunc(row$title, 120)),
        div(class = "bill-meta",
            span(class = pill_class, pill_text),
            span(row$predkladatel),
            span(format(as.Date(str_extract(row$published, "\\d{2} \\w+ \\d{4}"),
                                "%d %b %Y"), "%d. %m. %Y", locale = "Czech"))
        )
      )
    })
  })
  
  observeEvent(input$selected_pid, {
    selected_pid(input$selected_pid)
  })
  
  # Clicking the title resets to the intro screen
  observeEvent(input$go_home, {
    selected_pid(NULL)
  })
  
  # ── Handle URL query parameters (?confirm=, ?unsubscribe=, ?pid=) ──────────
  observeEvent(session$clientData$url_search, {
    query <- parseQueryString(session$clientData$url_search)
    
    if (!is.null(query$pid) && nchar(query$pid) > 0)
      selected_pid(query$pid)
    
    if (!is.null(query$confirm) && nchar(query$confirm) > 0) {
      db_q <- tryCatch(get_db(), error = function(e) NULL)
      if (!is.null(db_q)) {
        result <- confirm_alert(db_q, query$confirm)
        dbDisconnect(db_q)
        showModal(modalDialog(
          title     = if (result$ok) "\u2713 Upozornění aktivováno" else "Chyba",
          p(result$message),
          footer    = modalButton("Zavřít"),
          easyClose = TRUE
        ))
      }
    }
    
    if (!is.null(query$unsubscribe) && nchar(query$unsubscribe) > 0) {
      db_q <- tryCatch(get_db(), error = function(e) NULL)
      if (!is.null(db_q)) {
        result <- unsubscribe_alert(db_q, query$unsubscribe)
        dbDisconnect(db_q)
        showModal(modalDialog(
          title     = if (result$ok) "Upozornění zrušeno" else "Chyba",
          p(result$message),
          footer    = modalButton("Zavřít"),
          easyClose = TRUE
        ))
      }
    }
  }, ignoreNULL = FALSE, ignoreInit = FALSE)
  
  
  # ── Open alert signup modal ────────────────────────────────────────────────
  observeEvent(input$open_alert_modal, {
    showModal(modalDialog(
      title     = "\U0001F514 Nastavit e-mailové upozornění",
      size      = "m",
      easyClose = TRUE,
      
      div(class = "alert-form",
          
          # ── Email ──
          div(class = "form-group",
              p(class = "section-label", "Váš e-mail"),
              tags$input(
                id          = "alert_email",
                type        = "email",
                placeholder = "vas@email.cz",
                oninput     = "Shiny.setInputValue('alert_email', this.value)"
              )
          ),
          
          # ── Ministries — plain HTML checkboxes, NO onchange ──
          div(class = "form-group",
              p(class = "section-label", "Ministerstvo (jedno nebo více)"),
              div(class = "cb-grid",
                  tags$label(tags$input(type="checkbox", `data-group`="alert_predkladatele", value="MF"),   "MF — Finance"),
                  tags$label(tags$input(type="checkbox", `data-group`="alert_predkladatele", value="MPSV"), "MPSV — Práce"),
                  tags$label(tags$input(type="checkbox", `data-group`="alert_predkladatele", value="MZD"),  "MZD — Zdravotnictví"),
                  tags$label(tags$input(type="checkbox", `data-group`="alert_predkladatele", value="MPO"),  "MPO — Průmysl"),
                  tags$label(tags$input(type="checkbox", `data-group`="alert_predkladatele", value="MV"),   "MV — Vnitra"),
                  tags$label(tags$input(type="checkbox", `data-group`="alert_predkladatele", value="MZE"),  "MZE — Zemědělství"),
                  tags$label(tags$input(type="checkbox", `data-group`="alert_predkladatele", value="MŽP"),  "MŽP — Živ. prostředí"),
                  tags$label(tags$input(type="checkbox", `data-group`="alert_predkladatele", value="MS"),   "MS — Spravedlnost"),
                  tags$label(tags$input(type="checkbox", `data-group`="alert_predkladatele", value="MO"),   "MO — Obrana"),
                  tags$label(tags$input(type="checkbox", `data-group`="alert_predkladatele", value="MŠMT"), "MŠMT — Školství"),
                  tags$label(tags$input(type="checkbox", `data-group`="alert_predkladatele", value="MK"),   "MK — Kultura"),
                  tags$label(tags$input(type="checkbox", `data-group`="alert_predkladatele", value="KML"),  "KML — Poslanecký návrh")
              )
          ),
          
          # ── Subject types — plain HTML checkboxes, NO onchange ──
          div(class = "form-group",
              p(class = "section-label", "Typ dotčeného subjektu (jedno nebo více)"),
              div(class = "cb-grid cb-grid-single",
                  tags$label(tags$input(type="checkbox", `data-group`="alert_subjekty", value="státní rozpočet"),  "státní rozpočet"),
                  tags$label(tags$input(type="checkbox", `data-group`="alert_subjekty", value="obce"),             "obce"),
                  tags$label(tags$input(type="checkbox", `data-group`="alert_subjekty", value="podnikatelé"),      "podnikatelé"),
                  tags$label(tags$input(type="checkbox", `data-group`="alert_subjekty", value="domácnosti"),       "domácnosti"),
                  tags$label(tags$input(type="checkbox", `data-group`="alert_subjekty", value="zaměstnavatelé"),   "zaměstnavatelé"),
                  tags$label(tags$input(type="checkbox", `data-group`="alert_subjekty", value="spotřebitelé"),     "spotřebitelé"),
                  tags$label(tags$input(type="checkbox", `data-group`="alert_subjekty", value="zaměstnanci"),      "zaměstnanci"),
                  tags$label(tags$input(type="checkbox", `data-group`="alert_subjekty", value="životní prostředí"),"životní prostředí")
              )
          ),
          
          # ── Keyword ──
          div(class = "form-group",
              p(class = "section-label", "Klíčové slovo v textu dopadů (volitelné)"),
              tags$input(
                id          = "alert_keyword",
                type        = "text",
                placeholder = "např. digitální, elektrická vozidla …",
                oninput     = "Shiny.setInputValue('alert_keyword', this.value)"
              )
          ),
          
          uiOutput("alert_submit_feedback"),
          
          # ── Submit — reads checkboxes from DOM at click time ──────────────
          # All three setInputValue calls happen synchronously, so Shiny
          # batches them into ONE message and processes them together.
          # When submit_alert fires in R, predkladatele/subjekty are already set.
          tags$button(
            class   = "btn-alert",
            onclick = "
              var preds = Array.from(
                document.querySelectorAll('input[data-group=\"alert_predkladatele\"]:checked')
              ).map(function(b){ return b.value; });
 
              var subjs = Array.from(
                document.querySelectorAll('input[data-group=\"alert_subjekty\"]:checked')
              ).map(function(b){ return b.value; });
 
              Shiny.setInputValue('alert_predkladatele', preds, {priority: 'event'});
              Shiny.setInputValue('alert_subjekty',      subjs, {priority: 'event'});
              Shiny.setInputValue('submit_alert', Date.now(),   {priority: 'event'});
            ",
            "Zaregistrovat upozornění"
          ),
          
          div(class = "alert-note",
              "Po odeslání vám přijde potvrzovací e-mail — upozornění se aktivuje",
              "až po kliknutí na odkaz v něm.", tags$br(),
              "Na jednu adresu lze zaregistrovat nejvýše 5 upozornění.", tags$br(),
              "Z každého upozornění se lze kdykoli odhlásit odkazem v e-mailu."
          )
      ),
      
      footer = modalButton("Zavřít")
    ))
  })
  
  
  # ── Handle alert form submission ───────────────────────────────────────────
  
  alert_feedback <- reactiveVal(NULL)
  
  output$alert_submit_feedback <- renderUI({ alert_feedback() })
  
  observeEvent(input$open_alert_modal, {
    alert_feedback(NULL)
    
    showModal(modalDialog(
      title = "\U0001F514 Nastavit e-mailové upozornění",
      size = "m", easyClose = TRUE,
      div(class = "alert-form",
          div(class = "form-group",
              p(class = "section-label", "Váš e-mail"),
              tags$input(id="alert_email", type="email", placeholder="vas@email.cz",
                         oninput="Shiny.setInputValue('alert_email', this.value)")
          ),
          div(class = "form-group",
              p(class = "section-label", "Ministerstvo (jedno nebo více)"),
              div(class = "cb-grid",
                  tags$label(tags$input(type="checkbox", `data-group`="alert_predkladatele", value="MF"),   "MF — Finance"),
                  tags$label(tags$input(type="checkbox", `data-group`="alert_predkladatele", value="MPSV"), "MPSV — Práce"),
                  tags$label(tags$input(type="checkbox", `data-group`="alert_predkladatele", value="MZD"),  "MZD — Zdravotnictví"),
                  tags$label(tags$input(type="checkbox", `data-group`="alert_predkladatele", value="MPO"),  "MPO — Průmysl"),
                  tags$label(tags$input(type="checkbox", `data-group`="alert_predkladatele", value="MV"),   "MV — Vnitra"),
                  tags$label(tags$input(type="checkbox", `data-group`="alert_predkladatele", value="MZE"),  "MZE — Zemědělství"),
                  tags$label(tags$input(type="checkbox", `data-group`="alert_predkladatele", value="MŽP"),  "MŽP — Živ. prostředí"),
                  tags$label(tags$input(type="checkbox", `data-group`="alert_predkladatele", value="MS"),   "MS — Spravedlnost"),
                  tags$label(tags$input(type="checkbox", `data-group`="alert_predkladatele", value="MO"),   "MO — Obrana"),
                  tags$label(tags$input(type="checkbox", `data-group`="alert_predkladatele", value="MŠMT"), "MŠMT — Školství"),
                  tags$label(tags$input(type="checkbox", `data-group`="alert_predkladatele", value="MK"),   "MK — Kultura"),
                  tags$label(tags$input(type="checkbox", `data-group`="alert_predkladatele", value="KML"),  "KML — Poslanecký návrh")
              )
          ),
          div(class = "form-group",
              p(class = "section-label", "Typ dotčeného subjektu (jedno nebo více)"),
              div(class = "cb-grid cb-grid-single",
                  tags$label(tags$input(type="checkbox", `data-group`="alert_subjekty", value="státní rozpočet"),  "státní rozpočet"),
                  tags$label(tags$input(type="checkbox", `data-group`="alert_subjekty", value="obce"),             "obce"),
                  tags$label(tags$input(type="checkbox", `data-group`="alert_subjekty", value="podnikatelé"),      "podnikatelé"),
                  tags$label(tags$input(type="checkbox", `data-group`="alert_subjekty", value="domácnosti"),       "domácnosti"),
                  tags$label(tags$input(type="checkbox", `data-group`="alert_subjekty", value="zaměstnavatelé"),   "zaměstnavatelé"),
                  tags$label(tags$input(type="checkbox", `data-group`="alert_subjekty", value="spotřebitelé"),     "spotřebitelé"),
                  tags$label(tags$input(type="checkbox", `data-group`="alert_subjekty", value="zaměstnanci"),      "zaměstnanci"),
                  tags$label(tags$input(type="checkbox", `data-group`="alert_subjekty", value="životní prostředí"),"životní prostředí")
              )
          ),
          div(class = "form-group",
              p(class = "section-label", "Klíčové slovo v textu dopadů (volitelné)"),
              tags$input(id="alert_keyword", type="text",
                         placeholder="např. digitální, elektrická vozidla …",
                         oninput="Shiny.setInputValue('alert_keyword', this.value)")
          ),
          uiOutput("alert_submit_feedback"),
          tags$button(
            class = "btn-alert",
            onclick = "
          var preds = Array.from(document.querySelectorAll('input[data-group=\"alert_predkladatele\"]:checked')).map(function(b){ return b.value; });
          var subjs = Array.from(document.querySelectorAll('input[data-group=\"alert_subjekty\"]:checked')).map(function(b){ return b.value; });
          Shiny.setInputValue('alert_predkladatele', preds, {priority: 'event'});
          Shiny.setInputValue('alert_subjekty',      subjs, {priority: 'event'});
          Shiny.setInputValue('submit_alert', Date.now(),   {priority: 'event'});
        ",
            "Zaregistrovat upozornění"
          ),
          div(class = "alert-note",
              "Po odeslání vám přijde potvrzovací e-mail — upozornění se aktivuje až po kliknutí na odkaz v něm.", tags$br(),
              "Na jednu adresu lze zaregistrovat nejvýše 5 upozornění.", tags$br(),
              "Z každého upozornění se lze kdykoli odhlásit odkazem v e-mailu."
          )
      ),
      footer = modalButton("Zavřít")
    ))
  })
  
  observeEvent(input$submit_alert, {
    tryCatch({
      email         <- trimws(input$alert_email %||% "")
      predkladatele <- if (is.null(input$alert_predkladatele)) character(0) else input$alert_predkladatele
      subjekty      <- if (is.null(input$alert_subjekty))      character(0) else input$alert_subjekty
      keyword       <- trimws(input$alert_keyword %||% "")
      
      result <- save_alert(db, email, predkladatele, subjekty, keyword)
      
      if (!result$ok) {
        alert_feedback(div(style="color:#c0392b;font-size:0.9rem;margin-bottom:12px;", result$message))
        return()
      }
      
      email_sent <- tryCatch(
        send_confirmation_email(email=email, predkladatele=predkladatele,
                                subjekty=subjekty, keyword=keyword, token=result$token,
                                app_url=Sys.getenv("APP_URL", "https://snemovnadnes.cz")),
        error = function(e) FALSE
      )
      
      if (email_sent) {
        alert_feedback(div(style="color:#2e7d52;font-size:0.9rem;margin-bottom:12px;",
                           "\u2713 Potvrzovací e-mail byl odeslán. Zkontrolujte svou schránku a klikněte na odkaz."))
      } else {
        alert_feedback(div(style="color:#d4a017;font-size:0.9rem;margin-bottom:12px;",
                           "\u26a0 Upozornění bylo uloženo, ale potvrzovací e-mail se nepodařilo odeslat.",
                           tags$br(), tags$small("Nastavte RESEND_API_KEY nebo kontaktujte správce.")))
      }
    }, error = function(e) {
      message("submit_alert error: ", e$message)
      alert_feedback(div(style="color:#c0392b;font-size:0.9rem;margin-bottom:12px;",
                         "Nastala neočekávaná chyba: ", conditionMessage(e)))
    })
  })
  
  # ── Main detail panel ──────────────────────────────────────────────────────
  output$bill_detail <- renderUI({
    
    pid <- selected_pid()
    
    if (is.null(pid)) {
      return(div(class = "intro-panel",
                 div(class = "intro-eyebrow", "Analytický nástroj"),
                 h2("Kolik nás stojí nová legislativa?"),
                 div(class = "intro-lead",
                     "Sněmovna dnes automaticky sleduje návrhy zákonů a vyhlášek projednávané",
                     "v Poslanecké sněmovně a ze závěrečných zpráv z hodnocení dopadů regulace",
                     "(RIA) extrahuje odhadované náklady a přínosy pro státní rozpočet,",
                     "podnikatele, domácnosti i další skupiny."
                 ),
                 div(class = "intro-cards",
                     div(class = "intro-card",
                         div(class = "card-icon", "📋"),
                         div(class = "card-title", "Sleduje legislativu"),
                         div(class = "card-body",
                             "Automaticky načítá materiály z VeKLEP a pořad schůzí",
                             "Poslanecké sněmovny. Nové návrhy se přidávají průběžně.")
                     ),
                     div(class = "intro-card",
                         div(class = "card-icon", "🤖"),
                         div(class = "card-title", "Extrahuje dopady"),
                         div(class = "card-body",
                             "Pomocí jazykového modelu analyzuje dokumenty RIA a",
                             "vytahuje konkrétní finanční dopady a kvalitativní přínosy.")
                     ),
                     div(class = "intro-card",
                         div(class = "card-icon", "⚖️"),
                         div(class = "card-title", "Hodnocení Komise RIA"),
                         div(class = "card-body",
                             "Zobrazuje výsledky nezávislé Komise RIA Úřadu vlády —",
                             "od doporučení ke schválení (A) po nedoporučení (D).")
                     ),
                     div(class = "intro-card",
                         div(class = "card-icon", "🔍"),
                         div(class = "card-title", "Srovnání a filtrování"),
                         div(class = "card-body",
                             "Filtrujte podle ministerstva, typu hodnocení nebo",
                             "vyhledávejte konkrétní zákon v levém panelu.")
                     )
                 ),
                 div(class = "intro-how",
                     tags$strong("Jak začít:"), " vyberte materiál z levého panelu.",
                     " Kliknutím na název aplikace se vždy vrátíte na tuto stránku.",
                     tags$br(), tags$br(),
                     "Data pocházejí z veřejných zdrojů — VeKLEP (odok.cz),",
                     " psp.cz a ria.vlada.cz. Extrakce pomocí AI nemusí být vždy přesná;",
                     " vždy ověřte informace v původním dokumentu."
                 )
      ))
    }
    
    df   <- bills()
    row  <- df[df$pid == pid, ]
    if (nrow(row) == 0) return(NULL)
    
    costs    <- get_bill_costs(db, pid)
    benefits <- get_bill_benefits(db, pid)
    
    # ── Status badges ──
    detail_doc_type <- if (!is.na(row$typ_dokumentu)) row$typ_dokumentu else ""
    
    ria_badge <- if (detail_doc_type == "RIA") {
      div(class = "status-badge",
          div(class = "dot dot-green"),
          "Závěrečná zpráva z hodnocení dopadů regulace")
    } else if (detail_doc_type == "prehled_dopadu") {
      div(class = "status-badge",
          div(class = "dot dot-yellow"),
          "Přehled dopadů / Důvodová zpráva")
    } else {
      div(class = "status-badge",
          div(class = "dot dot-grey"),
          "Hodnocení dopadů nenalezeno")
    }
    
    cost_badge <- if (nrow(costs) > 0 && any(!is.na(costs$castka))) {
      total <- sum(costs$castka, na.rm = TRUE)
      div(class = "status-badge",
          div(class = "dot dot-red"),
          paste("Celkem náklady:", format(total, big.mark = " ", scientific = FALSE), "Kč"))
    } else if (nrow(costs) > 0) {
      div(class = "status-badge",
          div(class = "dot dot-grey"), "Náklady kvalitativně")
    } else {
      div(class = "status-badge",
          div(class = "dot dot-grey"), "Bez vyčíslených nákladů")
    }
    
    komise_badge <- if (isTRUE(!is.na(row$komise_verdict)) && row$komise_verdict != "") {
      verdict_color <- switch(row$komise_verdict,
                              "A" = "#2e7d52",
                              "B" = "#d4a017",
                              "C" = "#e65100",
                              "D" = "#c0392b",
                              "#7f8c8d"
      )
      verdict_label <- switch(row$komise_verdict,
                              "A" = "Komise RIA: A — doporučuje schválení",
                              "B" = "Komise RIA: B — doporučuje s připomínkami",
                              "C" = "Komise RIA: C — doporučuje přepracovat",
                              "D" = "Komise RIA: D — nedoporučuje schválení",
                              paste("Komise RIA:", row$komise_verdict)
      )
      div(class = "status-badge",
          div(class = "dot", style = paste0("background:", verdict_color, ";")),
          verdict_label
      )
    } else NULL
    
    # ── Costs ──
    costs_ui <- if (nrow(costs) == 0) {
      div(class = "empty-state",
          p("Žádné vyčíslené náklady nebyly nalezeny v dokumentu."))
    } else {
      max_cost <- max(costs$castka, na.rm = TRUE)
      if (is.infinite(max_cost) || is.na(max_cost)) max_cost <- 1
      
      tagList(lapply(seq_len(nrow(costs)), function(i) {
        c <- costs[i, ]
        has_amount  <- !is.na(c$castka)
        bar_pct     <- if (has_amount) round(c$castka / max_cost * 100) else 0
        amount_text <- if (has_amount) {
          paste(format(c$castka, big.mark = " ", scientific = FALSE), "Kč")
        } else {
          "kvalitativně"
        }
        div(class = "cost-row",
            div(class = "cost-subjekt", c$subjekt),
            div(class = "cost-bar-wrap",
                if (has_amount) div(class = "cost-bar-outer",
                                    div(class = "cost-bar-inner",
                                        style = paste0("width:", bar_pct, "%"))
                ),
                div(class = "cost-amount", amount_text),
                div(class = "cost-popis", c$popis),
                div(class = "cost-tags",
                    span(class = "cost-tag", c$periodicita),
                    span(class = "cost-tag", c$jistota)
                )
            )
        )
      }))
    }
    
    # ── Benefits ──
    benefits_ui <- if (nrow(benefits) == 0) {
      div(class = "empty-state", p("Žádné přínosy nebyly identifikovány."))
    } else {
      tagList(lapply(seq_len(nrow(benefits)), function(i) {
        b    <- benefits[i, ]
        text <- if (!is.na(b$castka)) {
          paste0(b$popis, " (", format(b$castka, big.mark = " ", scientific = FALSE), " Kč)")
        } else { b$popis }
        
        div(class = "cost-row",
            if (!is.na(b$subjekt) && b$subjekt != "") {
              div(class = "cost-subjekt", b$subjekt)
            } else {
              div(class = "cost-subjekt", style = "color:var(--muted);", "—")
            },
            div(class = "cost-bar-wrap",
                div(class = "cost-popis", style = "color: var(--benefit);", text)
            )
        )
      }))
    }
    
    # ── Assemble detail view ──
    tagList(
      div(class = "bill-header",
          div(class = "predkladatel", row$predkladatel),
          h2(row$title),
          
          if (!is.na(row$description) && row$description != "") {
            div(class = "shrnutí", row$description)
          },
          
          div(style = "display:flex; gap:24px; margin-top:14px; flex-wrap:wrap;",
              if (!is.na(row$status_name) && row$status_name != "") {
                div(style = "font-size:1.2rem;",
                    tags$span(style = "color:var(--muted);font-weight:600;
                                 text-transform:uppercase;letter-spacing:1px;
                                 font-size:1.2rem;", "Stav materiálu"),
                    tags$br(),
                    tags$span(row$status_name)
                )
              },
              if (!is.na(row$government_date) && row$government_date != "") {
                gov_date <- format(as.Date(str_sub(row$government_date, 1, 10)), "%d. %m. %Y")
                div(style = "font-size:1.2rem;",
                    tags$span(style = "color:var(--muted);font-weight:600;
                                 text-transform:uppercase;letter-spacing:1px;
                                 font-size:1.2rem;", "Datum schůze vlády"),
                    tags$br(),
                    tags$span(gov_date)
                )
              },
              if (!is.na(row$psp_status) && row$psp_status != "") {
                div(style = "font-size:1.2rem;",
                    tags$span(style = "color:var(--muted);font-weight:600;
                   text-transform:uppercase;letter-spacing:1px;
                   font-size:1.2rem;", "Stav projednávání"),
                    tags$br(),
                    tags$span(row$psp_status)
                )
              }
          ),
          
          div(style = "display:flex; gap:16px; margin-top:14px; flex-wrap:wrap;",
              if (!is.na(row$id_tisk) && row$id_tisk != "") {
                tags$a(
                  href   = paste0("https://www.psp.cz/sqw/historie.sqw?o=10&t=", row$id_tisk),
                  target = "_blank",
                  style  = "font-size:1.3rem;padding:6px 14px;border:1px solid var(--border);color:var(--ink);text-decoration:none;background:white;display:inline-block;",
                  "📄 Sněmovní tisk"
                )
              },
              tags$a(
                href   = paste0("https://www.odok.cz/portal/veklep/material/", row$pid, "/"),
                target = "_blank",
                style  = "font-size:1.3rem;padding:6px 14px;border:1px solid var(--border);color:var(--ink);text-decoration:none;background:white;display:inline-block;",
                "📋 VeKLEP"
              ),
              if (isTRUE(!is.na(row$komise_url)) && row$komise_url != "") {
                tags$a(
                  href   = row$komise_url,
                  target = "_blank",
                  style  = "font-size:1.3rem;padding:6px 14px;border:1px solid var(--border);color:var(--ink);text-decoration:none;background:white;display:inline-block;",
                  "⚖️ Stanovisko Komise RIA"
                )
              }
          )
      ),
      
      div(class = "status-row", ria_badge, cost_badge, komise_badge),
      
      div(class = "section-heading", "Náklady regulace"),
      costs_ui,
      
      div(class = "section-heading", "Přínosy regulace"),
      benefits_ui
    )
  })
}

shinyApp(ui, server)