# Sněmovna dnes

Automatický monitoring legislativních materiálů projednávaných v Poslanecké sněmovně ČR s důrazem na hodnocení dopadů regulace (RIA).

## Co aplikace dělá

- Sleduje pořad schůzí Poslanecké sněmovny a páruje sněmovní tisky s materiály ve VeKLEP
- Stahuje dokumenty RIA (Závěrečné zprávy z hodnocení dopadů regulace) z odok.cz
- Pomocí jazykového modelu (Gemini) extrahuje odhadované náklady a přínosy pro jednotlivé skupiny subjektů
- Zobrazuje výsledky nezávislé Komise RIA Úřadu vlády (verdikty A–D)
- Umožňuje e-mailová upozornění na nové materiály podle ministerstva nebo typu dotčeného subjektu

## Architektura

- **Pipeline** (R/07_scheduled_run.R) běží lokálně přes Windows Task Scheduler a zapisuje do Supabase (PostgreSQL)
- **Frontend** (app.R) je Shiny aplikace nasazená na shinyapps.io, která čte z Supabase

## Datové zdroje

- [VeKLEP / odok.cz](https://www.odok.cz/portal/veklep/) — legislativní materiály a dokumenty RIA
- [psp.cz](https://www.psp.cz) — pořad schůzí a sněmovní tisky
- [ria.vlada.cz](https://ria.vlada.cz) — stanoviska Komise RIA

## Spuštění

1. Naklonujte repozitář
2. Vytvořte soubor \.Renviron\ podle vzoru níže
3. Spusťte \R/07_scheduled_run.R\ pro první načtení dat
4. Spusťte \app.R\ pro zobrazení aplikace

\\\
SUPABASE_HOST=...
SUPABASE_PASSWORD=...
GEMINI_API_KEY=...
RESEND_API_KEY=...
APP_URL=...
\\\

## Licence

MIT
