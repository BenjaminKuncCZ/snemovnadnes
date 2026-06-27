# Sněmovna dnes

Automatický monitoring legislativních materiálů projednávaných v Poslanecké sněmovně ČR s důrazem na hodnocení dopadů regulace (RIA).

🌐 **Aplikace:** [snemovnadnes.cz](https://snemovnadnes.cz)

---

## Co aplikace dělá

- Sleduje pořad schůzí Poslanecké sněmovny a páruje sněmovní tisky s materiály ve VeKLEP
- Stahuje dokumenty RIA (Závěrečné zprávy z hodnocení dopadů regulace) z odok.cz
- Pomocí jazykového modelu (Gemini) extrahuje odhadované náklady a přínosy pro jednotlivé skupiny subjektů
- Zobrazuje výsledky nezávislé Komise RIA Úřadu vlády (verdikty A–D)
- Umožňuje e-mailová upozornění na nové materiály podle ministerstva nebo typu dotčeného subjektu

---

## Architektura

```
VeKLEP / psp.cz / ria.vlada.cz
        ↓
  Pipeline (R, GitHub Actions — denně v 5:00 UTC)
        ↓
  Supabase (PostgreSQL)
        ↓
  Shiny app (shinyapps.io) + Veřejné API
```

- **Pipeline** (`pipeline/07_scheduled_run.R`) běží automaticky přes GitHub Actions a zapisuje do Supabase (PostgreSQL)
- **Frontend** (`app.R`) je Shiny aplikace nasazená na shinyapps.io, která čte z Supabase

---

## Veřejné API

Data jsou dostupná přes veřejné REST API postavené na Supabase PostgREST. Nevyžaduje autentizaci — stačí přiložit hlavičku s publishable klíčem.

**Base URL:** `https://ktehtgonacsexkglnohi.supabase.co/rest/v1/`

**API klíč (publishable, read-only):** `sb_publishable_iu_w3f095qSF85-mBrnv2A_H1wQ3Yf8`

### Endpointy

#### Všechny materiály

```bash
GET /rest/v1/bills_public
```

Vrací záznamy z pohledu `bills_public`, který spojuje tabulky `bills` a `impacts`.

```bash
curl "https://ktehtgonacsexkglnohi.supabase.co/rest/v1/bills_public?select=*" \
  -H "apikey: sb_publishable_iu_w3f095qSF85-mBrnv2A_H1wQ3Yf8"
```

#### Filtrování

```bash
# Pouze materiály s plnohodnotnou ZZ RIA
curl ".../bills_public?typ_dokumentu=eq.RIA" \
  -H "apikey: sb_publishable_iu_w3f095qSF85-mBrnv2A_H1wQ3Yf8"

# Materiály konkrétního ministerstva
curl ".../bills_public?predkladatel=eq.MF" \
  -H "apikey: sb_publishable_iu_w3f095qSF85-mBrnv2A_H1wQ3Yf8"

# Materiály projednávané od určitého data
curl ".../bills_public?government_date=gte.2026-01-01" \
  -H "apikey: sb_publishable_iu_w3f095qSF85-mBrnv2A_H1wQ3Yf8"
```

#### Náklady regulace

```bash
curl ".../costs?pid=eq.ABCDEFGHIJKL&select=subjekt,castka,periodicita,jistota,popis" \
  -H "apikey: sb_publishable_iu_w3f095qSF85-mBrnv2A_H1wQ3Yf8"
```

#### Přínosy regulace

```bash
curl ".../benefits?pid=eq.ABCDEFGHIJKL&select=subjekt,monetizovano,castka,popis" \
  -H "apikey: sb_publishable_iu_w3f095qSF85-mBrnv2A_H1wQ3Yf8"
```

### Schéma `bills_public`

| Sloupec | Typ | Popis |
|---|---|---|
| `pid` | text | VeKLEP identifikátor materiálu |
| `title` | text | Název návrhu zákona |
| `predkladatel` | text | Kód ministerstva (MF, MPSV, …) |
| `id_tisk` | text | Číslo sněmovního tisku |
| `status_name` | text | Stav v systému VeKLEP |
| `psp_status` | text | Stav projednávání v Poslanecké sněmovně |
| `government_date` | text | Datum projednání vládou |
| `komise_verdict` | text | Verdikt Komise RIA (A/B/C/D) |
| `typ_dokumentu` | text | `RIA` / `prehled_dopadu` / `zadne` |
| `ria_provedena` | integer | 1 = hodnocení dopadů provedeno |
| `shrnutí` | text | Shrnutí cíle regulace (generováno LLM) |

### Dotazovací syntaxe

PostgREST podporuje bohatou sadu filtrů přímo v URL:

| Operátor | Příklad |
|---|---|
| rovná se | `?predkladatel=eq.MF` |
| větší nebo rovno | `?government_date=gte.2026-01-01` |
| obsahuje (case-insensitive) | `?title=ilike.*digitální*` |
| výběr sloupců | `?select=pid,title,typ_dokumentu` |
| řazení | `?order=government_date.desc` |
| stránkování | `?limit=20&offset=40` |

Kompletní dokumentace: [postgrest.org](https://docs.postgrest.org/en/stable/references/api/tables_views.html)

---

## Datové zdroje

- [VeKLEP / odok.cz](https://www.odok.cz/portal/veklep/) — legislativní materiály a dokumenty RIA
- [psp.cz](https://www.psp.cz) — pořad schůzí a sněmovní tisky
- [ria.vlada.cz](https://ria.vlada.cz) — stanoviska Komise RIA

---

## Spuštění vlastní instance

1. Naklonujte repozitář
2. Vytvořte soubor `.Renviron` v kořeni projektu:

```
SUPABASE_HOST=...
SUPABASE_PASSWORD=...
GEMINI_API_KEY=...
RESEND_API_KEY=...
APP_URL=https://your-app-url
ADMIN_EMAIL=your@email.com
```

3. Spusťte pipeline pro první načtení dat: `Rscript pipeline/07_scheduled_run.R`
4. Spusťte aplikaci: `shiny::runApp()`

---

## Licence

MIT
