# \# Sněmovna dnes

# 

# Automatický monitoring legislativních materiálů projednávaných v Poslanecké sněmovně ČR s důrazem na hodnocení dopadů regulace (RIA).

# 

# 🌐 \*\*Aplikace:\*\* \[snemovnadnes.cz](https://snemovnadnes.cz)

# 

# \## Co aplikace dělá

# 

# \- Sleduje pořad schůzí Poslanecké sněmovny a páruje sněmovní tisky s materiály ve VeKLEP

# \- Stahuje dokumenty RIA (Závěrečné zprávy z hodnocení dopadů regulace) z odok.cz

# \- Pomocí jazykového modelu (Gemini) extrahuje odhadované náklady a přínosy pro jednotlivé skupiny subjektů

# \- Zobrazuje výsledky nezávislé Komise RIA Úřadu vlády (verdikty A–D)

# \- Umožňuje e-mailová upozornění na nové materiály podle ministerstva nebo typu dotčeného subjektu

# 

# \## Architektura

# 

# \- \*\*Pipeline\*\* (`R/07\_scheduled\_run.R`) běží lokálně přes Windows Task Scheduler a zapisuje do Supabase (PostgreSQL)

# \- \*\*Frontend\*\* (`app.R`) je Shiny aplikace nasazená na shinyapps.io, která čte z Supabase

# 

# ```

# VeKLEP / psp.cz / ria.vlada.cz

# &#x20;       ↓

# &#x20; Pipeline (R, Windows Task Scheduler)

# &#x20;       ↓

# &#x20; Supabase (PostgreSQL)

# &#x20;       ↓

# &#x20; Shiny app (shinyapps.io) + Veřejné API

# ```

# 

# \## Veřejné API

# 

# Data jsou dostupná přes veřejné REST API postavené na Supabase PostgREST. Nevyžaduje autentizaci — stačí přiložit hlavičku s publishable klíčem.

# 

# \*\*Base URL:\*\* `https://ktehtgonacsexkglnohi.supabase.co/rest/v1/`

# 

# \*\*API klíč (publishable, read-only):\*\* `sb\_publishable\_iu\_w3f095qSF85-mBrnv2A\_H1wQ3Yf8`

# 

# \### Endpointy

# 

# \#### Všechny materiály (spojené s hodnocením dopadů)

# 

# ```

# GET /rest/v1/bills\_public

# ```

# 

# Vrací záznamy z pohledu `bills\_public`, který spojuje tabulky `bills` a `impacts`.

# 

# \*\*Příklad:\*\*

# ```bash

# curl "https://ktehtgonacsexkglnohi.supabase.co/rest/v1/bills\_public?select=\*" \\

# &#x20; -H "apikey: sb\_publishable\_iu\_w3f095qSF85-mBrnv2A\_H1wQ3Yf8"

# ```

# 

# \#### Filtrování

# 

# ```bash

# \# Pouze materiály s plnohodnotnou ZZ RIA

# curl ".../bills\_public?typ\_dokumentu=eq.RIA" \\

# &#x20; -H "apikey: sb\_publishable\_iu\_w3f095qSF85-mBrnv2A\_H1wQ3Yf8"

# 

# \# Materiály konkrétního ministerstva

# curl ".../bills\_public?predkladatel=eq.MF" \\

# &#x20; -H "apikey: sb\_publishable\_iu\_w3f095qSF85-mBrnv2A\_H1wQ3Yf8"

# 

# \# Materiály projednávané od určitého data

# curl ".../bills\_public?government\_date=gte.2026-01-01" \\

# &#x20; -H "apikey: sb\_publishable\_iu\_w3f095qSF85-mBrnv2A\_H1wQ3Yf8"

# ```

# 

# \#### Náklady regulace

# 

# ```bash

# \# Náklady pro konkrétní materiál (pid = VeKLEP identifikátor)

# curl ".../costs?pid=eq.ABCDEFGHIJKL\&select=subjekt,castka,periodicita,jistota,popis" \\

# &#x20; -H "apikey: sb\_publishable\_iu\_w3f095qSF85-mBrnv2A\_H1wQ3Yf8"

# ```

# 

# \#### Přínosy regulace

# 

# ```bash

# curl ".../benefits?pid=eq.ABCDEFGHIJKL\&select=subjekt,monetizovano,castka,popis" \\

# &#x20; -H "apikey: sb\_publishable\_iu\_w3f095qSF85-mBrnv2A\_H1wQ3Yf8"

# ```

# 

# \### Schéma `bills\_public`

# 

# | Sloupec | Typ | Popis |

# |---|---|---|

# | `pid` | text | VeKLEP identifikátor materiálu |

# | `title` | text | Název návrhu zákona |

# | `predkladatel` | text | Kód ministerstva (MF, MPSV, …) |

# | `id\_tisk` | text | Číslo sněmovního tisku |

# | `status\_name` | text | Stav v systému VeKLEP |

# | `psp\_status` | text | Stav projednávání v Poslanecké sněmovně |

# | `government\_date` | text | Datum projednání vládou |

# | `komise\_verdict` | text | Verdikt Komise RIA (A/B/C/D) |

# | `typ\_dokumentu` | text | `RIA` / `prehled\_dopadu` / `zadne` |

# | `ria\_provedena` | integer | 1 = hodnocení dopadů provedeno |

# | `shrnutí` | text | Shrnutí cíle regulace (generováno LLM) |

# 

# \### Dotazovací syntaxe

# 

# PostgREST podporuje bohatou sadu filtrů přímo v URL:

# 

# | Operátor | Příklad |

# |---|---|

# | rovná se | `?predkladatel=eq.MF` |

# | větší nebo rovno | `?government\_date=gte.2026-01-01` |

# | obsahuje (case-insensitive) | `?title=ilike.\*digitální\*` |

# | výběr sloupců | `?select=pid,title,typ\_dokumentu` |

# | řazení | `?order=government\_date.desc` |

# | stránkování | `?limit=20\&offset=40` |

# 

# Kompletní dokumentace: \[postgrest.org/api](https://docs.postgrest.org/en/stable/references/api/tables\_views.html)

# 

# \## Datové zdroje

# 

# \- \[VeKLEP / odok.cz](https://www.odok.cz/portal/veklep/) — legislativní materiály a dokumenty RIA

# \- \[psp.cz](https://www.psp.cz) — pořad schůzí a sněmovní tisky

# \- \[ria.vlada.cz](https://ria.vlada.cz) — stanoviska Komise RIA

# 

# \## Spuštění (vlastní instance)

# 

# 1\. Naklonujte repozitář

# 2\. Vytvořte soubor `.Renviron` podle vzoru níže

# 3\. Spusťte `R/07\_scheduled\_run.R` pro první načtení dat

# 4\. Spusťte `app.R` pro zobrazení aplikace

# 

# 

# \## Licence

# 

# MIT



