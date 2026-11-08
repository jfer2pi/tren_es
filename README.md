# tren_es · Renfe Live

Part of [Fernando Rodríguez Alvarado's portfolio](https://jfer2pi.github.io)

A single-file R/Shiny dashboard showing the live positions of Renfe's high-speed, long-distance and regional trains (AVE, Avlo, Alvia, Avant, Euromed, Intercity, Media Distancia, Regional) on an open-source map.

Everything lives in [`app.R`](app.R).

## Features

- **Live map** on OpenStreetMap, with a dark variant and an OpenRailwayMap overlay of the rail network. Markers are coloured by service; filled means moving and a ring means stopped. Recent trails are drawn behind each train.
- **Train table** with route, status, current or next stop and estimated speed. Click a row to fly to the train.
- **Filters** for search (train number, station, route), service type, a high-speed-only switch, and moving/stopped.
- **KPIs** for trains live, high-speed count, moving, stopped, flags, and fastest estimated high-speed train.
- **Refresh** every 30 s, 60 s, 2 min, or manually. One feed call per 20 s at most is shared across all viewers.
- **Close-approach flag** (red): a moving train with a stopped train ahead of it on its heading, reachable within a few minutes. This is a congestion and bunching indicator, not a collision-risk estimate (see [Limitations](#limitations)).
- **Data-anomaly flags** (amber):
  - an implausible position jump (over 360 km/h)
  - a position outside the network area
  - the same vehicle reported in two places
  - "stopped" while moving
  - a position frozen while "in transit"

## Data sources

| Source | Use | Licence / terms |
|---|---|---|
| Renfe GTFS-RT vehicle positions: `https://gtfsrt.renfe.com/vehicle_positions_LD.json` | Live positions | Renfe open data |
| Renfe GTFS static (AV/LD/MD): `https://ssl.renfe.com/gtransit/Fichero_AV_LD/google_transit.zip` | Service type, station names, origin and destination; downloaded and cached once a day | Renfe open data |
| OpenStreetMap tiles | Basemap | ODbL data; [tile usage policy](https://operations.osmfoundation.org/policies/tiles/) |
| OpenRailwayMap tiles | Rail overlay | CC-BY-SA |

## Run locally

```r
install.packages(c("shiny", "bslib", "httr2", "data.table",
                   "leaflet", "reactable", "htmltools"))
shiny::runApp("app.R")
```

The static GTFS file is downloaded on the first fetch and cached in `cache/`. If that folder isn't writable, the app uses a temporary folder instead; you can also set the `RENFE_CACHE_DIR` environment variable.

## Deploy

GitHub Pages only serves static files, so it can't run a Shiny server. Either of these works:

- **Posit Connect Cloud** (deploys straight from this repo; the free tier needs a public repo):
  1. In Connect Cloud, choose *Publish → Shiny*, pick this repo, branch `main` and `app.R`. The included `manifest.json` lists R 4.3.3 and the exact package versions to install.
  2. Turn on *Automatically publish on push* to redeploy on every commit.
  3. If you add a package or change `app.R`'s dependencies, regenerate the manifest locally with `rsconnect::writeManifest(appFiles = "app.R")` and commit it.
- **shinyapps.io**: run `rsconnect::deployApp(appFiles = "app.R", appName = "tren_es")`.

## How it works

### Feed parsing

- **Trip IDs.** As observed, a trip ID is a 5-digit train number, then a segment digit, then the service date, e.g. `12484` + `1` + `2026-10-29`.
- **Segments.** A train that changes number en route appears once per segment at identical coordinates. These rows are collapsed into one marker.
- **Specials.** Unplanned services appear as `SPECIAL_<number><yyyymmdd>`.
- **Missing stops.** The `stopId` is sometimes missing on later segments.

### Train types and stations

Trains are matched to the static timetable by trip ID, falling back to train number. That match gives each train its service type, station names, and origin and destination. Unmatched trains show as *Other*.

### Freshness

Renfe's `header.timestamp` is not reliable. On 31 Oct 2026 it lagged the live positions by about 19 hours. The app therefore detects updates by comparing the positions themselves. Whenever they change, it records the server time and uses that for:

- the Live/Stale badge (stale after 3 min without change, or after a failed fetch)
- trails and history
- speed and heading estimates

The header timestamp is still shown in the sidebar, for reference only.

### Speed and heading

Both are estimated from a train's last two distinct positions. They are left blank across gaps longer than 5 minutes or for jumps over 360 km/h. Because they need a couple of updates in memory, they fill in a minute or two after the app starts.

### Configuration

All thresholds are constants at the top of `app.R`:

- `CA_*` for the close-approach flag
- `MAX_PLAUSIBLE_KMH`, `DUP_SPREAD_KM` and `FROZEN_SNAPSHOTS` for the anomaly flags
- `MIN_FETCH_GAP_S` and `STALE_AFTER_S` for refresh and freshness

## Limitations

- **Close approaches are not a safety signal.** The feed has no track assignment: two nearby trains may be on parallel or different lines. Spanish lines are also protected by signalling (ETCS/ERTMS, LZB, ASFA) that enforces braking to a stopped train ahead. Treat the flag as a congestion and bunching indicator.
- **Speeds are estimates.** Positions refresh about every 30 s, and speeds are derived from them, not reported by Renfe.
- **History is in memory.** Trails, speeds and flags reset whenever the app restarts.
- **OSM tile usage.** The public OpenStreetMap tile server suits personal or low-traffic use. For heavier public use, point the tile URL in `app.R` at a self-hosted or commercial OSM tile server.

## Licence

MIT. Data remains under its providers' terms (see [Data sources](#data-sources)).
