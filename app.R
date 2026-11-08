# =============================================================================
# Renfe Live: real-time map of Renfe AV / LD / MD trains
# Single-file Shiny app. Linear-style UI.
#
# Data
#   Realtime : https://gtfsrt.renfe.com/vehicle_positions_LD.json  (GTFS-RT)
#   Static   : https://ssl.renfe.com/gtransit/Fichero_AV_LD/google_transit.zip
#              (used to label each train as AVE / Avlo / Alvia / MD ... and to
#               resolve station names, origin and destination)
#
# Packages: shiny, bslib, httr2, data.table, leaflet, reactable, htmltools
# =============================================================================

library(shiny)
library(bslib)
library(data.table)
library(leaflet)
library(reactable)
library(htmltools)

# -----------------------------------------------------------------------------
# Config
# -----------------------------------------------------------------------------
FEED_URL        <- "https://gtfsrt.renfe.com/vehicle_positions_LD.json"
GTFS_URL        <- "https://ssl.renfe.com/gtransit/Fichero_AV_LD/google_transit.zip"
UA              <- "renfe-live-dashboard (R/httr2; shiny)"
MIN_FETCH_GAP_S <- 20     # shared cache: at most one feed call per 20 s across all users
FORCE_GAP_S     <- 3      # "Refresh now" can't hammer the feed faster than this
GTFS_MAX_AGE_H  <- 24     # re-download static GTFS once a day
HISTORY_KEEP    <- 15     # distinct position snapshots kept for trails / speed estimates
STALE_AFTER_S   <- 180    # "Stale" if positions haven't changed for this long
MAX_SPEED_GAP_S <- 300    # don't estimate speed/heading across gaps longer than this

# NOTE on time: Renfe's header.timestamp is unreliable (on 31 Oct 2026 it read
# 30 Oct 15:41 UTC while positions were still updating). Freshness, history,
# speed and heading therefore use the server time at which the positions were
# first seen to change, not the header. The header is shown for reference only.
TZ              <- "Europe/Madrid"

# Close-approach heuristic: a moving train heading toward a stopped train.
# This is a congestion / bunching indicator, NOT a collision-risk estimate:
# the feed has no track assignment and lines are protected by ETCS/LZB/ASFA.
CA_MIN_SPEED_KMH <- 20    # mover must be doing at least this (estimated)
CA_MAX_DIST_KM   <- 10    # look this far ahead
CA_MIN_GAP_KM    <- 0.15  # closer than this = same spot (coupled sets, duplicates)
CA_CONE_DEG      <- 15    # target must lie within +/- this of the mover's heading
CA_MAX_ETA_MIN   <- 3     # flag if it would reach the target within this time
TARGET_SLOW_KMH  <- 5     # "stopped" also covers trains crawling below this

# Data-anomaly thresholds
MAX_PLAUSIBLE_KMH <- 360  # faster than the fastest Spanish HS service = GPS/feed jump
DUP_SPREAD_KM     <- 1    # same vehicle id reported this far apart
STOPPED_MOVE_KMH  <- 40   # "STOPPED_AT" but moved faster than this
FROZEN_SNAPSHOTS  <- 6    # identical position for this many updates while "in transit"
NETWORK_BBOX      <- c(lat_min = 35, lat_max = 51.5, lon_min = -10, lon_max = 9.6)

CACHE_DIR <- local({
  d <- Sys.getenv("RENFE_CACHE_DIR", "cache")
  ok <- tryCatch({ dir.create(d, showWarnings = FALSE, recursive = TRUE); file.access(d, 2) == 0 },
                 error = function(e) FALSE)
  if (isTRUE(ok)) d else { d <- file.path(tempdir(), "renfe-cache"); dir.create(d, showWarnings = FALSE); d }
})

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

# -----------------------------------------------------------------------------
# Service taxonomy
# -----------------------------------------------------------------------------
SERVICE_LEVELS <- c("AVE", "AVLO", "ALVIA", "AVANT", "EUROMED", "INTERCITY", "MD", "REGIONAL", "OTHER")
SERVICE_LABELS <- c(AVE = "AVE", AVLO = "Avlo", ALVIA = "Alvia", AVANT = "Avant", EUROMED = "Euromed",
                    INTERCITY = "Intercity", MD = "Media Distancia", REGIONAL = "Regional",
                    OTHER = "Other / unmatched")
SERVICE_COLORS <- c(AVE = "#7C83FF", AVLO = "#D16BF0", ALVIA = "#3FB8AF", AVANT = "#5BA8F5",
                    EUROMED = "#F2A65A", INTERCITY = "#E5C453", MD = "#8FD16A", REGIONAL = "#B0B5BD",
                    OTHER = "#6B7079")
HIGH_SPEED     <- c("AVE", "AVLO", "ALVIA", "AVANT", "EUROMED")

STATUS_LABELS <- c(STOPPED_AT = "Stopped at", IN_TRANSIT_TO = "In transit to", INCOMING_AT = "Arriving at")

classify_service <- function(short, long = NA_character_) {
  one <- function(x) {
    x <- toupper(ifelse(is.na(x), "", x))
    fcase(
      grepl("AVLO", x),                                   "AVLO",
      grepl("ALVIA", x),                                  "ALVIA",
      grepl("AVANT", x),                                  "AVANT",
      grepl("EUROMED", x),                                "EUROMED",
      grepl("INTERCITY", x),                              "INTERCITY",
      grepl("(^|[^A-Z])AVE([^A-Z]|$)", x),                "AVE",
      grepl("(^|[^A-Z])MD([^A-Z]|$)|MEDIA DISTANCIA", x), "MD",
      grepl("REGIONAL|REG\\.|R\\.EXP", x),                "REGIONAL",
      default = NA_character_
    )
  }
  out <- one(short)
  miss <- is.na(out)
  out[miss] <- one(long)[miss]
  out[is.na(out)] <- "OTHER"
  out
}

pad_train <- function(x) {
  x <- trimws(x)
  n <- suppressWarnings(as.integer(x))
  ifelse(!is.na(n), formatC(n, width = 5, flag = "0"), x)
}

haversine_km <- function(lat1, lon1, lat2, lon2) {
  r <- pi / 180
  a <- sin((lat2 - lat1) * r / 2)^2 + cos(lat1 * r) * cos(lat2 * r) * sin((lon2 - lon1) * r / 2)^2
  6371.0088 * 2 * asin(pmin(1, sqrt(a)))
}

# Initial bearing (degrees, 0 = north, clockwise) from point 1 to point 2
bearing_deg <- function(lat1, lon1, lat2, lon2) {
  r <- pi / 180
  y <- sin((lon2 - lon1) * r) * cos(lat2 * r)
  x <- cos(lat1 * r) * sin(lat2 * r) - sin(lat1 * r) * cos(lat2 * r) * cos((lon2 - lon1) * r)
  (atan2(y, x) / r + 360) %% 360
}

# -----------------------------------------------------------------------------
# Static GTFS lookup (train number -> service, origin, destination; stop names)
# -----------------------------------------------------------------------------
empty_lookup <- function() {
  list(
    built    = as.POSIXct(NA),
    stops    = data.table(stop_id = character(), stop_name = character()),
    by_trip  = data.table(trip_id = character(), service = character(),
                          origin = character(), destination = character(), route_name = character()),
    by_train = data.table(train_no = character(), service = character(),
                          origin = character(), destination = character(), route_name = character())
  )
}

build_gtfs_lookup <- function(cache_dir) {
  zip_path <- file.path(cache_dir, "google_transit.zip")
  httr2::request(GTFS_URL) |>
    httr2::req_user_agent(UA) |>
    httr2::req_timeout(180) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_perform(path = zip_path)

  ex <- file.path(cache_dir, "gtfs")
  unlink(ex, recursive = TRUE)
  utils::unzip(zip_path, exdir = ex, junkpaths = TRUE)

  rd <- function(f, cols) {
    p <- file.path(ex, f)
    if (!file.exists(p)) return(setnames(data.table(matrix(character(), 0, length(cols))), cols))
    hdr <- names(fread(p, nrows = 0L))
    d <- fread(p, select = intersect(cols, hdr), colClasses = "character",
               encoding = "UTF-8", showProgress = FALSE)
    for (cc in setdiff(cols, names(d))) set(d, j = cc, value = NA_character_)
    d
  }

  stops  <- rd("stops.txt",      c("stop_id", "stop_name"))
  routes <- rd("routes.txt",     c("route_id", "route_short_name", "route_long_name"))
  trips  <- rd("trips.txt",      c("trip_id", "route_id", "trip_short_name"))
  st     <- rd("stop_times.txt", c("trip_id", "stop_id", "stop_sequence"))

  st[, stop_sequence := as.integer(stop_sequence)]
  setorder(st, trip_id, stop_sequence)
  ends <- st[, .(origin_id = stop_id[1L], dest_id = stop_id[.N]), by = trip_id]

  tr <- merge(trips, routes, by = "route_id", all.x = TRUE)
  tr <- merge(tr, ends, by = "trip_id", all.x = TRUE)

  sname <- setNames(stops$stop_name, stops$stop_id)
  tr[, `:=`(
    train_no    = fifelse(!is.na(trip_short_name) & nzchar(trip_short_name),
                          pad_train(trip_short_name), substr(trip_id, 1L, 5L)),
    service     = classify_service(route_short_name, route_long_name),
    origin      = unname(sname[origin_id]),
    destination = unname(sname[dest_id]),
    route_name  = route_long_name
  )]

  cols <- c("service", "origin", "destination", "route_name")
  list(
    built    = Sys.time(),
    stops    = unique(stops, by = "stop_id"),
    by_trip  = unique(tr[, c("trip_id", cols), with = FALSE], by = "trip_id"),
    by_train = tr[, .SD[1L], by = train_no, .SDcols = cols]
  )
}

load_gtfs_lookup <- function(cache_dir = CACHE_DIR) {
  rds <- file.path(cache_dir, "gtfs_lookup.rds")
  if (file.exists(rds) &&
      difftime(Sys.time(), file.mtime(rds), units = "hours") < GTFS_MAX_AGE_H) {
    return(readRDS(rds))
  }
  lk <- tryCatch(build_gtfs_lookup(cache_dir), error = function(e) {
    message("GTFS static download/parse failed: ", conditionMessage(e)); NULL
  })
  if (is.null(lk)) return(if (file.exists(rds)) readRDS(rds) else empty_lookup())
  saveRDS(lk, rds)
  lk
}

# -----------------------------------------------------------------------------
# Realtime feed
#
# Observed quirks (Oct 2026):
#  * tripId = <5-digit train no.><segment digit><service date>, e.g.
#    "1248412026-10-29". Trains that change number en route appear once per
#    segment with identical coordinates, so those rows are collapsed.
#  * Unplanned services look like "SPECIAL_9029720261030".
#  * stopId is sometimes missing on later segments.
#  * The same vehicle id can appear at two different positions; each one is
#    kept as its own marker with a suffixed uid.
# -----------------------------------------------------------------------------
empty_positions <- function() {
  data.table(uid = character(), vehicle_id = character(), train_no = character(),
             trip_id = character(), segments = integer(), special = logical(),
             service_date = character(), status = character(), stop_id = character(),
             lat = numeric(), lon = numeric())
}

fetch_positions <- function(url = FEED_URL) {
  resp <- httr2::request(url) |>
    httr2::req_user_agent(UA) |>
    httr2::req_timeout(20) |>
    httr2::req_retry(max_tries = 3, backoff = function(i) 2 * i) |>
    httr2::req_perform()

  js      <- httr2::resp_body_json(resp, simplifyVector = FALSE)
  feed_ts <- suppressWarnings(as.numeric(js$header$timestamp %||% NA))
  ents    <- js$entity %||% list()
  if (!length(ents)) return(list(data = empty_positions(), feed_ts = feed_ts, n_raw = 0L))

  chr <- function(x) if (is.null(x)) NA_character_ else as.character(x)
  num <- function(x) if (is.null(x)) NA_real_ else as.numeric(x)

  dt <- rbindlist(lapply(ents, function(e) {
    v <- e$vehicle %||% list()
    list(
      trip_id    = chr(v$trip$tripId),
      lat        = num(v$position$latitude),
      lon        = num(v$position$longitude),
      status     = chr(v$currentStatus),
      stop_id    = chr(v$stopId),
      vehicle_id = chr(v$vehicle$id %||% sub("^VP_", "", e$id %||% ""))
    )
  }))
  dt <- dt[!is.na(lat) & !is.na(lon) & lat != 0 & lon != 0 & !is.na(trip_id)]

  dt[, special := startsWith(trip_id, "SPECIAL_")]
  dt[, core := sub("^SPECIAL_", "", trip_id)]
  dt[, train_no := substr(core, 1L, 5L)]
  dt[, service_date := fcase(
    grepl("\\d{4}-\\d{2}-\\d{2}$", core), sub(".*(\\d{4}-\\d{2}-\\d{2})$", "\\1", core),
    grepl("\\d{8}$", core),               sub(".*(\\d{4})(\\d{2})(\\d{2})$", "\\1-\\2-\\3", core),
    default = NA_character_
  )]
  setorder(dt, vehicle_id, trip_id)

  out <- dt[, .(
    train_no     = train_no[1L],
    trip_id      = trip_id[1L],
    segments     = .N,
    special      = special[1L],
    service_date = service_date[1L],
    status       = status[1L],
    stop_id      = stop_id[!is.na(stop_id)][1L]
  ), by = .(vehicle_id, lat = round(lat, 5), lon = round(lon, 5))]

  out[, uid := if (.N == 1L) vehicle_id else paste0(vehicle_id, "-", seq_len(.N)), by = vehicle_id]
  list(data = out[], feed_ts = feed_ts, n_raw = length(ents))
}

enrich <- function(d, lk) {
  if (!nrow(d)) {
    return(d[, `:=`(service = character(), hs = logical(), origin = character(),
                    destination = character(), route_name = character(),
                    stop_name = character(), speed_kmh = numeric())])
  }
  d  <- copy(d)
  i  <- match(d$trip_id, lk$by_trip$trip_id)
  j  <- match(d$train_no, lk$by_train$train_no)
  pick <- function(col) {
    v <- lk$by_trip[[col]][i]
    miss <- is.na(v)
    v[miss] <- lk$by_train[[col]][j][miss]
    v
  }
  d[, `:=`(
    service     = pick("service"),
    origin      = pick("origin"),
    destination = pick("destination"),
    route_name  = pick("route_name"),
    stop_name   = lk$stops$stop_name[match(stop_id, lk$stops$stop_id)]
  )]
  d[is.na(service), service := "OTHER"]
  d[, hs := service %in% HIGH_SPEED]
  d
}

# -----------------------------------------------------------------------------
# Shared state (one process, all sessions): feed cache, GTFS lookup, history
# -----------------------------------------------------------------------------
.state <- new.env(parent = emptyenv())
.state$snap       <- NULL
.state$fetched_at <- as.POSIXct(NA)
.state$last_fp     <- NULL          # fingerprint of the last distinct position set
.state$last_change <- as.POSIXct(NA) # server time when positions last changed
.state$lookup     <- NULL
.state$lookup_at  <- as.POSIXct(NA)
.state$hist       <- data.table(uid = character(), feed_ts = numeric(), lat = numeric(), lon = numeric())

get_lookup <- function() {
  if (is.null(.state$lookup) || is.na(.state$lookup_at) ||
      difftime(Sys.time(), .state$lookup_at, units = "hours") >= GTFS_MAX_AGE_H) {
    .state$lookup    <- load_gtfs_lookup()
    .state$lookup_at <- Sys.time()
  }
  .state$lookup
}

update_history <- function(d, feed_ts) {
  h <- .state$hist
  if (nrow(d) && !is.na(feed_ts) && !(feed_ts %in% h$feed_ts)) {
    h <- rbind(h, d[, .(uid, feed_ts = feed_ts, lat, lon)])
    keep_ts <- head(sort(unique(h$feed_ts), decreasing = TRUE), HISTORY_KEEP)
    h <- h[feed_ts %in% keep_ts]
    .state$hist <- h
  }

  d[, `:=`(speed_kmh = NA_real_, speed_raw = NA_real_, bearing = NA_real_, frozen_n = 0L)]
  if (!nrow(d) || is.na(feed_ts)) return(d)

  # speed + heading versus the previous distinct feed snapshot
  prev_ts <- head(sort(unique(h$feed_ts[h$feed_ts < feed_ts]), decreasing = TRUE), 1L)
  if (length(prev_ts) && (feed_ts - prev_ts) <= MAX_SPEED_GAP_S) {
    p    <- h[feed_ts == prev_ts]
    m    <- match(d$uid, p$uid)
    km   <- haversine_km(p$lat[m], p$lon[m], d$lat, d$lon)
    spd  <- km / ((feed_ts - prev_ts) / 3600)
    spd[!is.finite(spd)] <- NA_real_
    brg  <- bearing_deg(p$lat[m], p$lon[m], d$lat, d$lon)
    brg[is.na(km) | km < 0.05] <- NA_real_          # didn't really move: no heading
    plausible <- !is.na(spd) & spd <= MAX_PLAUSIBLE_KMH
    d[, `:=`(speed_raw = spd,
             speed_kmh = fifelse(plausible, round(spd), NA_real_),
             bearing   = fifelse(plausible, brg, NA_real_))]
  }

  # how many consecutive snapshots (incl. this one) at an identical position
  if (nrow(h)) {
    fr <- h[order(uid, -feed_ts),
            .(frozen_n = {
              same <- lat == lat[1L] & lon == lon[1L]
              if (all(same)) .N else which(!same)[1L] - 1L
            }), by = uid]
    d[, frozen_n := fr$frozen_n[match(uid, fr$uid)]]
    d[is.na(frozen_n), frozen_n := 1L]
  }
  d
}

# Data-quality flags: things that are probably feed/GPS problems, not reality
flag_anomalies <- function(d) {
  if (!nrow(d)) return(d[, `:=`(anomaly = character(), dup_km = numeric())])
  d[, dup_km := if (.N > 1L) max(haversine_km(lat[1L], lon[1L], lat, lon)) else 0, by = vehicle_id]
  b <- NETWORK_BBOX
  parts <- cbind(
    fifelse(!is.na(d$speed_raw) & d$speed_raw > MAX_PLAUSIBLE_KMH,
            sprintf("Implausible jump (%s km/h)", round(d$speed_raw)), NA_character_),
    fifelse(d$lat < b[["lat_min"]] | d$lat > b[["lat_max"]] | d$lon < b[["lon_min"]] | d$lon > b[["lon_max"]],
            "Position outside the network area", NA_character_),
    fifelse(d$dup_km > DUP_SPREAD_KM,
            sprintf("Same vehicle reported %.0f km apart", d$dup_km), NA_character_),
    fifelse(d$status == "STOPPED_AT" & !is.na(d$speed_kmh) & d$speed_kmh >= STOPPED_MOVE_KMH,
            sprintf("Reported stopped but moved at ~%s km/h", d$speed_kmh), NA_character_),
    fifelse(d$status != "STOPPED_AT" & d$frozen_n >= FROZEN_SNAPSHOTS,
            sprintf("Position frozen for %d updates while in transit", d$frozen_n), NA_character_)
  )
  txt <- apply(parts, 1L, function(x) { x <- x[!is.na(x)]; if (length(x)) paste(x, collapse = "; ") else NA_character_ })
  d[, anomaly := txt]
  d
}

# Close approaches: moving train with a stopped/crawling train ahead of it
find_close_approaches <- function(d) {
  empty <- data.table(uid = character(), target_uid = character(), dist_km = numeric(),
                      eta_min = numeric(), lat1 = numeric(), lon1 = numeric(),
                      lat2 = numeric(), lon2 = numeric())
  if (!nrow(d)) return(empty)
  mv <- d[status != "STOPPED_AT" & !is.na(speed_kmh) & !is.na(bearing) &
            speed_kmh >= CA_MIN_SPEED_KMH & is.na(anomaly)]
  tg <- d[(status == "STOPPED_AT" | (!is.na(speed_kmh) & speed_kmh <= TARGET_SLOW_KMH)) & is.na(anomaly)]
  if (!nrow(mv) || !nrow(tg)) return(empty)

  pr <- CJ(i = seq_len(nrow(mv)), j = seq_len(nrow(tg)))
  pr[, `:=`(uid = mv$uid[i], vid = mv$vehicle_id[i], next_stop = mv$stop_id[i],
            spd = mv$speed_kmh[i], brg = mv$bearing[i], lat1 = mv$lat[i], lon1 = mv$lon[i],
            target_uid = tg$uid[j], tvid = tg$vehicle_id[j], tstop = tg$stop_id[j],
            lat2 = tg$lat[j], lon2 = tg$lon[j])]
  pr <- pr[vid != tvid &
             abs(lat2 - lat1) < CA_MAX_DIST_KM / 100 & abs(lon2 - lon1) < CA_MAX_DIST_KM / 75]
  if (!nrow(pr)) return(empty)

  pr[, dist_km := haversine_km(lat1, lon1, lat2, lon2)]
  pr <- pr[dist_km >= CA_MIN_GAP_KM & dist_km <= CA_MAX_DIST_KM]
  pr[, off_deg := abs(((bearing_deg(lat1, lon1, lat2, lon2) - brg + 540) %% 360) - 180)]
  pr <- pr[off_deg <= CA_CONE_DEG]
  pr[, eta_min := dist_km / spd * 60]
  pr <- pr[eta_min <= CA_MAX_ETA_MIN]
  # normal station queueing: the stopped train is at the station the mover is heading to
  pr <- pr[is.na(next_stop) | is.na(tstop) | next_stop != tstop]
  if (!nrow(pr)) return(empty)

  setorder(pr, uid, eta_min)
  pr[, .SD[1L], by = uid][, .(uid, target_uid, dist_km = round(dist_km, 2),
                              eta_min = round(eta_min, 1), lat1, lon1, lat2, lon2)]
}

build_data <- function(raw, feed_ts, lk) {
  d  <- enrich(raw, lk)
  d  <- update_history(d, feed_ts)
  d  <- flag_anomalies(d)
  ca <- find_close_approaches(d)
  d[, `:=`(ca_target = NA_character_, ca_text = NA_character_)]
  if (nrow(ca)) {
    m  <- match(d$uid, ca$uid)
    tm <- match(ca$target_uid[m], d$uid)
    d[, `:=`(
      ca_target = ca$target_uid[m],
      ca_text   = fifelse(is.na(m), NA_character_,
                          sprintf("Stopped %s %s is %.1f km ahead (~%.1f min at current speed)",
                                  SERVICE_LABELS[d$service[tm]], d$train_no[tm],
                                  ca$dist_km[m], ca$eta_min[m]))
    )]
  }
  list(data = d, approaches = ca)
}

get_trails <- function(uids) {
  h <- .state$hist[uid %in% uids]
  setorder(h, uid, feed_ts)
  h
}

get_snapshot <- function(force = FALSE) {
  age <- if (is.na(.state$fetched_at)) Inf else
    as.numeric(difftime(Sys.time(), .state$fetched_at, units = "secs"))
  if (!is.null(.state$snap) && (age < MIN_FETCH_GAP_S && !force || age < FORCE_GAP_S)) {
    return(.state$snap)
  }
  .state$fetched_at <- Sys.time()

  res <- tryCatch(fetch_positions(), error = function(e) e)
  if (inherits(res, "error")) {
    snap <- .state$snap
    if (is.null(snap)) {
      b <- build_data(empty_positions(), NA_real_, empty_lookup())
      snap <- list(data = b$data, approaches = b$approaches,
                   header_ts = NA_real_, changed_at = NA_real_, n_raw = 0L)
    }
    snap$error      <- conditionMessage(res)
    snap$checked_at <- Sys.time()
    .state$snap <- snap
    return(snap)
  }

  # Detect real updates from the content itself (header timestamp is unreliable)
  fp <- if (nrow(res$data)) {
    o <- order(res$data$uid)
    paste(res$data$uid[o], res$data$lat[o], res$data$lon[o], res$data$status[o], collapse = "|")
  } else ""
  if (!identical(fp, .state$last_fp)) {
    .state$last_fp     <- fp
    .state$last_change <- Sys.time()
  }
  obs_ts <- as.numeric(.state$last_change)

  b <- build_data(res$data, obs_ts, get_lookup())
  .state$snap <- list(data = b$data, approaches = b$approaches,
                      header_ts = res$feed_ts, changed_at = obs_ts,
                      n_raw = res$n_raw, checked_at = Sys.time(), error = NULL)
  .state$snap
}

# -----------------------------------------------------------------------------
# Small HTML helpers
# -----------------------------------------------------------------------------
fmt_clock <- function(x) if (is.null(x) || is.na(x)) "n/a" else
  format(as.POSIXct(x, origin = "1970-01-01", tz = TZ), "%H:%M:%S")

fmt_age <- function(secs) {
  if (is.na(secs)) return("n/a")
  secs <- max(0, round(secs))
  if (secs < 90) paste0(secs, " s") else if (secs < 5400) paste0(round(secs / 60), " min")
  else paste0(round(secs / 3600, 1), " h")
}

svc_pill <- function(s) {
  tags$span(class = "svc-pill",
            tags$span(class = "dot", style = sprintf("background:%s", SERVICE_COLORS[[s]])),
            SERVICE_LABELS[[s]])
}

status_html <- function(st) {
  moving <- !identical(st, "STOPPED_AT")
  tags$span(class = "status",
            tags$span(class = if (moving) "sdot moving" else "sdot stopped"),
            if (moving) "Moving" else "Stopped")
}

route_text <- function(o, d) ifelse(!is.na(o) & !is.na(d), paste(o, "→", d), NA_character_)

make_popups <- function(f) {
  esc <- function(x) htmlEscape(ifelse(is.na(x), "n/a", as.character(x)))
  sprintf(
    '<div class="pop">
       <div class="pop-head"><span class="dot" style="background:%s"></span>%s <b>%s</b></div>
       <div class="pop-route">%s</div>
       <table class="pop-tbl">
         <tr><td>Status</td><td>%s %s</td></tr>
         <tr><td>Speed (est.)</td><td>%s</td></tr>
         <tr><td>Service date</td><td>%s</td></tr>
         <tr><td>Trip id</td><td><code>%s</code>%s</td></tr>
         <tr><td>Position</td><td>%.4f, %.4f</td></tr>
       </table>%s%s
     </div>',
    SERVICE_COLORS[f$service], esc(SERVICE_LABELS[f$service]), esc(f$train_no),
    esc(route_text(f$origin, f$destination)),
    esc(STATUS_LABELS[f$status]), esc(fifelse(is.na(f$stop_name), f$stop_id, f$stop_name)),
    ifelse(is.na(f$speed_kmh), "n/a", paste0(f$speed_kmh, " km/h")),
    esc(f$service_date), esc(f$trip_id),
    ifelse(f$segments > 1, sprintf(" <span class='muted'>+%d segment(s)</span>", f$segments - 1L), ""),
    f$lat, f$lon,
    ifelse(is.na(f$ca_text), "",
           sprintf('<div class="flag-box ca"><b>Close approach</b><br>%s</div>', htmlEscape(f$ca_text))),
    ifelse(is.na(f$anomaly), "",
           sprintf('<div class="flag-box anom"><b>Data flag</b><br>%s</div>', htmlEscape(f$anomaly)))
  )
}

flag_badges <- function(ca, anom) {
  tagList(
    if (!is.na(ca))   tags$span(class = "flag-badge ca", title = ca, "Close approach"),
    if (!is.na(anom)) tags$span(class = "flag-badge anom", title = anom, "Data")
  )
}

make_labels <- function(f) {
  lapply(sprintf('<span class="tt"><span class="dot" style="background:%s"></span>%s %s</span>',
                 SERVICE_COLORS[f$service], htmlEscape(SERVICE_LABELS[f$service]), htmlEscape(f$train_no)),
         HTML)
}

# -----------------------------------------------------------------------------
# Linear-style theme
# -----------------------------------------------------------------------------
lin_theme <- bs_theme(
  version   = 5,
  bg        = "#08090a",
  fg        = "#f7f8f8",
  primary   = "#5e6ad2",
  base_font = font_google("Inter", local = FALSE),
  code_font = font_google("JetBrains Mono", local = FALSE),
  "font-size-base" = "0.8125rem",
  "border-radius"  = "6px",
  "border-color"   = "rgba(255,255,255,0.08)"
)

lin_css <- "
:root{
  --bg:#08090a; --panel:#0f1011; --panel-2:#151617; --hover:rgba(255,255,255,.04);
  --border:rgba(255,255,255,.08); --border-strong:rgba(255,255,255,.14);
  --text:#f7f8f8; --muted:#8a8f98; --faint:#62666d; --accent:#5e6ad2; --accent-2:#7170ff;
  --green:#4cb782; --amber:#f2994a; --red:#eb5757;
}
html,body{background:var(--bg);color:var(--text);-webkit-font-smoothing:antialiased;
  font-feature-settings:'cv01','ss03';letter-spacing:-.006em}
.muted{color:var(--muted)} .faint{color:var(--faint)}
code{color:var(--muted);font-size:11px}

/* header */
.navbar,.bslib-page-sidebar>.navbar{background:var(--bg)!important;border-bottom:1px solid var(--border)!important;
  min-height:48px;padding:0 16px}
.app-title{display:flex;align-items:center;gap:10px;font-size:14px;font-weight:600;letter-spacing:-.01em}
.logo{width:18px;height:18px;border-radius:5px;background:linear-gradient(135deg,#7C83FF,#5e6ad2 60%,#3c3f9e);
  box-shadow:0 0 0 1px rgba(255,255,255,.12) inset}
.title-sub{font-weight:400;color:var(--faint);font-size:12px}

/* sidebar */
.bslib-sidebar-layout>.sidebar{background:var(--panel)!important;border-right:1px solid var(--border)!important}
.bslib-sidebar-layout>.sidebar .sidebar-content{padding:14px 14px 20px}
.bslib-sidebar-layout{--_border:var(--border)!important}
.section-label{font-size:11px;font-weight:500;color:var(--faint);letter-spacing:.02em;margin:18px 0 6px}
.section-label:first-child{margin-top:2px}
.form-control,.form-select,.selectize-input{background:var(--panel-2)!important;border:1px solid var(--border)!important;
  color:var(--text)!important;border-radius:6px!important;font-size:13px;min-height:32px;box-shadow:none!important}
.form-control:focus,.form-select:focus,.selectize-input.focus{border-color:var(--accent)!important;
  box-shadow:0 0 0 1px var(--accent)!important}
.form-control::placeholder{color:var(--faint)}
.selectize-dropdown{background:var(--panel-2);border:1px solid var(--border-strong);color:var(--text)}
.selectize-dropdown .active{background:var(--hover);color:var(--text)}
.shiny-input-container{margin-bottom:0;width:100%}
.form-check-input{background-color:var(--panel-2);border-color:var(--border-strong)}
.form-check-input:checked{background-color:var(--accent);border-color:var(--accent)}
.form-check-input:focus{box-shadow:none;border-color:var(--accent)}
.form-switch{padding-top:2px}
.form-switch .form-check-label{color:var(--muted)}

/* service chips */
.chips .shiny-options-group{display:flex;flex-direction:column;gap:1px}
.chips .checkbox,.chips .form-check{margin:0;padding:0}
.chips label{display:flex;align-items:center;gap:8px;width:100%;padding:5px 8px;border-radius:6px;
  color:var(--muted);cursor:pointer;transition:background .12s,color .12s}
.chips label:hover{background:var(--hover);color:var(--text)}
.chips input[type=checkbox]{accent-color:var(--accent);margin:0}
.chips label:has(input:checked){color:var(--text)}
.chips .count{margin-left:auto;color:var(--faint);font-variant-numeric:tabular-nums;font-size:12px}
.seg .shiny-options-group{display:flex;background:var(--panel-2);border:1px solid var(--border);border-radius:6px;padding:2px}
.seg .radio,.seg .form-check{flex:1;margin:0;padding:0}
.seg input{display:none}
.seg label{display:block;text-align:center;padding:4px 0;border-radius:4px;color:var(--muted);cursor:pointer;font-size:12px}
.seg label:has(input:checked){background:rgba(255,255,255,.07);color:var(--text);box-shadow:0 0 0 1px var(--border) inset}

.btn-lin{background:var(--accent)!important;border:0!important;color:#fff!important;border-radius:6px!important;
  font-weight:500;font-size:13px;height:32px;width:100%;margin-top:8px}
.btn-lin:hover{background:var(--accent-2)!important}
.feed-meta{margin-top:14px;padding:10px;border:1px solid var(--border);border-radius:8px;background:var(--panel-2);
  font-size:12px;color:var(--muted);line-height:1.6}
.feed-meta .row-kv{display:flex;justify-content:space-between}
.feed-meta .row-kv span:last-child{color:var(--text);font-variant-numeric:tabular-nums}
.feed-err{margin-top:8px;color:var(--amber)}
.live{display:inline-flex;align-items:center;gap:6px;color:var(--text);font-weight:500}
.live i{width:7px;height:7px;border-radius:50%;background:var(--green);box-shadow:0 0 0 0 rgba(76,183,130,.6);
  animation:pulse 2s infinite}
.live.stale i{background:var(--amber);animation:none}
@keyframes pulse{0%{box-shadow:0 0 0 0 rgba(76,183,130,.55)}70%{box-shadow:0 0 0 7px rgba(76,183,130,0)}
  100%{box-shadow:0 0 0 0 rgba(76,183,130,0)}}

/* main */
.bslib-page-sidebar .main{background:var(--bg)}
.kpi-row{display:grid;grid-template-columns:repeat(6,minmax(0,1fr));gap:10px}
.flag-badge{display:inline-flex;align-items:center;gap:5px;padding:1px 7px;border-radius:999px;font-size:11px;
  margin-right:4px;white-space:nowrap;cursor:help}
.flag-badge:before{content:'';width:6px;height:6px;border-radius:50%;background:currentColor}
.flag-badge.ca{color:var(--red);background:rgba(235,87,87,.10);border:1px solid rgba(235,87,87,.28)}
.flag-badge.anom{color:var(--amber);background:rgba(242,153,74,.10);border:1px solid rgba(242,153,74,.28)}
.flag-box{margin-top:8px;padding:7px 9px;border-radius:6px;font-size:12px;line-height:1.45}
.flag-box.ca{background:rgba(235,87,87,.10);border:1px solid rgba(235,87,87,.28);color:#f3b4b4}
.flag-box.anom{background:rgba(242,153,74,.10);border:1px solid rgba(242,153,74,.28);color:#f6cfa6}
.flag-note{color:var(--faint);font-size:11px;line-height:1.45;margin-top:6px}
.kpi-value .sep{color:var(--faint);font-weight:400;margin:0 4px}
.kpi{background:var(--panel);border:1px solid var(--border);border-radius:10px;padding:11px 14px}
.kpi-label{color:var(--muted);font-size:12px;display:flex;align-items:center;gap:6px}
.kpi-value{font-size:22px;font-weight:600;letter-spacing:-.025em;font-variant-numeric:tabular-nums;margin-top:2px}
.kpi-sub{color:var(--faint);font-size:11px}
.card.lin-card{background:var(--panel);border:1px solid var(--border);border-radius:10px;box-shadow:none;overflow:hidden}
.lin-card>.card-header{background:transparent;border-bottom:1px solid var(--border);font-weight:500;
  font-size:13px;padding:9px 14px;display:flex;justify-content:space-between;align-items:center}
.lin-card .card-body{padding:0}
.bslib-full-screen-enter{color:var(--muted)!important}

/* pills / status */
.dot{width:8px;height:8px;border-radius:50%;display:inline-block;flex:none}
.svc-pill{display:inline-flex;align-items:center;gap:6px;padding:1px 8px 1px 7px;border:1px solid var(--border-strong);
  border-radius:999px;font-size:11px;color:var(--muted);white-space:nowrap}
.train-no{font-variant-numeric:tabular-nums;font-weight:500;margin-left:8px}
.status{display:inline-flex;align-items:center;gap:6px;color:var(--muted)}
.sdot{width:8px;height:8px;border-radius:50%}
.sdot.moving{background:var(--green)}
.sdot.stopped{border:1.5px solid var(--muted)}

/* reactable */
.rt-table{font-size:12.5px}
.rt-th{color:var(--muted)!important;font-weight:500!important;font-size:11.5px}
.rt-tr-group:hover{background:var(--hover)}

/* leaflet */
.leaflet-container{background:#0b0c0d;font-family:inherit}
.osm-dark{filter:invert(1) hue-rotate(180deg) brightness(.82) contrast(.92) saturate(.25)}
.leaflet-control-layers{background:var(--panel-2)!important;color:var(--text)!important;
  border:1px solid var(--border-strong)!important;border-radius:8px!important;box-shadow:none!important}
.leaflet-control-layers-expanded{padding:8px 10px!important;font-size:12px}
.leaflet-control-layers-separator{border-top-color:var(--border)!important}
.leaflet-control-layers input{accent-color:var(--accent)}
.leaflet-control-layers-toggle{filter:invert(1) opacity(.7)}
.leaflet-bar a{background:var(--panel-2)!important;color:var(--text)!important;border-color:var(--border)!important}
.leaflet-control-attribution{background:rgba(8,9,10,.7)!important;color:var(--faint)!important}
.leaflet-control-attribution a{color:var(--muted)!important}
.leaflet-popup-content-wrapper,.leaflet-popup-tip{background:var(--panel-2);color:var(--text);
  border:1px solid var(--border-strong);box-shadow:0 12px 32px rgba(0,0,0,.55)}
.leaflet-popup-content-wrapper{border-radius:10px}
.leaflet-popup-content{margin:12px 14px;font-size:12.5px}
.leaflet-container a.leaflet-popup-close-button{color:var(--muted)}
.leaflet-tooltip{background:var(--panel-2);color:var(--text);border:1px solid var(--border-strong);
  border-radius:6px;box-shadow:none;font-size:12px;padding:3px 8px}
.leaflet-tooltip-top:before,.leaflet-tooltip-bottom:before,.leaflet-tooltip-left:before,.leaflet-tooltip-right:before{display:none}
.tt{display:inline-flex;align-items:center;gap:6px}
.pop-head{display:flex;align-items:center;gap:7px;font-size:13px}
.pop-route{color:var(--muted);margin:3px 0 8px}
.pop-tbl td{padding:2px 0;vertical-align:top}
.pop-tbl td:first-child{color:var(--faint);padding-right:14px;white-space:nowrap}
.legend-lin{background:rgba(15,16,17,.92)!important;border:1px solid var(--border);border-radius:8px;
  color:var(--muted);padding:8px 10px!important;font-size:11.5px;line-height:1.7}
.legend-lin .dot{margin-right:6px}

@media (max-width: 1280px){ .kpi-row{grid-template-columns:repeat(3,minmax(0,1fr))} }
@media (max-width: 992px){ .kpi-row{grid-template-columns:repeat(2,minmax(0,1fr))} }
"

reactable_theme <- reactableTheme(
  color = "#f7f8f8", backgroundColor = "transparent",
  borderColor = "rgba(255,255,255,0.06)", stripedColor = "rgba(255,255,255,0.015)",
  highlightColor = "rgba(255,255,255,0.035)",
  cellPadding = "7px 12px",
  headerStyle = list(background = "#0f1011", borderBottom = "1px solid rgba(255,255,255,0.08)")
)

service_choice_names <- lapply(SERVICE_LEVELS, function(s) {
  tags$span(style = "display:contents",
            tags$span(class = "dot", style = sprintf("background:%s", SERVICE_COLORS[[s]])),
            SERVICE_LABELS[[s]],
            tags$span(class = "count", id = paste0("cnt-", s)))
})

# -----------------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------------
ui <- page_sidebar(
  theme = lin_theme,
  title = tags$div(class = "app-title",
                   tags$span(class = "logo"),
                   "Renfe Live",
                   tags$span(class = "title-sub", "Alta Velocidad · Larga y Media Distancia")),
  window_title = "Renfe Live",
  fillable = TRUE,
  sidebar = sidebar(
    width = 272,
    open  = "desktop",   # open on desktop, closed on mobile (bslib 0.6.x syntax)
    tags$head(
      tags$style(HTML(lin_css)),
      tags$script(HTML("
        Shiny.addCustomMessageHandler('svc-counts', function(m){
          for (const k in m){ const el=document.getElementById('cnt-'+k); if(el) el.textContent = m[k] || ''; }
        });"))
    ),
    tags$div(class = "section-label", "Search"),
    textInput("q", NULL, placeholder = "Train no., station, route…"),

    tags$div(class = "section-label", "Service"),
    input_switch("hs_only", "High-speed only", value = FALSE),
    tags$div(class = "chips",
             checkboxGroupInput("services", NULL, choiceNames = service_choice_names,
                                choiceValues = SERVICE_LEVELS, selected = SERVICE_LEVELS)),

    tags$div(class = "section-label", "Status"),
    tags$div(class = "seg",
             radioButtons("status", NULL, inline = FALSE,
                          choices = c(All = "all", Moving = "moving", Stopped = "stopped"))),

    tags$div(class = "section-label", "Map"),
    input_switch("trails", "Show recent trail", value = TRUE),

    tags$div(class = "section-label", "Flags"),
    input_switch("show_ca", "Close approaches", value = TRUE),
    input_switch("show_anom", "Data anomalies", value = TRUE),
    input_switch("flagged_only", "Flagged trains only", value = FALSE),
    tags$div(class = "flag-note",
             sprintf("Close approach: moving train with a stopped train within %d km ahead, reachable in under %d min. ",
                     CA_MAX_DIST_KM, CA_MAX_ETA_MIN),
             "A congestion indicator, not a collision risk: the feed has no track data and lines are protected by signalling."),

    tags$div(class = "section-label", "Refresh"),
    selectInput("every", NULL, selected = "30",
                choices = c("Every 30 s" = "30", "Every 60 s" = "60",
                            "Every 2 min" = "120", "Manual only" = "0")),
    actionButton("refresh", "Refresh now", class = "btn-lin"),
    uiOutput("feed_meta"),
    tags$div(
      style = "margin-top: auto; padding-top: .75rem; border-top: 1px solid var(--bs-border-color, rgba(128,128,128,.25)); font-size: .75rem; line-height: 1.5; opacity: .8;",
      "Built by ", tags$a(href = "https://jfer2pi.github.io", target = "_blank", rel = "noopener", "Fernando Rodríguez Alvarado"),
      tags$br(),
      tags$a(href = "https://jfer2pi.github.io", target = "_blank", rel = "noopener", "Portfolio"), " · ",
      tags$a(href = "https://www.linkedin.com/in/jfer2pi", target = "_blank", rel = "noopener", "LinkedIn"), " · ",
      tags$a(href = "https://github.com/jfer2pi/tren_es", target = "_blank", rel = "noopener", "Source code")
    )
  ),

  uiOutput("kpis", fill = FALSE),
  layout_columns(
    col_widths = breakpoints(sm = 12, lg = c(8, 4)),
    fill = TRUE, gap = "10px",
    card(
      class = "lin-card", full_screen = TRUE,
      card_header(tags$span("Live map"), tags$span(class = "muted", textOutput("map_count", inline = TRUE))),
      card_body(leafletOutput("map", height = "100%"), fill = TRUE, padding = 0)
    ),
    card(
      class = "lin-card", full_screen = TRUE,
      card_header(tags$span("Trains"), tags$span(class = "faint", "Click a row to locate")),
      card_body(reactableOutput("tbl", height = "100%"), fill = TRUE, padding = 0)
    )
  )
)

# -----------------------------------------------------------------------------
# Server
# -----------------------------------------------------------------------------
server <- function(input, output, session) {

  snap <- reactiveVal(get_snapshot())

  # scheduled refresh (per session; the shared cache keeps feed calls bounded)
  observe({
    secs <- as.numeric(input$every %||% "30")
    if (secs > 0) invalidateLater(secs * 1000)
    snap(get_snapshot())
  })
  observeEvent(input$refresh, snap(get_snapshot(force = TRUE)), ignoreInit = TRUE)

  filtered <- reactive({
    d <- snap()$data
    if (!nrow(d)) return(d)
    svc <- input$services %||% character()
    if (isTRUE(input$hs_only)) svc <- intersect(svc, HIGH_SPEED)
    d <- d[service %in% svc]
    if (identical(input$status, "moving"))  d <- d[status != "STOPPED_AT"]
    if (identical(input$status, "stopped")) d <- d[status == "STOPPED_AT"]
    if (isTRUE(input$flagged_only)) {
      d <- d[(isTRUE(input$show_ca) & !is.na(ca_text)) | (isTRUE(input$show_anom) & !is.na(anomaly))]
    }
    q <- trimws(input$q %||% "")
    if (nzchar(q)) {
      hay <- paste(d$train_no, d$origin, d$destination, d$stop_name, d$route_name,
                   SERVICE_LABELS[d$service], sep = " | ")
      d <- d[grepl(tolower(q), tolower(hay), fixed = TRUE)]
    }
    setorder(d, -hs, service, train_no)
    d
  })

  # per-service counts next to the chips
  observe({
    d <- snap()$data
    cnt <- if (nrow(d)) as.list(table(factor(d$service, SERVICE_LEVELS))) else
      setNames(as.list(rep(0L, length(SERVICE_LEVELS))), SERVICE_LEVELS)
    session$sendCustomMessage("svc-counts", lapply(cnt, function(x) if (x > 0) x else ""))
  })

  # --- KPIs ------------------------------------------------------------------
  output$kpis <- renderUI({
    d <- snap()$data
    f <- filtered()
    kpi <- function(label, value, sub = NULL, dot = NULL)
      tags$div(class = "kpi",
               tags$div(class = "kpi-label",
                        if (!is.null(dot)) tags$span(class = "dot", style = sprintf("background:%s", dot)),
                        label),
               tags$div(class = "kpi-value", value),
               if (!is.null(sub)) tags$div(class = "kpi-sub", sub))
    spd <- f$speed_kmh[f$hs & f$status != "STOPPED_AT"]
    tags$div(class = "kpi-row",
      kpi("Trains live", nrow(d), sprintf("%d shown with current filters", nrow(f))),
      kpi("High-speed", sum(d$hs), "AVE · Avlo · Alvia · Avant · Euromed", dot = SERVICE_COLORS[["AVE"]]),
      kpi("Moving", sum(d$status != "STOPPED_AT"), "In transit or arriving", dot = "#4cb782"),
      kpi("Stopped", sum(d$status == "STOPPED_AT"), "At a station", dot = "#8a8f98"),
      kpi("Flags",
          tagList(sum(!is.na(d$ca_text)), tags$span(class = "sep", "/"), sum(!is.na(d$anomaly))),
          "Close approaches / data anomalies", dot = "#eb5757"),
      kpi("Fastest HS (est.)",
          if (length(spd) && any(!is.na(spd))) paste0(max(spd, na.rm = TRUE), " km/h") else "–",
          "From the last two position updates")
    )
  })

  output$feed_meta <- renderUI({
    invalidateLater(1000)
    s <- snap()
    changed  <- s$changed_at %||% NA_real_
    pos_age  <- if (is.na(changed)) NA else as.numeric(Sys.time()) - changed
    stale    <- !is.null(s$error) || is.na(pos_age) || pos_age > STALE_AFTER_S
    hdr      <- s$header_ts %||% NA_real_
    hdr_txt  <- if (is.na(hdr)) "n/a" else
      format(as.POSIXct(hdr, origin = "1970-01-01", tz = TZ), "%d %b %H:%M")
    tags$div(class = "feed-meta",
      tags$div(class = if (stale) "live stale" else "live", tags$i(), if (stale) "Stale" else "Live"),
      tags$div(class = "row-kv", tags$span("Positions changed"), tags$span(fmt_clock(changed))),
      tags$div(class = "row-kv", tags$span("Since last change"), tags$span(fmt_age(pos_age))),
      tags$div(class = "row-kv", tags$span("Header timestamp"),
               tags$span(class = "faint", title = "Reported by Renfe; often lags, not used for freshness", hdr_txt)),
      tags$div(class = "row-kv", tags$span("Last checked"), tags$span(fmt_clock(as.numeric(s$checked_at %||% NA)))),
      tags$div(class = "row-kv", tags$span("Entities / trains"),
               tags$span(sprintf("%d / %d", s$n_raw %||% 0L, nrow(s$data)))),
      if (!is.null(s$error)) tags$div(class = "feed-err", "Last fetch failed: ", s$error)
    )
  })

  output$map_count <- renderText({
    f <- filtered()
    sprintf("%d trains · %d high-speed", nrow(f), sum(f$hs))
  })

  # --- Map -------------------------------------------------------------------
  output$map <- renderLeaflet({
    legend_html <- paste0(
      vapply(SERVICE_LEVELS, function(s)
        sprintf('<div><span class="dot" style="background:%s"></span>%s</div>',
                SERVICE_COLORS[[s]], SERVICE_LABELS[[s]]), character(1)),
      collapse = "")
    # Open-source basemaps only: OpenStreetMap (ODbL data, standard OSM tiles)
    # plus the OpenRailwayMap overlay (CC-BY-SA). The dark look is a CSS filter
    # on the OSM tiles, so no proprietary tile provider is involved.
    leaflet(options = leafletOptions(preferCanvas = TRUE, zoomControl = TRUE, worldCopyJump = FALSE)) |>
      addTiles(
        urlTemplate = "https://tile.openstreetmap.org/{z}/{x}/{y}.png",
        attribution = '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors',
        options = tileOptions(maxZoom = 19, className = "osm-dark"),
        group = "OSM dark"
      ) |>
      addTiles(
        urlTemplate = "https://tile.openstreetmap.org/{z}/{x}/{y}.png",
        attribution = '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors',
        options = tileOptions(maxZoom = 19),
        group = "OSM light"
      ) |>
      addTiles(
        urlTemplate = "https://{s}.tiles.openrailwaymap.org/standard/{z}/{x}/{y}.png",
        attribution = 'Rail: <a href="https://www.openrailwaymap.org/">OpenRailwayMap</a> (CC-BY-SA)',
        options = tileOptions(maxZoom = 19, opacity = 0.5, subdomains = c("a", "b", "c")),
        group = "Rail network"
      ) |>
      addLayersControl(
        baseGroups    = c("OSM dark", "OSM light"),
        overlayGroups = c("Rail network"),
        position      = "topright",
        options       = layersControlOptions(collapsed = TRUE)
      ) |>
      setView(lng = -3.7, lat = 40.2, zoom = 6) |>
      addControl(HTML(paste0('<div class="legend-lin">', legend_html,
                             '<div class="faint" style="margin-top:4px">Filled = moving · ring = stopped</div></div>')),
                 position = "bottomleft")
  })

  observe({
    f <- filtered()
    p <- leafletProxy("map") |> clearGroup("trails") |> clearGroup("flags") |> clearGroup("trains")
    if (!nrow(f)) return()

    # close approaches: red ring on the mover + dashed line to the stopped train
    if (isTRUE(input$show_ca)) {
      ca <- snap()$approaches
      ca <- ca[uid %in% f$uid]
      if (nrow(ca)) {
        lines <- ca[, .(lon = c(lon1, lon2, NA_real_), lat = c(lat1, lat2, NA_real_)), by = uid]
        p <- p |>
          addPolylines(lng = lines$lon, lat = lines$lat, group = "flags", color = "#eb5757",
                       weight = 2, opacity = 0.9, dashArray = "4,4",
                       options = pathOptions(interactive = FALSE)) |>
          addCircleMarkers(lng = ca$lon1, lat = ca$lat1, group = "flags", radius = 13,
                           color = "#eb5757", weight = 2, opacity = 0.95, fill = FALSE,
                           options = pathOptions(interactive = FALSE)) |>
          addCircleMarkers(lng = ca$lon2, lat = ca$lat2, group = "flags", radius = 11,
                           color = "#eb5757", weight = 1, opacity = 0.6, fill = FALSE,
                           dashArray = "2,3", options = pathOptions(interactive = FALSE))
      }
    }

    # data anomalies: dashed amber ring
    if (isTRUE(input$show_anom)) {
      an <- f[!is.na(anomaly)]
      if (nrow(an)) {
        p <- addCircleMarkers(p, lng = an$lon, lat = an$lat, group = "flags", radius = 11,
                              color = "#f2994a", weight = 1.5, opacity = 0.9, fill = FALSE,
                              dashArray = "3,3", options = pathOptions(interactive = FALSE))
      }
    }

    if (isTRUE(input$trails)) {
      h <- get_trails(f$uid)
      if (nrow(h)) {
        h[, service := f$service[match(uid, f$uid)]]
        for (s in unique(h$service)) {
          segs <- h[service == s, if (.N >= 2L) .(lon = c(lon, NA_real_), lat = c(lat, NA_real_)), by = uid]
          if (nrow(segs)) {
            p <- addPolylines(p, lng = segs$lon, lat = segs$lat, group = "trails",
                              color = SERVICE_COLORS[[s]], weight = 2, opacity = 0.45,
                              options = pathOptions(interactive = FALSE))
          }
        }
      }
    }

    moving <- f$status != "STOPPED_AT"
    cols   <- unname(SERVICE_COLORS[f$service])
    addCircleMarkers(
      p, lng = f$lon, lat = f$lat, layerId = f$uid, group = "trains",
      radius = ifelse(f$hs, 7, 5),
      color = cols, weight = ifelse(moving, 1, 2), opacity = 1,
      fillColor = cols, fillOpacity = ifelse(moving, 0.9, 0.12),
      label = make_labels(f),
      labelOptions = labelOptions(direction = "top", offset = c(0, -6)),
      popup = make_popups(f),
      popupOptions = popupOptions(maxWidth = 320, closeButton = TRUE)
    )
  })

  # --- Table -----------------------------------------------------------------
  output$tbl <- renderReactable({
    f <- filtered()
    td <- f[, .(uid, service, train_no,
                route  = route_text(origin, destination),
                status,
                stop   = fifelse(is.na(stop_name), stop_id, stop_name),
                speed  = speed_kmh,
                ca     = if (isTRUE(input$show_ca)) ca_text else NA_character_,
                anom   = if (isTRUE(input$show_anom)) anomaly else NA_character_)]
    td[, flags := as.integer(!is.na(ca)) * 2L + as.integer(!is.na(anom))]
    setorder(td, -flags)
    reactable(
      td,
      theme = reactable_theme, compact = TRUE, highlight = TRUE, borderless = FALSE,
      pagination = FALSE, height = "100%", defaultSorted = NULL,
      rowStyle = list(cursor = "pointer"),
      onClick = JS("function(rowInfo){ Shiny.setInputValue('sel_uid', rowInfo.original.uid, {priority:'event'}); }"),
      columns = list(
        uid     = colDef(show = FALSE),
        service = colDef(show = FALSE),
        train_no = colDef(name = "Train", minWidth = 150, cell = function(value, index) {
          tagList(svc_pill(td$service[index]), tags$span(class = "train-no", value))
        }),
        route  = colDef(name = "Route", minWidth = 190, na = "–",
                        style = list(color = "#c9ccd1")),
        status = colDef(name = "Status", minWidth = 95, cell = function(value) status_html(value)),
        stop   = colDef(name = "At / next stop", minWidth = 140, na = "–",
                        style = list(color = "#8a8f98")),
        speed  = colDef(name = "km/h", minWidth = 64, align = "right", na = "–",
                        style = list(fontVariantNumeric = "tabular-nums")),
        ca     = colDef(show = FALSE),
        anom   = colDef(show = FALSE),
        flags  = colDef(name = "Flags", minWidth = 150, cell = function(value, index) {
          if (value == 0L) return(tags$span(class = "faint", "–"))
          flag_badges(td$ca[index], td$anom[index])
        })
      )
    )
  })

  observeEvent(input$sel_uid, {
    d <- snap()$data
    r <- d[uid == input$sel_uid]
    if (!nrow(r)) return()
    leafletProxy("map") |>
      flyTo(lng = r$lon[1], lat = r$lat[1], zoom = 11, options = list(duration = 0.8)) |>
      clearPopups() |>
      addPopups(lng = r$lon[1], lat = r$lat[1], popup = make_popups(r[1]))
  })
}

shinyApp(ui, server)
