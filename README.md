# NEON Breeding Bird Explorer

An (unofficial) R/Shiny explorer for NEON's **Breeding landbird point counts**
(**DP1.10003.001**) — a *NEONize* sibling of the Small Mammal Tracker, built to the same
Desert Data Labs quality bar.

> The unit is a **species detected at a site** (community grain, with detection). The honesty
> backbone: raw point-count totals are *detection-confounded* — a loud species and a quiet one
> at equal density give unequal counts — so the abundance axis is a **detection index** (birds
> per point-count), never a "population." `observerDistance` powers each species' detection-decay.

## Tabs
- **Overview** — most-detected species (coloured by how they're first detected: singing/calling/visual), the story so far.
- **Community** — species accumulation by points counted + a **Chao2** estimate of how many species really use the site (point counts miss nocturnal/secretive/rare birds).
- **Bird Board** (flagship) — every species as a dot: **ubiquity** (% of points where detected — a *less count-biased* axis than the index, though still a detection floor) × **detection index**. Tap to pin a card; faint dots are too few detections to place.
- **Species Profile** — a downloadable card (PNG + CSV): detection index, ubiquity, points/grids, the **area-corrected detectability-by-distance** (detections per hectare per ring — a raw count would rise then fall on annulus geometry alone), yearly counts.
- **Map** — point-count grids, sized by richness.

## Run it
R 4.5.x, bundle-only: `shiny::runApp(".", port = 8192)`. Demo = **HARV** (Harvard Forest — ovenbirds, vireos, veeries). Also bundled: **SCBI**, **WOOD** (prairie — 133 species, red-winged blackbird).

## Data
Per-site `data/sites/<SITE>.rds` = `list(obs, points, meta)`. `obs` = one row per detection
(`pointkey, scientificName, vernacularName, observerDistance, detectionMethod, clusterSize, …`);
`points` = per point (`nlcdClass, lat, lng, n_visits` = effort); abundance index = sum(clusterSize) / point-visits.

### Rebuild
1. `Rscript-4.1.1 ../App-NEON-Small-Mammal-Tracker/scripts/fetch_bird_demo.R`  2. `Rscript scripts/bundle_bird_data.R`

## Honesty notes
Detection index ≠ population (labelled everywhere); ubiquity (incidence) is *less count-biased* but still a detection floor (naïve occupancy, uncorrected for imperfect detection — not "the least-biased axis"); Chao2 is incidence-based and pools incidence over all monitored years (captioned); the detectability-by-distance panel is **area-corrected** (detections/ha per ring), since a raw point-count histogram rises with distance on annulus geometry; the effort denominator is total point-visits from the structural effort table (never `obs$eventID`, a different grain); lat/long are grid centroids (the map aggregates to grid); the basic package has no family/native-status, so the board colours by detection method. Built by Desert Data Labs · desertdatalabs@gmail.com. Not affiliated with NEON/Battelle/NSF.
