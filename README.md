# NEON Breeding Bird Explorer

An (unofficial) R/Shiny explorer for NEON's **Breeding landbird point counts**
(**DP1.10003.001**) across **46 NEON sites**, from arctic tundra (Utqiaġvik, −2 °C) to
Caribbean dry forest (Guánica, 26 °C) — a *NEONize* sibling of the Small Mammal Tracker,
built to the same Desert Data Labs quality bar, in a warm **"Field Guide"** theme
(parchment + ink, Fraunces serif, dawn-sky hero) distinct from the mammal app's house style.

🌐 **Landing page** → <https://tgilbert14.github.io/NEON-Breeding-Birds/> · 🚀 **Live app** → <https://019ee116-75d9-5940-8ccd-9b8c7afabce4.share.connect.posit.cloud/> · ▶ Run locally → `shiny::runApp(".")`

> The unit is a **species detected at a site** (community grain, with detection). The honesty
> backbone: raw point-count totals are *detection-confounded* — a loud species and a quiet one
> at equal density give unequal counts — so the abundance axis is a **detection index** (birds
> per point-count), never a "population." `observerDistance` powers each species' detection-decay.

## Tabs
- **Overview** — most-detected species (coloured by how they're first detected: singing/calling/visual), the story so far, and a **seasonal-context** panel placing the breeding-count window on the site's green-up + temperature year (co-located NEON phenology/temperature — context, *not* a bird-vs-environment driver model).
- **Community** — species accumulation by **point-counts** (point × year occasion) + a **Chao2** estimate of how many species really use the site (point counts miss nocturnal/secretive/rare birds).
- **Bird Board** (flagship) — every species as a dot: **ubiquity** (% of points where detected — a *less count-biased* axis than the index, though still a detection floor) × **detection index**. Tap to pin a card; faint dots are too few detections to place.
- **Across the continent** (flagship) — every NEON site as a dot in climate space: **breeding-season temperature** (all 46 sites; precipitation toggle gated to the 19 with a gauge) × a bird-community metric (richness **rarefied to a common number of point-counts**, Hill q1, mean ubiquity, …). Coloured by biome, sized by effort. Tap a site to pin its card or jump to it. Space-for-time, correlational — stated on the chart.
- **Species Profile** — a downloadable card (PNG + CSV): detection index, ubiquity, points/grids, the **area-corrected detectability-by-distance** (detections per hectare per ring — a raw count would rise then fall on annulus geometry alone), yearly counts.
- **Map** — point-count grids, sized by richness.

## Run it
R 4.5.x, bundle-only: `shiny::runApp(".", port = 8192)`. Splash leads with a national map picker (46 sites, coloured by biome). The default site is **CLBJ** (LBJ National Grassland, Texas oak savanna), the richest site in the set and one with a stable Chao2 estimate, so the first numbers a new user meets are honest.

## Data
Per-site `data/sites/<SITE>.rds` = `list(obs, points, meta)`. `obs` = one row per detection
(`pointkey, scientificName, vernacularName, observerDistance, detectionMethod, clusterSize, …`);
`points` = per point (`nlcdClass, lat, lng, n_visits` = effort); abundance index = sum(clusterSize) / point-visits.
Co-located monthly **environment** per site in `data/env/<SITE>.rds` (precip/temp/phenology, 2013–present);
precomputed cross-site tables `data/site_climate.rds`, `data/site_month_clim.rds`, `data/cross_site.rds`
(effort-rarefied richness, coverage, Hill numbers) feed the climate tab at boot.

### Rebuild
1. `Rscript-4.1.1 scripts/fetch_bird_all.R` (all 46 sites) · 2. `Rscript scripts/bundle_bird_data.R` ·
3. `Rscript scripts/refresh_site_climate.R` (climate + monthly climatology) · 4. `Rscript scripts/build_cross_site.R` (rarefied cross-site metrics).
Environment overlays are built by `../App-NEON-Small-Mammal-Tracker/scripts/refresh_env_data.R` and copied to `data/env/`.

## Honesty notes
Detection index ≠ population (labelled everywhere); ubiquity (incidence) is *less count-biased* but still a detection floor (naïve occupancy, uncorrected for imperfect detection — not "the least-biased axis"); **Chao2 and species accumulation use the point × year *occasion* as the incidence unit** (a point's yearly revisits are not pooled as separate places — avoids the pseudoreplication that inflates richness); the **cross-site climate gradient is space-for-time** (46 places observed at once, not one site warming) and **richness is rarefied to a common number of point-counts** because raw richness tracks effort (sites differ 13–144 points) — both stated on the chart; precipitation is shown only for the 19 sites with a NEON gauge, never imputed; the within-site seasonal panel is **context, not a driver model** (counts run only 1–2×/yr, so there's no within-season bird trend to correlate); the detectability-by-distance panel is **area-corrected** (detections/ha per ring), since a raw point-count histogram rises with distance on annulus geometry; the effort denominator is total point-visits from the structural effort table (never `obs$eventID`, a different grain); lat/long are grid centroids (the map aggregates to grid); the basic package has no family/native-status, so the board colours by detection method. Built by Desert Data Labs · desertdatalabs@gmail.com. Not affiliated with NEON/Battelle/NSF.
