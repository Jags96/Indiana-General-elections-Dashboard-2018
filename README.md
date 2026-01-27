# Indiana General Election Results (2018) Dashboard (R Shiny + Docker)

This repository contains an **R Shiny dashboard** for analyzing **Indiana General Election results (2018)**, fully **containerized with Docker** to ensure reproducibility across operating systems and deployment environments.

The application visualizes:

* County-level vote totals
* Democrat vs Republican vote margins
* Office-level comparisons
* Interactive geospatial maps using official Census boundaries

---

## Tech Stack

* **R / Shiny**
* **ggplot2, dplyr, tidyr, bslib**
* **sf, tigris** (geospatial data)
* **plotly** (interactive visualization)
* **Docker (rocker/shiny base image)**

---

## Project Structure

```
.
├── Dockerfile
└── App/
    ├── app.R
    ├── utils.R
    └── 2018-in-precinct-general.csv
    └── R/
        ├── DemoVSRepub_Offices.R
        ├── IndianaMap.R
        └── SummaryPlots.R
└── EDA/
    └── eda1.Rmd

 
```

* `App/` is copied into the container and served by **Shiny Server**
* All paths inside the app are **relative**, enabling container portability

---

## Build and Run (Any OS)

### Prerequisite

* Docker Desktop (macOS / Windows / Linux)

---

### Build the Image

**Apple Silicon (M1/M2/M3):**

```bash
docker build --platform linux/amd64 -t indiana-election-shiny .
```

**Intel macOS / Linux / Windows:**

```bash
docker build -t indiana-election-shiny .
```

---

### Run the Container

```bash
docker run -p 3838:3838 indiana-election-shiny
```

Access the app at:

```
http://localhost:3838/App
```

---

## Docker Design Notes

* Uses `rocker/shiny:4.3` for production-grade Shiny Server
* Installs required **system libraries** for `sf` (`GDAL`, `GEOS`, `PROJ`, `UDUNITS`)
* Fixes file permissions to run as the non-root `shiny` user
* Includes cache directory setup for `tigris`
* No external runtime dependencies required

---

## Application Design Highlights

* **Clear separation of concerns**:

  * Data preparation and aggregation functions in `utils.R`
  * Visualization logic isolated from reactive wiring
* **Reactive pipelines** start from raw data to support future live data integration
* **Global Objects** are also used, if in case the data is static in nature
* **Geospatial joins via FIPS codes** (not string matching)
* **Diverging color scales** to encode party direction and vote margin magnitude
* Designed for extension (modules, caching, CI data sources)


---

## Summary

This project demonstrates:

* Practical **R Shiny dashboard development**
* **Geospatial data handling** with `sf`
* **Containerized deployment** using Docker


