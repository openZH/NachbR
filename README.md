# Nachbr Toolkit

The "Nachbr" package is built to work with official building permit ("Baupublikationen") data from the canton of Zurich. These data reflect all construction projects for which a building application has been submitted — an obligatory step for any building activity in the canton. Since 2019, such applications have been published exclusively in digital form via the Amtsblatt portal.

The source data is maintained by individual municipalities, which record and submit building applications directly to the [Amtsblattportal](https://amtsblattportal.ch/#!/home). There, the applications are made publicly accessible, typically for up to one year. This toolkit systematically **retrieves**, **processes**, and **structures** these publications so they can be used for **spatial analysis**, **monitoring**, and **visualization**.

> **Note**:
A building application represents a legal notice — it does not guarantee that a project will be carried out as proposed. The toolkit does not track the outcome or execution status of a permit.

## 📎 Important Links
- [API documentation of the Amtsblattportal](https://www.amtsblattportal.ch/docs/api/)
- [Dataset with all building publications since autumn 2024](https://www.zh.ch/de/politik-staat/statistik-daten/datenkatalog.html#/datasets/2982@statistisches-amt-kanton-zuerich) (*updated on a daily basis, a variable description can be found [here](https://github.com/openZH/baupub_OGD/blob/main/Variable-list.md)*)


## 🔧 Installation
```
remotes::install_github("openZH/Nachbr",
                        ref = "main")
```



## ✨ Features

- 📥 Download the official building permit data
- 🔍 Parse and extract building permit details from XML files
- 📦 Export spatialized data to a GeoPackage
- 🗺️ Visualize permits on an interactive Leaflet map
    - 📐 Detect overlapping permit geometries
    - 🧾 Auto-enrich data with addresses, URLs, and deadlines

---

## 🧰 Main Functions Overview
### Retrieving and Processing XML files

#### `get_new_pub_url(page_size, url, params, df_bp)`
Generates a list of XML URLs for newly published permits by comparing against existing IDs. Existing IDs should come from the OGD dataset which will be passed into the function as `df_bp`. If omitted, nly the specified `params` will be used to retrieve data.

- `page_size`: Number of entries to fetch per request (default = 100)
- `url`: API base URL
- `params`: A named list of query parameters (e.g., publication date range, cantons)
- `df_bp`: Existing data frame from OGD distribution (optional)

#### `create_baupub_df(url)`
A wrapper function of `xml_to_df()`. Takes a list of XML URLs, retrieves the XML files (without saving them) and converts them into a cleaned, structured data frame.

- `url`: Vector of XML URLs (output of `get_new_pub_url()`)

##### `xml_to_df(xml)`

⚙️ **Core Function – Not user-facing, but central to the toolkit's architecture.**

Parses a single XML file recursively, extracting and flattening all terminal (leaf) nodes into a structured format. This function is the backbone of the data transformation process used by `create_baupub_df()`.

- `xml`: XML file

> **Note**:
The cadaster numbers are cleaned in this process which are required for geo-referencing the parcels involved in a building project. However, error-free cleaning cannot be guaranteed. These numbers are provided by municipalities and lack a standard format, so some combinations (with the BFS number) may still not match a specific parcel.



#### `download_xml_files(path, params)`
Downloads all available Baupublikationen as XML files from the official API based on the selected parameters.

- `path`: Folder path to store XML files
- `params`: A named list of query parameters (e.g., publication date range, cantons)


### Spatialization and Mapping

#### `get_liegenschaften_layer(file_destination, email_address, retrieval_day)`
Downloads the "Liegenschaften" layer from the [Geo-Portal](https://geo.zh.ch/).

- `file_destination`: Character string indicating the local folder where the shapefile should be saved.
- `email_address`: A valid email address required to authenticate the download request via the GISZH API.
- `retrieval_day`: Weekday (e.g., "Monday") on which the file should be refreshed (the layer is updated weekly).

> **Note**:
Downloading may take up to two hours due to infrastructure constraints at the Geo-Portal.

#### `add_spatial_information(df_bp, sf_liegenschaften)`
Converts a cleaned permit data frame into a spatial layer (sf) and enriches it with address, deadline, and URL information.

- `df_bp`: Cleaned building permit data
- `sf_liegenschaften`: Official Zurich land parcel spatial layer

📌 Output is in coordinate system **EPSG:2056**.


#### `create_map(sf_bp_geo)`
Generates an interactive Leaflet map of permits.
- `sf_bp_geo`: Spatial permit layer (output from add_spatial_information())

🗺️ Returns an interactive map with popups, styled polygons, and zoom-to-fit behavior.



### 🧪 Example Workflow

```

# Step 1: Setup
params <- list(
  publicationStates = "PUBLISHED",
  rubrics = "BP-ZH",
  subrubrics = "BP-ZH01",
  publicationDate.start = "2025-06-01",
  publicationDate.end = as.character(Sys.Date()),
  cantons = "ZH",
  pageRequest.size = 100
  )

url <- "https://amtsblattportal.ch/api/v1/publications/xml?"

# Step 2: get the url of the publications based on parameters
new_urls <-  get_new_pub_url(page_size = 100, url, params)

# Step 3: convert multiple xml to a single data frame
df_bp <- create_baupub_df(new_urls)

# Step 4: Retrieve Liegenschaften-information
sf_liegenschaften <- get_liegenschaften_layer("YOUR_DESTINATION",
                                              "YOUR_NAME@example.com",
                                              "Monday")

# Step 5: Add spatial Information
sf_bp <- df_bp |> 
  add_spatial_information(sf_liegenschaften)

# Step 6: Visualize building projects
sf_bp |> create_map()



```






## 🔁 Replicate OGD dataset
#### `create_OGD_df(days_of_data, base_file_path)`
Downloads and processes building permit publications from the Amtsblatt API. If the OGD dataset exists, new entries are appended; otherwise, a full dataset is created

- `days_of_data`: Number of days to look back (or `"all"` for full history)
- `base_file_path`: Path for local file storage

#### `create_gpkg(gpkg_file, df_bp, sf_liegenschaften)`
Exports building permit data to a **GeoPackage (.gpkg)**. If the file exists, only new entries (based on `id`) are appended.

- `gpkg_file`: Destination path of the `.gpkg` file
- `df_bp`: Data frame of permit data
- `sf_liegenschaften`: Spatial dataset of land parcels for spatial joins. This spatial dataset can be (down)loaded using the function `get_liegenschaften_layer()`.

**>Note**:
Downloading `sf_liegenschaften` may take up to two hours due to infrastructure constraints at the Geo-Portal.

#### `create_map(sf_bp_geo)`
Generates a **Leaflet map** with detailed popups including permit number, date, address, deadlines, and overlapping projects.

- `sf_bp_geo`: The layer written to `baupub.gpkg`

---

### 🧪 Example OGD workflow

```
# Step 1: Setup
base_file_path <- "PATH/TO/YOUR/WORKING_DIRECTORY"
baupub_gpkg_file <- paste0(base_file_path, "gpkg/baupub.gpkg")
liegenschaften_dest <- paste0(base_file_path, "/gpkg")

# Step 2: Download latest building permit publications (here the last 30 days)
df_bp <- create_OGD_df(days_of_data = 30, base_file_path = NULL)

# Step 3: If not already available otherwise, download Liegenschaften-data
sf_liegenschaften <- get_liegenschaften_layer(liegenschaften_dest,
                                              "YOUR.NAME@EXAMPLE.com",
                                              "Monday")

# Step 4: Prepare spatial info (requires sf_liegenschaften shapefile)
sf_bp <- add_spatial_information(df_bp, sf_liegenschaften)

# Step 5: Export to GeoPackage
create_gpkg("baupub.gpkg", df_bp, sf_liegenschaften)

# Step 6: Visualize in an interactive map
create_map(sf_bp)
```





