[![Build Status](https://travis-ci.org/adamdsmith/fwspp.png)](https://travis-ci.org/adamdsmith/fwspp)

USFWS Disclaimer
================

This United States Fish & Wildlife Service (USFWS) code is provided on an "as is" basis and the user assumes responsibility for its use. USFWS has relinquished control of the information and no longer has responsibility to protect the integrity , confidentiality, or availability of the information. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by USFWS. The USFWS seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by USFWS or the United States Government.

Important usage limitations/notes
=================================

The `fwspp` package exists strictly to extract occurrence data on USFWS properties, usually National Wildlife Refuges. Attempts to estimate or infer relative abundance are **most strongly discouraged** and **almost certainly meaningless**.

Installing `fwspp`
==================

The `fwspp` package requires [R](https://www.r-project.org/) (&gt;= 3.4) and [Rtools](https://cran.r-project.org/bin/windows/Rtools/). Both will require administrative privileges but the subsequent installation of packages will not.

With R and Rtools installed, it's relatively painless to install and load the `fwspp` package. If you receive an SSL or CA Certificate error, take the extra steps documented below.

    # If devtools package is not installed
    install.packages("devtools", dependencies = TRUE)

    # Now install fwspp
    devtools::install_github("USFWS/fwspp")

    # If you receive a SSL or CA Certificate error
    install.packages("httr")
    library("httr")
    set_config(config(ssl_verifypeer = 0L))
    devtools::install_github("USFWS/fwspp")

    # Now load the functionality
    library("fwspp")

Prior to your first use of `fwspp`, you'll need to install the USFWS Cadastral Geodatabase. You can do this now, using the `install_fws_cadastral` function

    install_fws_cadastral()

or later, when you'll be prompted by the `fws_occ` function.

The `fwspp` package
===================

This packages contains functions to perform geographic query of several biodiversity databases based on U.S. Fish and Wildlife Service (USFWS) administrative or acquisition property boundaries (plus optional buffers). At present, the package queries the [Global Biodiversity Information Facility (GBIF)](http://www.gbif.org/), [Biodiversity Information Serving Our Nation (BISON)](https://bison.usgs.gov/#home), [Integrated Digitized Biocollections (iDigBio)](https://www.idigbio.org/), [VertNet](http://vertnet.org/), the [Berkeley Ecoinformatics Engine](https://ecoengine.berkeley.edu/), and [AntWeb](http://www.antweb.org/). It draws heavily from the outstanding work by the [ROpenScience group](https://ropensci.org/) and their suite of species occurrence packages.

We provide options to:

-   scrub records to reduce the number of returned records for each refuge (see scrubbing details below);
-   update scientific names to their current accepted or valid form based on the [Integrated Taxonomic Information System (ITIS)](http://www.itis.gov) (see taxonomic details below).

Using `fwspp`
=============

Extracting species observation data is essentially a three-step process:

1.  Decide on the USFWS properties to query
2.  Decide on the boundary, scrubbing, and output parameters of that query
3.  Run the query

    Okay, four steps:

4.  Wait... probably a long while...

Step 1 - Find USFWS properties to query
---------------------------------------

The easiest way to generate a list (actually a `data.frame`) of USFWS properties to query is to use the `find_fws` function. With `find_fws` you can restrict your search of USFWS properties to a particular [USFWS region](https://www.fws.gov/where/), particular USFWS property types (see below), and use regular expressions to make the string matching as strict as necessary. Here are a few examples:

``` r
# Get all National Wildlife Refuges (527 as of 2017-11-10) 
# Issues a warning because of two identically-named, but distinct, refuges
all_refs <- find_fws()
#> Warning: Your search returned multiple USFWS properties with the same name.
#>  *  BUFFALO LAKE NATIONAL WILDLIFE REFUGE 
#> To avoid querying an unwanted property, check the output and
#> perhaps specify the `region` argument to avoid unintended
#> behavior.

# Search across all refuges with a partial name match
# Only 1 refuge contains 'longleaf'; Mountain Longleaf NWR
(ml <- find_fws("longleaf"))
#>                                      ORGNAME FWSREGION RSL_TYPE
#> 1 MOUNTAIN LONGLEAF NATIONAL WILDLIFE REFUGE         4      NWR

# Search across all refuges matching multiple strings
(multi <- find_fws(c("longleaf", "romain")))
#>                                      ORGNAME FWSREGION RSL_TYPE
#> 1       CAPE ROMAIN NATIONAL WILDLIFE REFUGE         4      NWR
#> 2 MOUNTAIN LONGLEAF NATIONAL WILDLIFE REFUGE         4      NWR

# Could also have used regular expression
(multi <- find_fws("longleaf|romain"))
#>                                      ORGNAME FWSREGION RSL_TYPE
#> 1       CAPE ROMAIN NATIONAL WILDLIFE REFUGE         4      NWR
#> 2 MOUNTAIN LONGLEAF NATIONAL WILDLIFE REFUGE         4      NWR

# Here's a tricky one - Hatchie NWR
# Simple search returns three refuges with 'hatchie' in name
(hatch <- find_fws("hatchie"))
#>                                  ORGNAME FWSREGION RSL_TYPE
#> 1       HATCHIE NATIONAL WILDLIFE REFUGE         4      NWR
#> 2 LOWER HATCHIE NATIONAL WILDLIFE REFUGE         4      NWR
#> 3  TALLAHATCHIE NATIONAL WILDLIFE REFUGE         4      NWR

# Option 1: select the one you need ad hoc
(hatch <- hatch[1, ])
#>                            ORGNAME FWSREGION RSL_TYPE
#> 1 HATCHIE NATIONAL WILDLIFE REFUGE         4      NWR

# Option 2: regular expressions
(hatch <- find_fws("^hatchie")) # Make name start with 'hatchie'
#>                            ORGNAME FWSREGION RSL_TYPE
#> 1 HATCHIE NATIONAL WILDLIFE REFUGE         4      NWR

# Return all southeast (region 4) refuges
r4_refs <- find_fws(region = 4)
nrow(r4_refs)
#> [1] 130

# Return all mountain-prairie (region 6) refuges and waterfowl production areas
r6_all <- find_fws(ptype = c("NWR", "WPA"), region = 6)
```

### USFWS property types

The `find_fws` function gives you a `ptype` argument to search for several USFWS property types. The most common and default property type is the National Wildlife Refuge (NWR), but other options include Waterfowl Production Areas (WPA), Wildlife Management Areas (WMA), National Fish Hatcheries (NFH), Wildlife Management Areas (WMA), and Farm Service Agency indices (FSA). You can specify multiple options as illustrated in the last example above.

Step 2 - Boundary, scrubbing, and taxonomic decisions
-----------------------------------------------------

### Boundary options

We offer two options for querying the boundaries of refuges and other USFWS properties via the `bnd` argument to the `fws_occ` function. The default `bnd = "admin"` queries those lands and waters **administered** by the USFWS in North America, U.S. Trust Territories and Possessions. It may also include inholdings that are not administered by the USFWS. The primary source for this information is the USFWS Realty program. See <https://ecos.fws.gov/ServCat/Reference/Profile/82894> for more information. Using `bnd = "acq"` queries the external boundaries of lands and waters that are **approved for acquisition** by the USFWS in North America, U.S. Trust Territories and Possessions. See <https://ecos.fws.gov/ServCat/Reference/Profile/82893> for more information.

### Scrubbing options

By default, we scrub **a lot** of records (`scrub = "strict"`). Specifically, we endeavor to retain, for a given geometry, a single record for each species. We attempt to preferentially retain observations with a URL that best substantiates the observation (i.e., the "best" evidence). We rank evidence in the following order: (1) URL to observation with media (photo, audio, video) or the media itself, (2) URL to the observation in the original collection, (3) URL of the collection, with catalog number, or (4) URL of the institution housing the collection. We do not retain any records for which evidence was not available (i.e., no associated collection or catalog number). We optionally offer a less restrictive scrubbing option (`scrub = "moderate"`) that attempts only to eliminate records sharing the same catalog number and redundant observations (i.e., multiple individuals of the same species recorded on the same date at a single location). Users can also disable scrubbing altogether (`scrub = "none"`).

### Taxonomy

By default (`taxonomy = TRUE`), we attempt to validate scientific names against the [Integrated Taxonomic Information System (ITIS)](http://www.itis.gov). It does this not by connecting to ITIS directly, but by requesting information from a REST web service maintained by the National Park Service (NPS) as part of their [NPSpecies database](https://irma.nps.gov/npspecies). Note that this means if taxonomy information is requested, and an ITIS match found, the scientific name will be converted to the "accepted" ITIS scientific name (if it wasn't already), and the corresponding ITIS Taxonomic Serial Number, common names, NPS-specific taxon code, and a general taxa "category" (e.g., Mammals, Birds, Fungi) designated by the NPS are returned. Modifications to observation taxonomy can be suppressed with `taxonomy = FALSE`.

### Other options

By default, the `fws_occ` function makes the query using the actual property boundary, either administrative or acquisition. However, the `buffer` argument provides the option to expand the query of occurrence records beyond the property boundary into a user-specified buffer (in kilometers). This may be useful for very small properties in which species observations in the adjacent areas may provide a good indication of what is present, or expected to be present, within the boundary. By design, `fws_occ` generates a lot of messaging as properties are processed; we like to see that things are moving along. If this annoys you, specify `verbose = FALSE` and enjoy a very slowly updating progress bar and limited messaging.

Step 3 - Run the query
----------------------

With USFWS properties and query options identified, all that's left is to run the `fws_occ` function, passing the object containing the properties to query generated by `find_fws` and specifying any changes from the defaults to the `bnd`, `buffer`, `scrub`, `taxonomy`, and `verbose` options.

Some examples:

    # Query all southeast refuges (NWR only), using defaults
    r4 <- find_fws(region = 4)
    r4_occ <- fws_occ(r4)

    # Query mountain-prairie refuges and waterfowl production areas
    # Suppress most messaging & replace with progress bar
    r6 <- find_fws(region = 6, ptype = c("NWR", "WPA"))
    r6_occ <- fws_occ(r6, verbose = FALSE)

    # Query Key West NWR with 10 km buffer; suppressing taxonomy changes
    kw <- find_fws("key west")
    kw_occ <- fws_occ(kw, buffer = 10, taxonomy = FALSE)

Step 4 - Wait...
----------------

Querying many properties can take hours, particularly if they are relatively large or contain hundreds of thousands of records. Typically the best option is to set `fws_occ` off and running in the background (or overnight) and do something more productive with yourself...
