Integrated Upland Guide
================
2024-06-24

The goal of the integrated upland package is to provide data QC,
visualization, and publication assistance for MOJN integrated upland
data.

### Installation

The integrated upland package can be found here. If you already have a
GitHub token for RStudio run the code below to download the integrated
upland package. If you haven’t generated a GitHub token for RStudio, you
can follow the guide here to set it up and then run the following code
to download the integrated upland package.

``` r
# install.packages("devtools")
devtools::install_github(repo = "MOJN-Veg/mojn-iu-rpackage")
```

### Metadata and AGOL Password

Before running any of the functions in the package you should check that
the data in you integrated upland AGOL database is filled out.
Instructions for filling out AGOL metadata can be found here.

Before running any of the the functions, it is also recommended that you
save the password to your AGOL headless account using the keyring
package in R. To add headless account information to the default keyring
follow the steps below. The code should only have to be run once.

``` r
# Run the function below. Change the username field to the username of your headless account and input the password when prompted
keyring::key_set(service = "AGOL", username = "USERNAME", prompt = "Password for headless account: ")
# To check the key was saved run code below, filling in the username to your headless account
keyring::key_get(service = "AGOL", username = "USERNAME")
```

### Functions

A short summary of the functions in this package contains. For more
information see function documentation.

- loadAndWrangleIU(): load integrated upland data from AGOL into R and
  perform some basic data wrangling
- writeIU(): write integrated upland data and metadata to CSVs
