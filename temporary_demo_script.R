remotes::install_github("MOJN-Veg/mojn-iu-rpackage")

keyring::key_set("AGOL", "mojn_data")  # Run this to save password to keyring - only need to do this once per user per computer

# Get the data
raw_data <- integrateduplands:::fetchRawIU()
raw_data <- integrateduplands:::wrangleIU(raw_data)

# Look at visit table
raw_data$data$Visit
