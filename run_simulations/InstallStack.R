

# Core Package Stack for Project ------------------------------------------

# Base EpiModel
install.packages("EpiModel")
install.packages("tidyverse", dep = TRUE)
install.packages("data.table", dep = TRUE)

# Extra Helper Packages
install.packages(c("remotes", "sessioninfo"))

# Fixed Dev Versions of Statnet Packages
remotes::install_github(c("statnet/network@deff2a0",
                          "statnet/networkDynamic@14182bf",
                          "statnet/statnet.common@3307a8c",
                          "statnet/ergm@8b30e92",
                          "statnet/tergm@d3af135"),
                        upgrade = FALSE)

# Latest Dev Versions of EpiModel Packages
remotes::install_github(c("statnet/EpiModel@3914c98",
                          "statnet/EpiModelHPC@257bbf3",
                          "statnet/tergmLite@73d2a2d",
                          "EpiModel/EpiABC@c32ecb6"),
                        upgrade = FALSE)

# Current Version of EpiModelHIV for Project
### NEEDS AUTHORIZATION TOKEN ###
remotes::install_github("gknowlt/EpiModelHIV-p@PrEP-CEA",
                        auth_token = "Your authorization token here", upgrade = FALSE, force = TRUE)


# Package Listing ---------------------------------------------------------

suppressMessages(library("EpiModelHIV"))
options(width = 100)
sessioninfo::package_info(pkgs = c("network", "networkDynamic", "statnet.common",
                                   "ergm", "tergm", "EpiModel", "EpiModelHPC",
                                   "tergmLite", "EpiABC", "EpiModelHIV", "data.table"),
                          dependencies = FALSE)
