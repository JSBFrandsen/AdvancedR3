# AdvancedR3: Super course

This project serves to learn me the basics of reproducible research.

# Brief description of folder and file contents

TODO: As project evolves, add brief description of what is inside the
data, doc and R folders.

The following folders contain:

-   `data/`: a lipidomics dataset that has been cleaned up and in csv
    format.
-   `doc/`: Markdowns to learn about reproducibility.
-   `R/`: R-functions.

# Installing project R package dependencies

If dependencies have been managed by using
`usethis::use_package("packagename")` through the `DESCRIPTION` file,
installing dependencies is as easy as opening the `AdvancedR3.Rproj`
file and running this command in the console:

```         
# install.packages("remotes")
remotes::install_deps()
```

You'll need to have remotes installed for this to work.
