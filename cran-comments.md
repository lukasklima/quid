## Fourth Resubmission
This is the fourth resubmission. In this version I have:

* Added references describing the methods of the package to the description field of the DESCRIPTION file.
* Added \\value to .Rd files regarding exported methods and explain the functions results in the documentation. Missing Rd-tags were:

      pipe.Rd: \value
      plotEffects.Rd: \value
      
* \\dontrun{} replaced with with \\donttest{} as the examples take more than 5 seconds even for toy examples.

## Third Resubmission
This is a resubmission. In this version I have:

* Reduced vignette build time by using precomputed results.

## Second Resubmission
This is a resubmission. In this version I have:

* Removed (possibly) invalid URLs from the vignettes.

## First Resubmission
This is a resubmission. In this version I have:

* Removed badges and (possibly) invalid URLs from the vignettes.

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs.

## Downstream dependencies
There are currently no downstream dependencies for this package.
