## Test environments

* local Windows 10 install, R 4.2.0
* devtools::check_rhub()
* devtools::check_win_release()
* devtools::check_win_devel()
* GitHub Actions for windows-latest (release), macOS-latest (release), ubuntu-latest (release), ubuntu-latest (devel), ubuntu-latest (oldrel-1)


## R CMD check results

There were no ERRORs or WARNINGs.

There were two NOTEs:

* New submission

This is indeed a new submission.

* Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.1214/088342304000000396
    From: inst/doc/nguyen.html
          README.md
    Status: 500
    Message: Internal Server Error
  URL: https://doi.org/10.2307/2532852
    From: inst/doc/nguyen.html
    Status: 403
    Message: Forbidden

These URLs are valid.


## Downstream dependencies

There are currently no downstream dependencies for this package.
