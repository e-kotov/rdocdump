# before release
# usethis::use_version("patch")
usethis::use_tidy_description()
cffr::cff_write()
codemetar::write_codemeta(write_minimeta = T)
# urlchecker::url_check()
# devtools::check(remote = TRUE, manual = TRUE)
# devtools::check_win_devel()
# devtools::check_win_release()
# devtools::check_win_oldrelease()
# foghorn::winbuilder_queue()
# revdepcheck::revdep_check(num_workers = 4)

# devtools::submit_cran()

# usethis::use_github_release()
# usethis::use_dev_version(push = TRUE)
