# Parse Remote Reference String

Supports formats:

- "user/repo" -\> GitHub shorthand

- "github::user/repo" -\> Explicit GitHub

- "gitlab::user/repo" -\> GitLab

- "bitbucket::user/repo" -\> Bitbucket

- "user/repo@ref" -\> With commit/branch/tag

- "user/repo/subdir" -\> With subdirectory

- "user/repo/subdir@ref" -\> Combined

- "https://github.com/user/repo/tree/ref/subdir" -\> Web URL

## Usage

``` r
parse_remote_ref(ref)
```

## Arguments

- ref:

  Character string reference

## Value

List with components: type, user, repo, ref, subdir
