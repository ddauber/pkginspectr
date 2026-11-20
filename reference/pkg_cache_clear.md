# Clear the cache of downloaded packages

Deletes the cache directory used by `pkginspectr` to store downloaded
CRAN and GitHub packages. This is useful for freeing up disk space or
forcing fresh downloads in future analyses.

## Usage

``` r
pkg_cache_clear(type = c("session", "persistent"))
```

## Arguments

- type:

  A string indicating which cache to clear. Must be one of:

  - `"session"`: temporary cache stored in
    [`tempdir()`](https://rdrr.io/r/base/tempfile.html) (default)

  - `"persistent"`: long-term cache stored in a platform-specific user
    cache directory via `rappdirs::user_cache_dir("pkginspectr")`

## Value

Used for side effects. Invisibly returns the path that was cleared, or
`NULL` if no cache was found.

## Details

You can choose to clear either the session-level cache (temporary files
created during the current R session) or the persistent cache (files
stored across sessions in a user-specific cache directory).

## Examples

``` r
# Clear the cache of the current R session
pkg_cache_clear("session")
#> ℹ No cache found at /tmp/Rtmp3psFpS/pkginspectr

if (FALSE) { # interactive()
# Clear packages cached across R sessions
pkg_cache_clear("persistent")
}
```
