
# 0.8.0.9000

First public release.

* `http_get()`, `http_head()`, `http_post()` and `http_delete()` now retry
  failed requests, configurable via the new `retry` argument. By default
  idempotent requests are retried (up to twice) on transient HTTP status
  codes and connection errors, honoring the `Retry-After` response header.
