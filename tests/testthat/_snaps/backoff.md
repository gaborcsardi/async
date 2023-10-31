# fail, success

    Code
      synchronise(async_backoff(uns, custom_backoff = bo))
    Message
      not yet
      not yet
      not yet
    Output
      [1] "answer"

---

    Code
      synchronise(async_backoff(uns, custom_backoff = bo, times = 2))
    Message
      not yet
      not yet
    Condition
      Error:
      ! not yet

---

    Code
      synchronise(async_backoff(uns, custom_backoff = bo, time_limit = 1))
    Message
      not yet
      not yet
      not yet
    Output
      [1] "answer"

---

    Code
      synchronise(async_backoff(uns2, custom_backoff = bo, time_limit = 0.1))
    Condition
      Error in `uns()`:
      ! not yet

# progress

    Code
      synchronise(async_backoff(uns, custom_backoff = bo, on_progress = progress,
        progress_data = "data"))
    Message
      not yet
    Output
      $event
      [1] "retry"
      
      $tries
      [1] 1
      
      $spent
      Time difference of <some> secs
      
      $error
      <simpleError: not yet>
      
      $retry_in
      [1] 0.1
      
      [1] "data"
    Message
      not yet
    Output
      $event
      [1] "retry"
      
      $tries
      [1] 2
      
      $spent
      Time difference of <some> secs
      
      $error
      <simpleError: not yet>
      
      $retry_in
      [1] 0.1
      
      [1] "data"
    Message
      not yet
    Output
      $event
      [1] "retry"
      
      $tries
      [1] 3
      
      $spent
      Time difference of <some> secs
      
      $error
      <simpleError: not yet>
      
      $retry_in
      [1] 0.1
      
      [1] "data"
      [1] "answer"

---

    Code
      synchronise(async_backoff(uns, custom_backoff = bo, times = 2, on_progress = progress,
        progress_data = "data"))
    Message
      not yet
    Output
      $event
      [1] "retry"
      
      $tries
      [1] 1
      
      $spent
      Time difference of <some> secs
      
      $error
      <simpleError: not yet>
      
      $retry_in
      [1] 0.1
      
      [1] "data"
    Message
      not yet
    Output
      $event
      [1] "givenup"
      
      $tries
      [1] 2
      
      $spent
      Time difference of <some> secs
      
      $error
      <simpleError: not yet>
      
      $retry_in
      [1] NA
      
      [1] "data"
    Condition
      Error:
      ! not yet

# HTTP backoff example

    Code
      uniq(messages)
    Output
      [1] "http://127.0.0.1<port>/unstable: got 0/0"                         
      [2] "http://127.0.0.1<port>/unstable failed, retry in 0.100000 seconds"
      [3] "http://127.0.0.1<port>/unstable: got 0/0"                         
      [4] "http://127.0.0.1<port>/unstable failed, retry in 0.100000 seconds"
      [5] "http://127.0.0.1<port>/unstable: got 0/0"                         
      [6] "http://127.0.0.1<port>/unstable failed, retry in 0.100000 seconds"
      [7] "http://127.0.0.1<port>/unstable: got 0/0"                         
      [8] "http://127.0.0.1<port>/unstable: got 215/215"                     

