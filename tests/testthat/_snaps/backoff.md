# fail, success

    Code
      synchronise(async_backoff(uns, backoff = bo))
    Message <simpleMessage>
      not yet
      not yet
      not yet
    Output
      [1] "answer"

---

    Code
      synchronise(async_backoff(uns, backoff = bo, times = 2))
    Message <simpleMessage>
      not yet
      not yet
    Error <async_rejected>
      not yet

---

    Code
      synchronise(async_backoff(uns, backoff = bo, time_limit = 1))
    Message <simpleMessage>
      not yet
      not yet
      not yet
    Output
      [1] "answer"

---

    Code
      synchronise(async_backoff(uns2, backoff = bo, time_limit = 0.1))
    Error <async_rejected>
      not yet

