# parse_sse_event

    Code
      parse_sse_event(charToRaw(txt))
    Output
        foo 
      "bar" 

---

    Code
      parse_sse_event(charToRaw(txt))
    Output
        foo 
      "bar" 

---

    Code
      parse_sse_event(charToRaw(txt))
    Output
           foo      baz      and 
         "bar" "foobar"   "last" 

# chunk_sse_events

    Code
      chunk_sse_events(charToRaw(txt))
    Output
      $events
      list()
      
      $rest
       [1] 66 6f 6f 3a 20 62 61 72 0a 62 61 7a 3a 20 66 6f 6f 0a
      

---

    Code
      chunk_sse_events(charToRaw(txt))
    Output
      $events
      $events[[1]]
           foo      baz 
         "bar" "foobar" 
      
      $events[[2]]
      another 
      "event" 
      
      $events[[3]]
            and 
      "another" 
      
      
      $rest
      raw(0)
      

---

    Code
      chunk_sse_events(charToRaw(txt))
    Output
      $events
      $events[[1]]
           foo      baz 
         "bar" "foobar" 
      
      $events[[2]]
      another 
      "event" 
      
      $events[[3]]
            and 
      "another" 
      
      
      $rest
      raw(0)
      

---

    Code
      chunk_sse_events(charToRaw(txt))
    Output
      $events
      $events[[1]]
           foo      baz 
         "bar" "foobar" 
      
      $events[[2]]
      another 
      "event" 
      
      
      $rest
       [1] 61 6e 64 3a 61 6e 6f 74 68 65 72 0a
      

# sse

    Code
      events
    Output
      [[1]]
                        event                 message 
                          "1" "live long and prosper" 
      
      [[2]]
                        event                 message 
                          "2" "live long and prosper" 
      
      [[3]]
                        event                 message 
                          "3" "live long and prosper" 
      
      [[4]]
                        event                 message 
                          "4" "live long and prosper" 
      
      [[5]]
                        event                 message 
                          "5" "live long and prosper" 
      

