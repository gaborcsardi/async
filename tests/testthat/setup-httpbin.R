
http <- presser::new_app_process(
  presser::httpbin_app(), opts = presser::server_opts(num_threads = 3)
)
