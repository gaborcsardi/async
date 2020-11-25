
http <- webfakes::new_app_process(
  webfakes::httpbin_app(), opts = webfakes::server_opts(num_threads = 3)
)
