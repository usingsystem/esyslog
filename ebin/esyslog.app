{application,esyslog,
             [{description,"esyslog"},
              {vsn,"0.2"},
              {modules,[esyslog,esyslog_app,esyslog_sup,esyslog_test]},
              {registered,[esyslog,esyslog_sup]},
              {mod,{esyslog_app,[]}},
              {env,[]},
              {applications,[kernel,stdlib,elog,extlib]}]}.
