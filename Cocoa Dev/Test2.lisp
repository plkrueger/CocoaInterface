(require :build-application)

(ccl::build-application :name "MyApp" 
                        :copy-ide-resources t
                        :directory "ip:Cocoa Dev;")
