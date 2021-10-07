## Risktable Procam-Score 2002

risktable_procam2002 <- data.frame(points = 20:60, risk = c(1,1.1,1.2,1.3,1.4,1.6,1.7,1.8,1.9,2.3,2.4,2.8,2.9,3.3,3.5,4.0,4.2,4.8,5.1,5.7,6.1,7.0,7.4,
                                                 8.0,8.8,10.2,10.5,10.7,12.8,13.2,15.5,16.8,17.5,19.6,21.7,22.2,23.8,25.1,28.0,29.4,30.0))

#usethis::use_data(risktable_procam2002, overwrite = TRUE ,compress = "xz")


## Risktable Procam-Score 2007


## 10 year Risk

risktable_procam2007_men <- list("20-24" =  data.frame(score = 0:81, risk = rep("0-4%", times = length(0:81))),
                      "25" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:66)), rep("5-9%", times = length(67:81)))),
                      "26" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:63)), rep("5-9%", times = length(64:81)))),
                      "27" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:60)), rep("5-9%", times = length(61:69)), rep("10-19%", times = length(70:81)))),
                      "28" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:57)), rep("5-9%", times = length(58:67)), rep("10-19%", times = length(68:81)))),
                      "29" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:55)), rep("5-9%", times = length(56:64)), rep("10-19%", times = length(65:81)))),
                      "30" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:53)), rep("5-9%", times = length(54:62)), rep("10-19%", times = length(63:81)))),
                      "31" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:51)), rep("5-9%", times = length(52:60)), rep("10-19%", times = length(61:62)), rep("20-29%", times = length(63:81)))),
                      "32" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:49)), rep("5-9%", times = length(50:58)), rep("10-19%", times = length(59:67)), rep("20-29%", times = length(68:81)))),
                      "33" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:47)), rep("5-9%", times = length(48:56)), rep("10-19%", times = length(57:65)), rep("20-29%", times = length(66:81)))),
                      "34" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:45)), rep("5-9%", times = length(46:54)), rep("10-19%", times = length(55:63)), rep("20-29%", times = length(64:69)), rep("=30%", times = length(70:81)))),
                      "35" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:43)), rep("5-9%", times = length(44:52)), rep("10-19%", times = length(53:62)), rep("20-29%", times = length(63:67)), rep("=30%", times = length(68:81)))),
                      "36" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:41)), rep("5-9%", times = length(42:51)), rep("10-19%", times = length(52:60)), rep("20-29%", times = length(61:66)), rep("=30%", times = length(67:81)))),
                      "37" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:40)), rep("5-9%", times = length(41:49)), rep("10-19%", times = length(50:58)), rep("20-29%", times = length(59:64)), rep("=30%", times = length(65:81)))),
                      "38" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:38)), rep("5-9%", times = length(39:48)), rep("10-19%", times = length(49:57)), rep("20-29%", times = length(58:63)), rep("=30%", times = length(64:81)))),
                      "39" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:37)), rep("5-9%", times = length(38:46)), rep("10-19%", times = length(47:55)), rep("20-29%", times = length(56:61)), rep("=30%", times = length(62:81)))),
                      "40" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:35)), rep("5-9%", times = length(36:45)), rep("10-19%", times = length(46:54)), rep("20-29%", times = length(55:60)), rep("=30%", times = length(61:81)))),
                      "41" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:34)), rep("5-9%", times = length(35:43)), rep("10-19%", times = length(44:53)), rep("20-29%", times = length(54:58)), rep("=30%", times = length(59:81)))),
                      "42" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:33)), rep("5-9%", times = length(34:42)), rep("10-19%", times = length(43:51)), rep("20-29%", times = length(52:57)), rep("=30%", times = length(58:81)))),
                      "43" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:31)), rep("5-9%", times = length(32:41)), rep("10-19%", times = length(42:50)), rep("20-29%", times = length(51:56)), rep("=30%", times = length(57:81)))),
                      "44" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:30)), rep("5-9%", times = length(31:39)), rep("10-19%", times = length(40:49)), rep("20-29%", times = length(50:55)), rep("=30%", times = length(56:81)))),
                      "45" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:29)), rep("5-9%", times = length(30:38)), rep("10-19%", times = length(39:48)), rep("20-29%", times = length(49:53)), rep("=30%", times = length(54:81)))),
                      "46" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:28)), rep("5-9%", times = length(29:37)), rep("10-19%", times = length(38:46)), rep("20-29%", times = length(47:52)), rep("=30%", times = length(53:81)))),
                      "47" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:27)), rep("5-9%", times = length(28:36)), rep("10-19%", times = length(37:45)), rep("20-29%", times = length(46:51)), rep("=30%", times = length(52:81)))),
                      "48" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:26)), rep("5-9%", times = length(27:35)), rep("10-19%", times = length(36:44)), rep("20-29%", times = length(45:50)), rep("=30%", times = length(51:81)))),
                      "49" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:25)), rep("5-9%", times = length(26:34)), rep("10-19%", times = length(35:43)), rep("20-29%", times = length(44:49)), rep("=30%", times = length(50:81)))),
                      "50" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:23)), rep("5-9%", times = length(24:33)), rep("10-19%", times = length(34:42)), rep("20-29%", times = length(43:48)), rep("=30%", times = length(49:81)))),
                      "51" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:23)), rep("5-9%", times = length(24:32)), rep("10-19%", times = length(33:41)), rep("20-29%", times = length(42:47)), rep("=30%", times = length(48:81)))),
                      "52" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:22)), rep("5-9%", times = length(23:31)), rep("10-19%", times = length(32:40)), rep("20-29%", times = length(41:46)), rep("=30%", times = length(47:81)))),
                      "53" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:21)), rep("5-9%", times = length(22:30)), rep("10-19%", times = length(31:39)), rep("20-29%", times = length(40:45)), rep("=30%", times = length(46:81)))),
                      "54" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:20)), rep("5-9%", times = length(21:29)), rep("10-19%", times = length(30:38)), rep("20-29%", times = length(39:44)), rep("=30%", times = length(45:81)))),
                      "55" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:19)), rep("5-9%", times = length(20:28)), rep("10-19%", times = length(29:37)), rep("20-29%", times = length(38:43)), rep("=30%", times = length(44:81)))),
                      "56" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:18)), rep("5-9%", times = length(19:27)), rep("10-19%", times = length(28:37)), rep("20-29%", times = length(38:42)), rep("=30%", times = length(43:81)))),
                      "57" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:17)), rep("5-9%", times = length(18:26)), rep("10-19%", times = length(27:36)), rep("20-29%", times = length(37:41)), rep("=30%", times = length(42:81)))),

                      "58" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:16)), rep("5-9%", times = length(17:26)), rep("10-19%", times = length(27:35)), rep("20-29%", times = length(36:41)), rep("=30%", times = length(42:81)))),
                      "59" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:15)), rep("5-9%", times = length(16:25)), rep("10-19%", times = length(26:34)), rep("20-29%", times = length(35:40)), rep("=30%", times = length(41:81)))),
                      "60" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:15)), rep("5-9%", times = length(16:24)), rep("10-19%", times = length(25:33)), rep("20-29%", times = length(34:39)), rep("=30%", times = length(40:81)))),
                      "61" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:14)), rep("5-9%", times = length(15:23)), rep("10-19%", times = length(24:33)), rep("20-29%", times = length(34:38)), rep("=30%", times = length(39:81)))),
                      "62" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:13)), rep("5-9%", times = length(14:22)), rep("10-19%", times = length(23:32)), rep("20-29%", times = length(33:38)), rep("=30%", times = length(39:81)))),
                      "63" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:12)), rep("5-9%", times = length(13:22)), rep("10-19%", times = length(23:31)), rep("20-29%", times = length(32:37)), rep("=30%", times = length(38:81)))),
                      "64" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:12)), rep("5-9%", times = length(13:21)), rep("10-19%", times = length(22:30)), rep("20-29%", times = length(31:36)), rep("=30%", times = length(37:81)))),
                      "65" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:11)), rep("5-9%", times = length(12:20)), rep("10-19%", times = length(21:30)), rep("20-29%", times = length(31:35)), rep("=30%", times = length(36:81)))),
                      "66" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:10)), rep("5-9%", times = length(11:20)), rep("10-19%", times = length(21:29)), rep("20-29%", times = length(30:35)), rep("=30%", times = length(36:81)))),
                      "67" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:10)), rep("5-9%", times = length(11:19)), rep("10-19%", times = length(20:28)), rep("20-29%", times = length(29:34)), rep("=30%", times = length(35:81)))),
                      "68" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:9)), rep("5-9%", times = length(10:18)), rep("10-19%", times = length(19:28)), rep("20-29%", times = length(29:33)), rep("=30%", times = length(34:81)))),
                      "69" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:8)), rep("5-9%", times = length(9:17)), rep("10-19%", times = length(18:27)), rep("20-29%", times = length(28:33)), rep("=30%", times = length(34:81)))),
                      "70" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:8)), rep("5-9%", times = length(9:17)), rep("10-19%", times = length(18:26)), rep("20-29%", times = length(27:32)), rep("=30%", times = length(33:81)))),

                      "71" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:7)), rep("5-9%", times = length(8:16)), rep("10-19%", times = length(17:26)), rep("20-29%", times = length(27:31)), rep("=30%", times = length(32:81)))),
                      "72" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:6)), rep("5-9%", times = length(7:16)), rep("10-19%", times = length(17:25)), rep("20-29%", times = length(26:31)), rep("=30%", times = length(32:81)))),
                      "73" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:6)), rep("5-9%", times = length(7:15)), rep("10-19%", times = length(16:24)), rep("20-29%", times = length(25:30)), rep("=30%", times = length(31:81)))),
                      "74" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:5)), rep("5-9%", times = length(6:14)), rep("10-19%", times = length(15:24)), rep("20-29%", times = length(25:29)), rep("=30%", times = length(30:81)))),
                      "75" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:4)), rep("5-9%", times = length(5:14)), rep("10-19%", times = length(15:23)), rep("20-29%", times = length(24:29)), rep("=30%", times = length(30:81))))
)


risktable_procam2007_women <- list("20-33" =  data.frame(score = 0:81, risk = rep("0-4%", times = length(0:81))),
                        "34" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:69)), rep("5-9%", times = length(70:81)))),
                        "35" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:66)), rep("5-9%", times = length(67:81)))),
                        "36" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:64)), rep("5-9%", times = length(65:81)))),

                        "37" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:62)), rep("5-9%", times = length(63:70)), rep("10-19%", times = length(71:81)))),
                        "38" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:59)), rep("5-9%", times = length(60:68)), rep("10-19%", times = length(69:81)))),
                        "39" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:57)), rep("5-9%", times = length(58:66)), rep("10-19%", times = length(67:81)))),
                        "40" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:55)), rep("5-9%", times = length(56:64)), rep("10-19%", times = length(65:81)))),
                        "41" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:53)), rep("5-9%", times = length(54:62)), rep("10-19%", times = length(63:81)))),

                        "42" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:51)), rep("5-9%", times = length(52:60)), rep("10-19%", times = length(61:69)), rep("20-29%", times = length(70:81)))),
                        "43" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:49)), rep("5-9%", times = length(50:58)), rep("10-19%", times = length(59:67)), rep("20-29%", times = length(68:81)))),
                        "44" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:48)), rep("5-9%", times = length(49:56)), rep("10-19%", times = length(57:65)), rep("20-29%", times = length(66:81)))),

                        "45" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:46)), rep("5-9%", times = length(47:55)), rep("10-19%", times = length(56:64)), rep("20-29%", times = length(65:69)), rep("=30%", times = length(70:81)))),
                        "46" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:44)), rep("5-9%", times = length(45:53)), rep("10-19%", times = length(54:62)), rep("20-29%", times = length(63:69)), rep("=30%", times = length(70:81)))),
                        "47" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:43)), rep("5-9%", times = length(44:51)), rep("10-19%", times = length(52:60)), rep("20-29%", times = length(61:66)), rep("=30%", times = length(67:81)))),
                        "48" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:41)), rep("5-9%", times = length(42:50)), rep("10-19%", times = length(51:59)), rep("20-29%", times = length(60:64)), rep("=30%", times = length(65:81)))),
                        "49" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:39)), rep("5-9%", times = length(40:48)), rep("10-19%", times = length(49:57)), rep("20-29%", times = length(58:62)), rep("=30%", times = length(63:81)))),
                        "50" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:38)), rep("5-9%", times = length(39:47)), rep("10-19%", times = length(48:56)), rep("20-29%", times = length(57:61)), rep("=30%", times = length(62:81)))),
                        "51" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:36)), rep("5-9%", times = length(37:45)), rep("10-19%", times = length(46:54)), rep("20-29%", times = length(55:60)), rep("=30%", times = length(61:81)))),
                        "52" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:35)), rep("5-9%", times = length(36:44)), rep("10-19%", times = length(45:53)), rep("20-29%", times = length(54:58)), rep("=30%", times = length(59:81)))),
                        "53" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:34)), rep("5-9%", times = length(35:42)), rep("10-19%", times = length(43:51)), rep("20-29%", times = length(52:57)), rep("=30%", times = length(58:81)))),
                        "54" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:32)), rep("5-9%", times = length(33:41)), rep("10-19%", times = length(42:50)), rep("20-29%", times = length(51:55)), rep("=30%", times = length(56:81)))),
                        "55" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:31)), rep("5-9%", times = length(32:40)), rep("10-19%", times = length(41:49)), rep("20-29%", times = length(50:54)), rep("=30%", times = length(55:81)))),

                        "56" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:30)), rep("5-9%", times = length(31:39)), rep("10-19%", times = length(40:47)), rep("20-29%", times = length(48:53)), rep("=30%", times = length(54:81)))),
                        "57" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:28)), rep("5-9%", times = length(29:37)), rep("10-19%", times = length(38:46)), rep("20-29%", times = length(47:51)), rep("=30%", times = length(52:81)))),
                        "58" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:27)), rep("5-9%", times = length(28:36)), rep("10-19%", times = length(37:45)), rep("20-29%", times = length(46:50)), rep("=30%", times = length(51:81)))),
                        "59" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:26)), rep("5-9%", times = length(27:35)), rep("10-19%", times = length(36:44)), rep("20-29%", times = length(45:49)), rep("=30%", times = length(50:81)))),
                        "60" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:25)), rep("5-9%", times = length(26:34)), rep("10-19%", times = length(35:42)), rep("20-29%", times = length(43:48)), rep("=30%", times = length(49:81)))),
                        "61" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:24)), rep("5-9%", times = length(25:32)), rep("10-19%", times = length(33:41)), rep("20-29%", times = length(42:47)), rep("=30%", times = length(48:81)))),
                        "62" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:23)), rep("5-9%", times = length(24:31)), rep("10-19%", times = length(32:40)), rep("20-29%", times = length(41:46)), rep("=30%", times = length(47:81)))),
                        "63" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:21)), rep("5-9%", times = length(22:30)), rep("10-19%", times = length(31:39)), rep("20-29%", times = length(40:45)), rep("=30%", times = length(46:81)))),
                        "64" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:20)), rep("5-9%", times = length(21:29)), rep("10-19%", times = length(30:38)), rep("20-29%", times = length(39:43)), rep("=30%", times = length(44:81)))),
                        "65" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:19)), rep("5-9%", times = length(20:28)), rep("10-19%", times = length(29:37)), rep("20-29%", times = length(38:42)), rep("=30%", times = length(43:81)))),
                        "66" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:18)), rep("5-9%", times = length(19:27)), rep("10-19%", times = length(28:36)), rep("20-29%", times = length(37:41)), rep("=30%", times = length(42:81)))),

                        "67" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:17)), rep("5-9%", times = length(18:26)), rep("10-19%", times = length(27:35)), rep("20-29%", times = length(36:40)), rep("=30%", times = length(41:81)))),
                        "68" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:16)), rep("5-9%", times = length(17:25)), rep("10-19%", times = length(26:34)), rep("20-29%", times = length(35:39)), rep("=30%", times = length(40:81)))),
                        "69" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:15)), rep("5-9%", times = length(16:24)), rep("10-19%", times = length(25:33)), rep("20-29%", times = length(34:38)), rep("=30%", times = length(39:81)))),
                        "70" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:14)), rep("5-9%", times = length(15:23)), rep("10-19%", times = length(24:32)), rep("20-29%", times = length(33:37)), rep("=30%", times = length(38:81)))),
                        "71" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:13)), rep("5-9%", times = length(14:22)), rep("10-19%", times = length(23:31)), rep("20-29%", times = length(32:36)), rep("=30%", times = length(37:81)))),
                        "72" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:12)), rep("5-9%", times = length(13:21)), rep("10-19%", times = length(22:30)), rep("20-29%", times = length(31:35)), rep("=30%", times = length(36:81)))),
                        "73" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:12)), rep("5-9%", times = length(13:20)), rep("10-19%", times = length(21:29)), rep("20-29%", times = length(30:35)), rep("=30%", times = length(36:81)))),
                        "74" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:11)), rep("5-9%", times = length(12:19)), rep("10-19%", times = length(20:28)), rep("20-29%", times = length(29:34)), rep("=30%", times = length(35:81)))),
                        "75" = data.frame(score = 0:81, risk = c(rep("0-4%", times = length(0:10)), rep("5-9%", times = length(11:19)), rep("10-19%", times = length(20:27)), rep("20-29%", times = length(28:33)), rep("=30%", times = length(34:81))))
)
#usethis::use_data(risktable_procam2007_men, overwrite = TRUE ,compress = "xz")
#usethis::use_data(risktable_procam2007_women, overwrite = TRUE ,compress = "xz")
