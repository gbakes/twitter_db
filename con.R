library(RPostgres)
con <- dbConnect(RPostgres::Postgres(), dbname = "twitter_malta",
                 host = "46.101.159.150", user = "gbakes",
                 password = "GhawdexMLT478")
