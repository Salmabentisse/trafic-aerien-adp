library(DBI)
library(RMySQL)

con <- dbConnect(
  MySQL(),
  host     = Sys.getenv("DB_HOST"),
  port     = as.integer(Sys.getenv("DB_PORT")),
  dbname   = Sys.getenv("DB_NAME"),
  user     = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASS")
)

# Test simple : lister les tables (vide pour l'instant, c'est normal)
dbListTables(con)

# Test d'écriture : créer une petite table de test
dbExecute(con, "CREATE TABLE test_connexion (id INT, msg VARCHAR(50));")
dbExecute(con, "INSERT INTO test_connexion VALUES (1, 'Connexion réussie !');")
dbGetQuery(con, "SELECT * FROM test_connexion;")

# Nettoyage du test
dbExecute(con, "DROP TABLE test_connexion;")

