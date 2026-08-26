# ============================================================
# Projet Trafic Aérien - Mission 2 : Création du schéma MySQL
# ============================================================

library(DBI)
library(RMySQL)

# --- Connexion à MySQL (variables définies dans .Renviron) ---
con <- dbConnect(
  MySQL(),
  host     = Sys.getenv("DB_HOST"),
  port     = as.integer(Sys.getenv("DB_PORT")),
  dbname   = Sys.getenv("DB_NAME"),
  user     = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASS")
)

# ---- Helpers -----------------------------------------------
run_sql <- function(con, sql, label = "") {
  tryCatch({
    dbExecute(con, sql)
    if (nchar(label) > 0) cat("[OK]", label, "\n")
  }, error = function(e) {
    cat("[ERREUR]", label, "->", conditionMessage(e), "\n")
  })
}

# ============================================================
# 1. Création de la base de données (idempotent)
# ============================================================
run_sql(con,
  "CREATE DATABASE IF NOT EXISTS trafic_aerien_adp
     CHARACTER SET utf8mb4
     COLLATE utf8mb4_unicode_ci;",
  "CREATE DATABASE trafic_aerien_adp"
)

run_sql(con, "USE trafic_aerien_adp;", "USE DATABASE")

# ============================================================
# 2. Suppression des tables (ordre inverse des dépendances FK)
# ============================================================
for (tbl in c("flights", "weather", "planes", "airports", "airlines")) {
  run_sql(con,
    paste0("DROP TABLE IF EXISTS ", tbl, ";"),
    paste("DROP TABLE", tbl)
  )
}

# ============================================================
# 3. Création des tables (ordre des dépendances FK)
# ============================================================

# --- 3.1 airlines -------------------------------------------
run_sql(con,
  "CREATE TABLE airlines (
     carrier  VARCHAR(2)   NOT NULL,
     name     VARCHAR(255) NOT NULL,
     CONSTRAINT pk_airlines PRIMARY KEY (carrier)
   ) ENGINE=InnoDB;",
  "CREATE TABLE airlines"
)

# --- 3.2 airports -------------------------------------------
run_sql(con,
  "CREATE TABLE airports (
     faa    VARCHAR(3)    NOT NULL,
     name   VARCHAR(255),
     lat    DECIMAL(10,6),
     lon    DECIMAL(10,6),
     alt    INT,
     tz     SMALLINT,
     dst    CHAR(1),
     tzone  VARCHAR(50),
     CONSTRAINT pk_airports PRIMARY KEY (faa),
     CONSTRAINT chk_dst CHECK (dst IN ('A','U','N') OR dst IS NULL)
   ) ENGINE=InnoDB;",
  "CREATE TABLE airports"
)

# --- 3.3 planes ---------------------------------------------
run_sql(con,
  "CREATE TABLE planes (
     tailnum      VARCHAR(10) NOT NULL,
     year         SMALLINT UNSIGNED,
     type         VARCHAR(100),
     manufacturer VARCHAR(100),
     model        VARCHAR(100),
     engines      TINYINT UNSIGNED,
     seats        SMALLINT UNSIGNED,
     speed        SMALLINT UNSIGNED,
     engine       VARCHAR(50),
     CONSTRAINT pk_planes PRIMARY KEY (tailnum)
   ) ENGINE=InnoDB;",
  "CREATE TABLE planes"
)

# --- 3.4 weather --------------------------------------------
run_sql(con,
  "CREATE TABLE weather (
     id         INT UNSIGNED  NOT NULL AUTO_INCREMENT,
     origin     VARCHAR(3)    NOT NULL,
     year       SMALLINT      NOT NULL,
     month      TINYINT       NOT NULL,
     day        TINYINT       NOT NULL,
     hour       TINYINT       NOT NULL,
     temp       DECIMAL(6,2),
     dewp       DECIMAL(6,2),
     humid      DECIMAL(5,2),
     wind_dir   SMALLINT UNSIGNED,
     wind_speed DECIMAL(7,2),
     wind_gust  DECIMAL(7,2),
     precip     DECIMAL(5,2),
     pressure   DECIMAL(7,2),
     visib      DECIMAL(5,2),
     time_hour  DATETIME      NOT NULL,
     CONSTRAINT pk_weather    PRIMARY KEY (id),
     CONSTRAINT uq_weather    UNIQUE      (origin, year, month, day, hour),
     CONSTRAINT fk_weather_origin
       FOREIGN KEY (origin) REFERENCES airports (faa)
       ON UPDATE CASCADE ON DELETE RESTRICT,
     CONSTRAINT chk_month  CHECK (month  BETWEEN 1 AND 12),
     CONSTRAINT chk_day    CHECK (day    BETWEEN 1 AND 31),
     CONSTRAINT chk_hour   CHECK (hour   BETWEEN 0 AND 23),
     CONSTRAINT chk_humid  CHECK (humid  IS NULL OR (humid  >= 0 AND humid  <= 100)),
     CONSTRAINT chk_visib  CHECK (visib  IS NULL OR visib  >= 0)
   ) ENGINE=InnoDB;",
  "CREATE TABLE weather"
)

# --- 3.5 flights --------------------------------------------
run_sql(con,
  "CREATE TABLE flights (
     id             INT UNSIGNED NOT NULL AUTO_INCREMENT,
     year           SMALLINT     NOT NULL,
     month          TINYINT      NOT NULL,
     day            TINYINT      NOT NULL,
     dep_time       SMALLINT UNSIGNED,
     sched_dep_time SMALLINT UNSIGNED NOT NULL,
     dep_delay      SMALLINT,
     arr_time       SMALLINT UNSIGNED,
     sched_arr_time SMALLINT UNSIGNED NOT NULL,
     arr_delay      SMALLINT,
     carrier        VARCHAR(2)   NOT NULL,
     flight         SMALLINT UNSIGNED NOT NULL,
     tailnum        VARCHAR(10),
     origin         VARCHAR(3)   NOT NULL,
     dest           VARCHAR(3)   NOT NULL,
     air_time       SMALLINT,
     distance       INT UNSIGNED,
     hour           TINYINT,
     minute         TINYINT,
     time_hour      DATETIME     NOT NULL,
     CONSTRAINT pk_flights PRIMARY KEY (id),
     CONSTRAINT fk_flights_carrier
       FOREIGN KEY (carrier) REFERENCES airlines (carrier)
       ON UPDATE CASCADE ON DELETE RESTRICT,
     CONSTRAINT fk_flights_tailnum
       FOREIGN KEY (tailnum) REFERENCES planes (tailnum)
       ON UPDATE CASCADE ON DELETE SET NULL,
     CONSTRAINT fk_flights_origin
       FOREIGN KEY (origin) REFERENCES airports (faa)
       ON UPDATE CASCADE ON DELETE RESTRICT,
     CONSTRAINT fk_flights_dest
       FOREIGN KEY (dest)   REFERENCES airports (faa)
       ON UPDATE CASCADE ON DELETE RESTRICT,
     CONSTRAINT chk_flights_month   CHECK (month  BETWEEN 1 AND 12),
     CONSTRAINT chk_flights_day     CHECK (day    BETWEEN 1 AND 31),
     CONSTRAINT chk_flights_hour    CHECK (hour   IS NULL OR hour   BETWEEN 0 AND 23),
     CONSTRAINT chk_flights_minute  CHECK (minute IS NULL OR minute BETWEEN 0 AND 59)
   ) ENGINE=InnoDB;",
  "CREATE TABLE flights"
)

# ============================================================
# 4. Index d'optimisation (au-delà des FK auto-indexées)
# ============================================================

# flights : requêtes fréquentes par date, par délai, par destination
run_sql(con, "CREATE INDEX idx_flights_date     ON flights (year, month, day);",         "INDEX flights(date)")
run_sql(con, "CREATE INDEX idx_flights_dest     ON flights (dest);",                     "INDEX flights(dest)")
run_sql(con, "CREATE INDEX idx_flights_dep_delay ON flights (dep_delay);",               "INDEX flights(dep_delay)")
run_sql(con, "CREATE INDEX idx_flights_arr_delay ON flights (arr_delay);",               "INDEX flights(arr_delay)")
run_sql(con, "CREATE INDEX idx_flights_time_hour ON flights (time_hour);",               "INDEX flights(time_hour)")

# weather : jointure météo-vol sur origin+time_hour
run_sql(con, "CREATE INDEX idx_weather_time_hour ON weather (origin, time_hour);",       "INDEX weather(origin, time_hour)")

# airports : recherche par nom
run_sql(con, "CREATE INDEX idx_airports_name    ON airports (name(50));",                "INDEX airports(name)")

# ============================================================
# 5. Vérification du schéma créé
# ============================================================
cat("\n=== Tables créées ===\n")
tables <- dbGetQuery(con, "SHOW TABLES;")
print(tables)

cat("\n=== Colonnes et clés : flights ===\n")
print(dbGetQuery(con, "DESCRIBE flights;"))

cat("\n=== Clés étrangères actives ===\n")
fkeys <- dbGetQuery(con, "
  SELECT TABLE_NAME, CONSTRAINT_NAME, COLUMN_NAME,
         REFERENCED_TABLE_NAME, REFERENCED_COLUMN_NAME
  FROM   information_schema.KEY_COLUMN_USAGE
  WHERE  TABLE_SCHEMA = DATABASE()
    AND  REFERENCED_TABLE_NAME IS NOT NULL
  ORDER  BY TABLE_NAME, CONSTRAINT_NAME;
")
print(fkeys)

# --- Fermeture de la connexion ---
dbDisconnect(con)
cat("\nSchéma MySQL créé avec succès.\n")
