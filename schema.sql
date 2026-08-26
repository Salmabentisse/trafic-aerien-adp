-- =================================================================
-- Projet Trafic Aérien ADP  –  Schéma MySQL
-- Base   : trafic_aerien_adp
-- Moteur : InnoDB  |  Charset : utf8mb4  |  Collation : unicode_ci
-- =================================================================

CREATE DATABASE IF NOT EXISTS trafic_aerien_adp
  CHARACTER SET utf8mb4
  COLLATE utf8mb4_unicode_ci;

USE trafic_aerien_adp;

-- -----------------------------------------------------------------
-- Suppression (ordre inverse des dépendances)
-- -----------------------------------------------------------------
DROP TABLE IF EXISTS flights;
DROP TABLE IF EXISTS weather;
DROP TABLE IF EXISTS planes;
DROP TABLE IF EXISTS airports;
DROP TABLE IF EXISTS airlines;

-- =================================================================
-- TABLE 1 : airlines
--   Clé primaire : carrier (code IATA 2 lettres)
-- =================================================================
CREATE TABLE airlines (
  carrier  VARCHAR(2)   NOT NULL,
  name     VARCHAR(255) NOT NULL,
  CONSTRAINT pk_airlines PRIMARY KEY (carrier)
) ENGINE=InnoDB;

-- =================================================================
-- TABLE 2 : airports
--   Clé primaire : faa (code FAA 3 lettres)
--   Contrainte   : dst ∈ {A, U, N}  (ou NULL pour données manquantes)
-- =================================================================
CREATE TABLE airports (
  faa    VARCHAR(3)    NOT NULL,
  name   VARCHAR(255),
  lat    DECIMAL(10,6),               -- latitude  (degrés décimaux)
  lon    DECIMAL(10,6),               -- longitude (degrés décimaux)
  alt    INT,                         -- altitude en pieds
  tz     SMALLINT,                    -- décalage UTC en heures
  dst    CHAR(1),                     -- heure d'été : A=active, U=US, N=aucune
  tzone  VARCHAR(50),                 -- nom IANA du fuseau (ex. America/New_York)
  CONSTRAINT pk_airports PRIMARY KEY (faa),
  CONSTRAINT chk_dst CHECK (dst IN ('A','U','N') OR dst IS NULL)
) ENGINE=InnoDB;

-- =================================================================
-- TABLE 3 : planes
--   Clé primaire : tailnum (immatriculation N-number)
-- =================================================================
CREATE TABLE planes (
  tailnum      VARCHAR(10)  NOT NULL,
  year         SMALLINT UNSIGNED,     -- année de construction
  type         VARCHAR(100),
  manufacturer VARCHAR(100),
  model        VARCHAR(100),
  engines      TINYINT UNSIGNED,
  seats        SMALLINT UNSIGNED,
  speed        SMALLINT UNSIGNED,     -- vitesse de croisière en mph
  engine       VARCHAR(50),
  CONSTRAINT pk_planes PRIMARY KEY (tailnum)
) ENGINE=InnoDB;

-- =================================================================
-- TABLE 4 : weather
--   Clé primaire  : id (substitut auto-incrémenté)
--   Clé naturelle : (origin, year, month, day, hour)  → UNIQUE
--   FK            : origin → airports.faa
-- =================================================================
CREATE TABLE weather (
  id         INT UNSIGNED  NOT NULL AUTO_INCREMENT,
  origin     VARCHAR(3)    NOT NULL,
  year       SMALLINT      NOT NULL,
  month      TINYINT       NOT NULL,
  day        TINYINT       NOT NULL,
  hour       TINYINT       NOT NULL,
  temp       DECIMAL(6,2),            -- température en °F
  dewp       DECIMAL(6,2),            -- point de rosée en °F
  humid      DECIMAL(5,2),            -- humidité relative (%)
  wind_dir   SMALLINT UNSIGNED,       -- direction du vent (degrés)
  wind_speed DECIMAL(7,2),            -- vitesse du vent (mph)
  wind_gust  DECIMAL(7,2),            -- rafales (mph)
  precip     DECIMAL(5,2),            -- précipitations (pouces)
  pressure   DECIMAL(7,2),            -- pression (mbar)
  visib      DECIMAL(5,2),            -- visibilité (miles)
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
) ENGINE=InnoDB;

-- =================================================================
-- TABLE 5 : flights
--   Clé primaire : id (substitut auto-incrémenté)
--   FK           : carrier → airlines  |  tailnum → planes
--                  origin  → airports  |  dest    → airports
-- =================================================================
CREATE TABLE flights (
  id             INT UNSIGNED      NOT NULL AUTO_INCREMENT,
  year           SMALLINT          NOT NULL,
  month          TINYINT           NOT NULL,
  day            TINYINT           NOT NULL,
  dep_time       SMALLINT UNSIGNED,          -- heure réelle départ  (HHMM)
  sched_dep_time SMALLINT UNSIGNED NOT NULL, -- heure planifiée départ
  dep_delay      SMALLINT,                   -- retard départ (min, négatif = avance)
  arr_time       SMALLINT UNSIGNED,          -- heure réelle arrivée (HHMM)
  sched_arr_time SMALLINT UNSIGNED NOT NULL, -- heure planifiée arrivée
  arr_delay      SMALLINT,                   -- retard arrivée (min)
  carrier        VARCHAR(2)        NOT NULL,
  flight         SMALLINT UNSIGNED NOT NULL, -- numéro de vol
  tailnum        VARCHAR(10),
  origin         VARCHAR(3)        NOT NULL,
  dest           VARCHAR(3)        NOT NULL,
  air_time       SMALLINT,                   -- durée de vol (min)
  distance       INT UNSIGNED,               -- distance (miles)
  hour           TINYINT,
  minute         TINYINT,
  time_hour      DATETIME          NOT NULL,
  CONSTRAINT pk_flights PRIMARY KEY (id),
  CONSTRAINT fk_flights_carrier
    FOREIGN KEY (carrier) REFERENCES airlines (carrier)
    ON UPDATE CASCADE ON DELETE RESTRICT,
  CONSTRAINT fk_flights_tailnum
    FOREIGN KEY (tailnum) REFERENCES planes (tailnum)
    ON UPDATE CASCADE ON DELETE SET NULL,
  CONSTRAINT fk_flights_origin
    FOREIGN KEY (origin)  REFERENCES airports (faa)
    ON UPDATE CASCADE ON DELETE RESTRICT,
  CONSTRAINT fk_flights_dest
    FOREIGN KEY (dest)    REFERENCES airports (faa)
    ON UPDATE CASCADE ON DELETE RESTRICT,
  CONSTRAINT chk_flights_month   CHECK (month  BETWEEN 1 AND 12),
  CONSTRAINT chk_flights_day     CHECK (day    BETWEEN 1 AND 31),
  CONSTRAINT chk_flights_hour    CHECK (hour   IS NULL OR hour   BETWEEN 0 AND 23),
  CONSTRAINT chk_flights_minute  CHECK (minute IS NULL OR minute BETWEEN 0 AND 59)
) ENGINE=InnoDB;

-- =================================================================
-- Index d'optimisation
-- =================================================================

-- Filtrage et agrégation par date
CREATE INDEX idx_flights_date      ON flights (year, month, day);
-- Analyses par destination
CREATE INDEX idx_flights_dest      ON flights (dest);
-- Tri / filtre sur retards
CREATE INDEX idx_flights_dep_delay ON flights (dep_delay);
CREATE INDEX idx_flights_arr_delay ON flights (arr_delay);
-- Jointure vols-météo sur time_hour
CREATE INDEX idx_flights_time_hour ON flights (time_hour);
-- Jointure météo-vols sur (origin, time_hour)
CREATE INDEX idx_weather_time_hour ON weather  (origin, time_hour);
-- Recherche aéroport par nom
CREATE INDEX idx_airports_name     ON airports (name(50));

-- =================================================================
-- Vue récapitulative : schéma des relations
-- =================================================================
-- airlines  ←── flights.carrier
-- airports  ←── flights.origin
-- airports  ←── flights.dest
-- planes    ←── flights.tailnum
-- airports  ←── weather.origin
