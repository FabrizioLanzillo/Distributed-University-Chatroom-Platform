-- MySQL dump 10.13  Distrib 8.0.32, for Linux (x86_64)
--
-- Host: 127.0.0.1    Database: dsmt_student_platform
-- ------------------------------------------------------
-- Server version	8.0.32-0ubuntu0.22.04.2

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!50503 SET NAMES utf8mb4 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

DROP DATABASE IF EXISTS dsmt_student_platform;
CREATE DATABASE dsmt_student_platform;
--
-- Table structure for table `admin`
--

USE dsmt_student_platform;

DROP TABLE IF EXISTS `admin`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `admin` (
  `id` binary(16) NOT NULL,
  `username` varchar(64) NOT NULL,
  `password` varchar(64) NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `username` (`username`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `admin`
--

LOCK TABLES `admin` WRITE;
/*!40000 ALTER TABLE `admin` DISABLE KEYS */;
/*!40000 ALTER TABLE `admin` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `booked_meeting`
--

DROP TABLE IF EXISTS `booked_meeting`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `booked_meeting` (
  `id` binary(16) NOT NULL,
  `time_slot` binary(16) NOT NULL,
  `date` date NOT NULL,
  `student` binary(16) NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `booked_meeting_pk` (`time_slot`,`date`),
  KEY `booked_meeting___fk2` (`student`),
  CONSTRAINT `booked_meeting___fk` FOREIGN KEY (`time_slot`) REFERENCES `meeting_slot` (`id`) ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT `booked_meeting___fk2` FOREIGN KEY (`student`) REFERENCES `student` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `booked_meeting`
--

LOCK TABLES `booked_meeting` WRITE;
/*!40000 ALTER TABLE `booked_meeting` DISABLE KEYS */;
/*!40000 ALTER TABLE `booked_meeting` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `course`
--

DROP TABLE IF EXISTS `course`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `course` (
  `id` int unsigned NOT NULL AUTO_INCREMENT,
  `name` varchar(64) NOT NULL,
  `professor` binary(16) NOT NULL,
  `description` text NOT NULL,
  PRIMARY KEY (`id`),
  KEY `course__professor.id_fk` (`professor`),
  UNIQUE KEY `name` (`name`),
  CONSTRAINT `course__professor.id_fk` FOREIGN KEY (`professor`) REFERENCES `professor` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB AUTO_INCREMENT=2 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `meeting_slot`
--

DROP TABLE IF EXISTS `meeting_slot`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `meeting_slot` (
  `id` binary(16) NOT NULL,
  `course` int unsigned NOT NULL,
  `weekday` int unsigned NOT NULL,
  `starting_time` time NOT NULL,
  PRIMARY KEY (`id`),
  KEY `meeting_slot_course_id_fk` (`course`),
  CONSTRAINT `meeting_slot_course_id_fk` FOREIGN KEY (`course`) REFERENCES `course` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `meeting_slot`
--

LOCK TABLES `meeting_slot` WRITE;
/*!40000 ALTER TABLE `meeting_slot` DISABLE KEYS */;
/*!40000 ALTER TABLE `meeting_slot` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `professor`
--

DROP TABLE IF EXISTS `professor`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `professor` (
  `id` binary(16) NOT NULL,
  `username` varchar(32) NOT NULL,
  `password` varchar(64) NOT NULL,
  `email` varchar(64) NOT NULL,
  `name` varchar(64) NOT NULL,
  `surname` varchar(64) NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `username` (`username`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;
/*!40101 SET character_set_client = @saved_cs_client */;


--
-- Table structure for table `student`
--

DROP TABLE IF EXISTS `student`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `student` (
  `id` binary(16) NOT NULL,
  `username` varchar(32) NOT NULL,
  `password` varchar(64) NOT NULL,
  `email` varchar(64) NOT NULL,
  `name` varchar(64) NOT NULL,
  `surname` varchar(64) NOT NULL,
  `degree` varchar(64) NOT NULL,
  `language` varchar(64) NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `username` (`username`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `course`
--

DROP TABLE IF EXISTS `student_starred_courses`;
CREATE TABLE `student_starred_courses` (
  `student` binary(16) NOT NULL,
  `course` int unsigned NOT NULL,
  PRIMARY KEY (student, course),
  FOREIGN KEY (student) REFERENCES student(id) ON DELETE CASCADE ON UPDATE CASCADE,
  FOREIGN KEY (course) REFERENCES course(id) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB AUTO_INCREMENT=2 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

INSERT INTO professor (id, username, password, email, name, surname)
VALUES (UUID_TO_BIN(UUID()), 'm.rossi', 'pass', 'm.rossi@unipi.it', 'Mario', 'Rossi'),
       (UUID_TO_BIN(UUID()), 'l.marrone', 'pass', 'l.marrone@unipi.it', 'Luca', 'Marrone'),
        (UUID_TO_BIN(UUID()), 'g.bianchi', 'pass', 'g.bianchi@unipi.it', 'Gianni', 'Bianchi');

INSERT INTO student (id, username, password, email, name, surname, degree, language)
VALUES (UUID_TO_BIN(UUID()), 'f.lanzillo', 'pass', 'f.lanzillo@unipi.it', 'Fabrizio', 'Lanzillo','CE', 'ITA'),
       (UUID_TO_BIN(UUID()), 'f.montini', 'pass', 'f.montini@unipi.it', 'Federico', 'Montini','CE', 'ITA'),
        (UUID_TO_BIN(UUID()), 'r.sagramoni', 'pass', 'r.sagramoni@unipi.it', 'Riccardo', 'Sagramoni','CE', 'ITA');

INSERT INTO course (name, professor, description)
VALUES ('dsmt', (SELECT id FROM professor WHERE surname = 'Bianchi'), 'description'),
       ('msss', (SELECT id FROM professor WHERE surname = 'Marrone'), 'description'),
       ('nsh', (SELECT id FROM professor WHERE surname = 'Marrone'), 'description'),
       ('foc', (SELECT id FROM professor WHERE surname = 'Rossi'), 'description'),
       ('fmfss', (SELECT id FROM professor WHERE surname = 'Rossi'), 'description');

INSERT INTO admin (id, username, password)
VALUES (UUID_TO_BIN(UUID()), 'admin', 'admin');


INSERT INTO student_starred_courses (student, course)
VALUES ((SELECT id FROM student WHERE surname = 'Lanzillo'), (SELECT id FROM course WHERE name = 'dsmt')),
       ((SELECT id FROM student WHERE surname = 'Lanzillo'), (SELECT id FROM course WHERE name = 'foc')),
       ((SELECT id FROM student WHERE surname = 'Montini'), (SELECT id FROM course WHERE name = 'dsmt')),
       ((SELECT id FROM student WHERE surname = 'Montini'), (SELECT id FROM course WHERE name = 'fmfss')),
       ((SELECT id FROM student WHERE surname = 'Sagramoni'), (SELECT id FROM course WHERE name = 'dsmt'));

INSERT INTO meeting_slot
VALUES (UUID_TO_BIN(UUID()), (SELECT id FROM course WHERE name = 'dsmt'), 3, '15:20:40'),
       (UUID_TO_BIN(UUID()), (SELECT id FROM course WHERE name = 'foc'), 5, '18:20:40');

INSERT INTO booked_meeting
VALUES (UUID_TO_BIN(UUID()), (SELECT id FROM meeting_slot WHERE course = (SELECT id FROM course WHERE name = 'dsmt')), '2023-04-05', (SELECT id FROM student WHERE surname = 'Lanzillo')),
       (UUID_TO_BIN(UUID()), (SELECT id FROM meeting_slot WHERE course = (SELECT id FROM course WHERE name = 'foc')), '2023-04-08', (SELECT id FROM student WHERE surname = 'Lanzillo'));



/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2023-03-21 14:39:37
