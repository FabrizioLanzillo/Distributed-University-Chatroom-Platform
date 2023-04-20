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
VALUES  (UUID_TO_BIN(UUID()), 'm.rossi', 'pass', 'm.rossi@unipi.it', 'Mario', 'Rossi'),
        (UUID_TO_BIN(UUID()), 'l.marrone', 'pass', 'l.marrone@unipi.it', 'Luca', 'Marrone'),
        (UUID_TO_BIN(UUID()), 'g.fantini', 'pass', 'g.fantini@unipi.it', 'Giorgio', 'Fantini'),
        (UUID_TO_BIN(UUID()), 't.baldini', 'pass', 't.baldini@unipi.it', 'Tommaso', 'Baldini'),
        (UUID_TO_BIN(UUID()), 'r.paoli', 'pass', 'r.paoli@unipi.it', 'Ruggero', 'Paoli'),
        (UUID_TO_BIN(UUID()), 'v.cantini', 'pass', 'v.cantini@unipi.it', 'Valerio', 'Cantini'),
        (UUID_TO_BIN(UUID()), 'a.torri', 'pass', 'a.torri@unipi.it', 'Antonio', 'Torri'),
        (UUID_TO_BIN(UUID()), 'e.romani', 'pass', 'e.romani@unipi.it', 'Ettore', 'Romani'),
        (UUID_TO_BIN(UUID()), 'm.banti', 'pass', 'm.banti@unipi.it', 'Maurizio', 'Banti'),
        (UUID_TO_BIN(UUID()), 'g.lini', 'pass', 'g.lini@unipi.it', 'Giulia', 'Lini');

INSERT INTO student (id, username, password, email, name, surname, degree, language)
VALUES  (UUID_TO_BIN(UUID()), 'f.lanzillo', 'pass', 'f.lanzillo@unipi.it', 'Fabrizio', 'Lanzillo','CE', 'ITA'),
        (UUID_TO_BIN(UUID()), 'f.montini', 'pass', 'f.montini@unipi.it', 'Federico', 'Montini','CE', 'ITA'),
        (UUID_TO_BIN(UUID()), 'r.sagramoni', 'pass', 'r.sagramoni@unipi.it', 'Riccardo', 'Sagramoni','CE', 'ITA'),
        (UUID_TO_BIN(UUID()), 's.trussardi', 'pass', 's.trussardi@unipi.it', 'Sandro', 'Trussardi','AIDE', 'ITA'),
        (UUID_TO_BIN(UUID()), 'g.giannetti', 'pass', 'g.giannetti@unipi.it', 'Giulio', 'Giannetti','AIDE', 'ITA'),
        (UUID_TO_BIN(UUID()), 'o.galiazzo', 'pass', 'o.galiazzo@unipi.it', 'Olga', 'Galiazzo','CE', 'ITA'),
        (UUID_TO_BIN(UUID()), 'e.mazzeo', 'pass', 'e.mazzeo@unipi.it', 'Eraldo', 'Mazzeo','CE', 'ITA'),
        (UUID_TO_BIN(UUID()), 'b.antonetti', 'pass', 'b.antonetti@unipi.it', 'Benito', 'Antonetti','AIDE', 'ITA'),
        (UUID_TO_BIN(UUID()), 'v.abatantuono', 'pass', 'v.abatantuono@unipi.it', 'Veronica', 'Abatantuono','AIDE', 'ITA'),
        (UUID_TO_BIN(UUID()), 'r.finetti', 'pass', 'r.finetti@unipi.it', 'Roberto', 'Finetti','AIDE', 'ITA'),
        (UUID_TO_BIN(UUID()), 'g.trentini', 'pass', 'g.trentini@unipi.it', 'Giulietta', 'Trentini','CE', 'ITA'),
        (UUID_TO_BIN(UUID()), 'b.gori', 'pass', 'b.gori@unipi.it', 'Bianca', 'Gori','AIDE', 'ITA'),
        (UUID_TO_BIN(UUID()), 'n.abbagnale', 'pass', 'n.abbagnale@unipi.it', 'Nicoletta', 'Abbagnale','AIDE', 'ITA'),
        (UUID_TO_BIN(UUID()), 'p.lancisi', 'pass', 'p.lancisi@unipi.it', 'Pina', 'Lancisi','CE', 'ITA'),
        (UUID_TO_BIN(UUID()), 'l.tomaselli', 'pass', 'l.tomaselli@unipi.it', 'Liana', 'Tomaselli','CE', 'ITA');

INSERT INTO course (name, professor, description)
VALUES ('DSMT', (SELECT id FROM professor WHERE surname = 'Rossi'), 'The course is aimed at providing students with proper conceptual and technological tools for the design, analysis, and development of modern distributed applications. After introducing models, paradigms and algorithms for distributed software, various types of middleware systems are presented, focusing on the issues they have been designed to deal with. Students will learn to design, implement, and integrate distributed software, possibly made of heterogeneous components; moreover, they will acquire the ability to choose and apply the most suitable middleware solutions to address practical problems in distributed enterprise applications.'),
       ('MaSSS', (SELECT id FROM professor WHERE surname = 'Marrone'), 'The course is aimed at providing students a knowledge about architectural and operating system issues, middleware abstractions and mechanisms (distributed objects middleware), wireless sensor network programming (event-driven, in-network, power-aware). In laboratory sessions students will exercise with programming smartphones-based mobile applications (Android), their integration with back-end servers, and wireless and wearable sensor applications.'),
       ('SNH', (SELECT id FROM professor WHERE surname = 'Fantini'), 'The best way to understand what attackers can do is to reason like one of them. In this course we will explore the tecniques that are common knowledge among attackers. The purpose is to understand the strenghts and, most importantantly, the limits of all the countermeasures that modern systems implement to mitigate these attacks. In turn, this requires a study of some topics that are sometimes skipped in architectural courses, like heap implementation, dynamic libraries and Virtual Machines.'),
       ('FOC', (SELECT id FROM professor WHERE surname = 'Paoli'), 'student will acquire knowledge about applied cryptography, secure coding and web security. The objective of the course is to make a student able to properly design and implement a secure distributed application. More precisely, the student will get a detailed knowledge of the main cryptographic primitives (ciphers, hash functions, digital signatures), their properties in terms of security and performance, and their appropriate usage in designing and building protocols and systems. The student will also get basic notions about secure coding, and web security and the related main attacks including buffer overflow and SQL injection.'),
       ('ECS', (SELECT id FROM professor WHERE surname = 'Baldini'), 'description'),
       ('LSMSD', (SELECT id FROM professor WHERE surname = 'Cantini'), 'description'),
       ('PESN', (SELECT id FROM professor WHERE surname = 'Torri'), 'description'),
       ('IS', (SELECT id FROM professor WHERE surname = 'Romani'), 'description'),
       ('CCA', (SELECT id FROM professor WHERE surname = 'Banti'), 'description'),
       ('ANAWS', (SELECT id FROM professor WHERE surname = 'Lini'), 'description'),
       ('CC', (SELECT id FROM professor WHERE surname = 'Rossi'), 'description'),
       ('FOE', (SELECT id FROM professor WHERE surname = 'Rossi'), 'description'),
       ('TREA', (SELECT id FROM professor WHERE surname = 'Rossi'), 'description'),
       ('NAO', (SELECT id FROM professor WHERE surname = 'Rossi'), 'description'),
       ('FMFSS', (SELECT id FROM professor WHERE surname = 'Rossi'), 'description');

INSERT INTO admin (id, username, password)
VALUES (UUID_TO_BIN(UUID()), 'admin', 'admin');


INSERT INTO student_starred_courses (student, course)
VALUES ((SELECT id FROM student WHERE surname = 'Lanzillo'), (SELECT id FROM course WHERE name = 'DSMT')),
       ((SELECT id FROM student WHERE surname = 'Lanzillo'), (SELECT id FROM course WHERE name = 'FOC')),
       ((SELECT id FROM student WHERE surname = 'Montini'), (SELECT id FROM course WHERE name = 'DSMT')),
       ((SELECT id FROM student WHERE surname = 'Montini'), (SELECT id FROM course WHERE name = 'FMFSS')),
       ((SELECT id FROM student WHERE surname = 'Sagramoni'), (SELECT id FROM course WHERE name = 'DSMT'));

INSERT INTO meeting_slot
VALUES (UUID_TO_BIN(UUID()), (SELECT id FROM course WHERE name = 'DSMT'), 3, '15:30:00'),
       (UUID_TO_BIN(UUID()), (SELECT id FROM course WHERE name = 'FMFSS'), 1, '13:00:00'),
       (UUID_TO_BIN(UUID()), (SELECT id FROM course WHERE name = 'MaSSS'), 2, '17:00:00'),
       (UUID_TO_BIN(UUID()), (SELECT id FROM course WHERE name = 'ECS'), 4, '10:00:00'),
       (UUID_TO_BIN(UUID()), (SELECT id FROM course WHERE name = 'LSMSD'), 5, '09:00:00'),
       (UUID_TO_BIN(UUID()), (SELECT id FROM course WHERE name = 'ANAWS'), 4, '09:30:00'),
       (UUID_TO_BIN(UUID()), (SELECT id FROM course WHERE name = 'FOC'), 2, '14:30:00');

INSERT INTO booked_meeting
VALUES (UUID_TO_BIN(UUID()), (SELECT id FROM meeting_slot WHERE course = (SELECT id FROM course WHERE name = 'DSMT')), '2023-05-05', (SELECT id FROM student WHERE surname = 'Lanzillo')),
       (UUID_TO_BIN(UUID()), (SELECT id FROM meeting_slot WHERE course = (SELECT id FROM course WHERE name = 'FOC')), '2023-05-08', (SELECT id FROM student WHERE surname = 'Lanzillo')),
       (UUID_TO_BIN(UUID()), (SELECT id FROM meeting_slot WHERE course = (SELECT id FROM course WHERE name = 'ECS')), '2023-05-10', (SELECT id FROM student WHERE surname = 'Lanzillo')),
       (UUID_TO_BIN(UUID()), (SELECT id FROM meeting_slot WHERE course = (SELECT id FROM course WHERE name = 'ANAWS')), '2023-05-27', (SELECT id FROM student WHERE surname = 'Lanzillo')),
       (UUID_TO_BIN(UUID()), (SELECT id FROM meeting_slot WHERE course = (SELECT id FROM course WHERE name = 'ECS')), '2023-05-11', (SELECT id FROM student WHERE surname = 'Montini')),
       (UUID_TO_BIN(UUID()), (SELECT id FROM meeting_slot WHERE course = (SELECT id FROM course WHERE name = 'ANAWS')), '2023-05-22', (SELECT id FROM student WHERE surname = 'Montini')),
       (UUID_TO_BIN(UUID()), (SELECT id FROM meeting_slot WHERE course = (SELECT id FROM course WHERE name = 'ECS')), '2023-05-09', (SELECT id FROM student WHERE surname = 'Sagramoni')),
       (UUID_TO_BIN(UUID()), (SELECT id FROM meeting_slot WHERE course = (SELECT id FROM course WHERE name = 'ANAWS')), '2023-05-25', (SELECT id FROM student WHERE surname = 'Sagramoni'));




/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2023-03-21 14:39:37
