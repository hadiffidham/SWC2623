-- phpMyAdmin SQL Dump
-- version 5.2.1
-- https://www.phpmyadmin.net/
--
-- Host: 127.0.0.1:3308
-- Generation Time: Feb 18, 2026 at 06:57 PM
-- Server version: 10.4.32-MariaDB
-- PHP Version: 8.2.12

SET SQL_MODE = "NO_AUTO_VALUE_ON_ZERO";
START TRANSACTION;
SET time_zone = "+00:00";


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8mb4 */;

--
-- Database: `university_db`
--

-- --------------------------------------------------------

--
-- Table structure for table `courses`
--

CREATE TABLE `courses` (
  `course_code` varchar(10) NOT NULL,
  `course_name` varchar(100) DEFAULT NULL,
  `credits` int(11) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci;

--
-- Dumping data for table `courses`
--

INSERT INTO `courses` (`course_code`, `course_name`, `credits`) VALUES
('CS101', 'Intro to Programming', 3),
('CS102', 'Data Structures', 4),
('CS201', 'Algorithms', 4),
('CS202', 'Software Engineering', 3),
('CS301', 'Database Systems', 3),
('CS401', 'Artificial Intelligence', 4),
('MA101', 'Calculus I', 3),
('MA102', 'Calculus II', 3);

-- --------------------------------------------------------

--
-- Table structure for table `enrollments`
--

CREATE TABLE `enrollments` (
  `id` int(11) NOT NULL,
  `student_id` varchar(10) DEFAULT NULL,
  `course_code` varchar(10) DEFAULT NULL,
  `grade` int(11) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci;

--
-- Dumping data for table `enrollments`
--

INSERT INTO `enrollments` (`id`, `student_id`, `course_code`, `grade`) VALUES
(141, 'S1001', 'CS101', 85),
(142, 'S1001', 'CS102', 90),
(143, 'S1001', 'CS201', 78),
(144, 'S1002', 'CS101', 76),
(145, 'S1002', 'CS102', 82),
(146, 'S1002', 'MA101', 81),
(147, 'S1003', 'CS101', 92),
(148, 'S1003', 'CS102', 94),
(149, 'S1003', 'CS201', 96),
(150, 'S1003', 'CS202', 91),
(151, 'S1003', 'CS301', 89),
(152, 'S1003', 'MA101', 85),
(153, 'S1003', 'MA102', 82),
(154, 'S1004', 'CS101', 88),
(155, 'S1004', 'CS102', 84),
(156, 'S1004', 'MA101', 85),
(157, 'S1004', 'MA102', 86),
(158, 'S1004', 'CS201', 90),
(159, 'S1006', 'CS101', 91),
(160, 'S1006', 'CS102', 89),
(161, 'S1006', 'CS201', 93),
(162, 'S1006', 'CS202', 88),
(163, 'S1006', 'CS301', 90),
(164, 'S1006', 'MA101', 87),
(165, 'S1006', 'MA102', 86),
(166, 'S1006', 'CS401', 85);

-- --------------------------------------------------------

--
-- Table structure for table `graduation_requirements`
--

CREATE TABLE `graduation_requirements` (
  `course_code` varchar(10) NOT NULL,
  `program` varchar(50) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci;

--
-- Dumping data for table `graduation_requirements`
--

INSERT INTO `graduation_requirements` (`course_code`, `program`) VALUES
('CS101', 'Computer Science'),
('CS102', 'Computer Science'),
('CS201', 'Computer Science'),
('CS202', 'Computer Science'),
('CS301', 'Computer Science'),
('CS401', 'Computer Science'),
('MA101', 'Computer Science'),
('MA102', 'Computer Science');

-- --------------------------------------------------------

--
-- Table structure for table `prerequisites`
--

CREATE TABLE `prerequisites` (
  `course_code` varchar(10) NOT NULL,
  `prerequisite_code` varchar(10) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci;

--
-- Dumping data for table `prerequisites`
--

INSERT INTO `prerequisites` (`course_code`, `prerequisite_code`) VALUES
('CS102', 'CS101'),
('CS201', 'CS102'),
('CS202', 'CS102'),
('CS301', 'CS201'),
('CS401', 'CS202'),
('CS401', 'CS301'),
('MA102', 'MA101');

-- --------------------------------------------------------

--
-- Table structure for table `students`
--

CREATE TABLE `students` (
  `student_id` varchar(10) NOT NULL,
  `student_name` varchar(100) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci;

--
-- Dumping data for table `students`
--

INSERT INTO `students` (`student_id`, `student_name`) VALUES
('S1001', 'Hadif Idham'),
('S1002', 'Akmal'),
('S1003', 'Hezry'),
('S1004', 'Hakimi'),
('S1006', 'Faliq');

--
-- Indexes for dumped tables
--

--
-- Indexes for table `courses`
--
ALTER TABLE `courses`
  ADD PRIMARY KEY (`course_code`);

--
-- Indexes for table `enrollments`
--
ALTER TABLE `enrollments`
  ADD PRIMARY KEY (`id`),
  ADD KEY `student_id` (`student_id`),
  ADD KEY `course_code` (`course_code`);

--
-- Indexes for table `graduation_requirements`
--
ALTER TABLE `graduation_requirements`
  ADD PRIMARY KEY (`course_code`);

--
-- Indexes for table `prerequisites`
--
ALTER TABLE `prerequisites`
  ADD PRIMARY KEY (`course_code`,`prerequisite_code`),
  ADD KEY `prerequisite_code` (`prerequisite_code`);

--
-- Indexes for table `students`
--
ALTER TABLE `students`
  ADD PRIMARY KEY (`student_id`);

--
-- AUTO_INCREMENT for dumped tables
--

--
-- AUTO_INCREMENT for table `enrollments`
--
ALTER TABLE `enrollments`
  MODIFY `id` int(11) NOT NULL AUTO_INCREMENT, AUTO_INCREMENT=167;

--
-- Constraints for dumped tables
--

--
-- Constraints for table `enrollments`
--
ALTER TABLE `enrollments`
  ADD CONSTRAINT `enrollments_ibfk_1` FOREIGN KEY (`student_id`) REFERENCES `students` (`student_id`),
  ADD CONSTRAINT `enrollments_ibfk_2` FOREIGN KEY (`course_code`) REFERENCES `courses` (`course_code`);

--
-- Constraints for table `graduation_requirements`
--
ALTER TABLE `graduation_requirements`
  ADD CONSTRAINT `graduation_requirements_ibfk_1` FOREIGN KEY (`course_code`) REFERENCES `courses` (`course_code`);

--
-- Constraints for table `prerequisites`
--
ALTER TABLE `prerequisites`
  ADD CONSTRAINT `prerequisites_ibfk_1` FOREIGN KEY (`course_code`) REFERENCES `courses` (`course_code`),
  ADD CONSTRAINT `prerequisites_ibfk_2` FOREIGN KEY (`prerequisite_code`) REFERENCES `courses` (`course_code`);
COMMIT;

/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
