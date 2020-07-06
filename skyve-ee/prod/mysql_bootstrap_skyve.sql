-- MySQL Administrator dump 1.4
--
-- ------------------------------------------------------
-- Server version	5.0.20a-nt


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;

/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;


--
-- Create schema skyve - demo, mike, mike
-- MD5
--

CREATE DATABASE /*!32312 IF NOT EXISTS*/ skyve CHARACTER SET utf8 COLLATE utf8_general_ci;
USE skyve;

--
-- Table structure for table `skyve`.`adm_contact`
--

DROP TABLE IF EXISTS `adm_contact`;
CREATE TABLE `adm_contact` (
  `bizId` varchar(36) NOT NULL,
  `bizVersion` int(11) NOT NULL,
  `bizLock` varchar(50) NOT NULL,
  `bizCustomer` varchar(50) NOT NULL,
  `bizFlagComment` varchar(1024) default NULL,
  `bizDataGroupId` varchar(36) default NULL,
  `bizUserId` varchar(36) NOT NULL,
  `bizKey` varchar(1024) NOT NULL,
  `name` varchar(100) default NULL,
  `contactType` varchar(50) default NULL,
  `mobile` varchar(20) default NULL,
  `email1` varchar(50) default NULL,
  PRIMARY KEY  (`bizId`),
  KEY `bizUserIdIndex` (`bizUserId`),
  KEY `bizCustomerIndex` (`bizCustomer`)
) ENGINE=InnoDB;

--
-- Dumping data for table `skyve`.`adm_contact`
--

/*!40000 ALTER TABLE `adm_contact` DISABLE KEYS */;
INSERT INTO `adm_contact` (`bizId`,`bizVersion`,`bizLock`,`bizCustomer`,bizUserId,bizKey,`name`,`mobile`,`email1`,`contactType`) VALUES 
 ('3b596402-553b-4046-8660-c4c0b47e58ec',72,'20080114123937714mike','demo','781e8526-0795-49a9-926b-de40b8c4fb9e','Mike Sands','Mike Sands',NULL,'mike.sands@bizhub.com.au','Person');
/*!40000 ALTER TABLE `adm_contact` ENABLE KEYS */;

--
-- Table structure for table `skyve`.`adm_datagroup`
--

DROP TABLE IF EXISTS `adm_datagroup`;
CREATE TABLE `adm_datagroup` (
  `bizId` varchar(36) NOT NULL,
  `bizVersion` int(11) NOT NULL,
  `bizLock` varchar(50) NOT NULL,
  `bizCustomer` varchar(50) NOT NULL,
  `bizFlagComment` varchar(1024) default NULL,
  `bizDataGroupId` varchar(36) default NULL,
  `bizUserId` varchar(36) NOT NULL,
  `bizKey` varchar(1024) NOT NULL,
  `name` varchar(30) default NULL,
  `description` varchar(255) default NULL,
  PRIMARY KEY  (`bizId`),
  KEY `bizUserIdIndex` (`bizUserId`),
  KEY `bizCustomerIndex` (`bizCustomer`)
) ENGINE=InnoDB;

--
-- Dumping data for table `skyve`.`adm_datagroup`
--

/*!40000 ALTER TABLE `adm_datagroup` DISABLE KEYS */;
/*!40000 ALTER TABLE `adm_datagroup` ENABLE KEYS */;

--
-- Table structure for table `skyve`.`adm_securitygroup`
--

DROP TABLE IF EXISTS `adm_securitygroup`;
CREATE TABLE `adm_securitygroup` (
  `bizId` varchar(36) NOT NULL,
  `bizVersion` int(11) NOT NULL,
  `bizLock` varchar(50) NOT NULL,
  `bizCustomer` varchar(50) NOT NULL,
  `bizFlagComment` varchar(1024) default NULL,
  `bizDataGroupId` varchar(36) default NULL,
  `bizUserId` varchar(36) NOT NULL,
  `bizKey` varchar(1024) NOT NULL,
  `name` varchar(30) default NULL,
  `description` varchar(255) default NULL,
  PRIMARY KEY  (`bizId`),
  KEY `bizUserIdIndex` (`bizUserId`),
  KEY `bizCustomerIndex` (`bizCustomer`)
) ENGINE=InnoDB;

--
-- Dumping data for table `skyve`.`adm_securitygroup`
--

/*!40000 ALTER TABLE `adm_securitygroup` DISABLE KEYS */;
INSERT INTO `adm_securitygroup` (`bizId`,`bizVersion`,`bizLock`,`bizCustomer`,bizUserId,bizKey,`name`,`description`) VALUES 
 ('397f731c-6b7c-40f9-bf35-142d4d30d55b',200,'20080114123937714mike','demo','781e8526-0795-49a9-926b-de40b8c4fb9e','Everything','Everything','The kitchen sink');
/*!40000 ALTER TABLE `adm_securitygroup` ENABLE KEYS */;


--
-- Table structure for table `skyve`.`adm_securitygrouprole`
--

DROP TABLE IF EXISTS `adm_securitygrouprole`;
CREATE TABLE `adm_securitygrouprole` (
  `bizId` varchar(36) NOT NULL,
  `bizVersion` int(11) NOT NULL,
  `bizLock` varchar(50) NOT NULL,
  `bizCustomer` varchar(50) NOT NULL,
  `bizFlagComment` varchar(1024) default NULL,
  `bizDataGroupId` varchar(36) default NULL,
  `bizUserId` varchar(36) NOT NULL,
  `bizKey` varchar(1024) NOT NULL,
  `parent_id` varchar(36) default NULL,
  `roleName` varchar(60) default NULL,
  PRIMARY KEY  (`bizId`),
  KEY `FK13A63BA0A3176D40` (`parent_id`),
  KEY `bizUserIdIndex` (`bizUserId`),
  KEY `bizCustomerIndex` (`bizCustomer`),
  CONSTRAINT `FK13A63BA0A3176D40` FOREIGN KEY (`parent_id`) REFERENCES `adm_securitygroup` (`bizId`)
) ENGINE=InnoDB;

--
-- Dumping data for table `skyve`.`adm_securitygrouprole`
--

/*!40000 ALTER TABLE `adm_securitygrouprole` DISABLE KEYS */;
INSERT INTO `adm_securitygrouprole` (`bizId`,`bizVersion`,`bizLock`,`bizCustomer`,bizUserId,bizKey,`roleName`,`parent_id`) VALUES 
  ('4d3e4e15-fd50-48ca-ac00-72d1d6e44430',69,'20080114123937714mike','demo','781e8526-0795-49a9-926b-de40b8c4fb9e','admin.BasicUser','admin.BasicUser','397f731c-6b7c-40f9-bf35-142d4d30d55b'),
 ('6686f21b-f9e6-4d94-9cf9-ca22dd4a731f',69,'20080114123937714mike','demo','781e8526-0795-49a9-926b-de40b8c4fb9e','admin.ContactViewer','admin.ContactViewer','397f731c-6b7c-40f9-bf35-142d4d30d55b'),
 ('a9af46cd-292c-4810-ab23-db3344e81d41',69,'20080114123937714mike','demo','781e8526-0795-49a9-926b-de40b8c4fb9e','admin.StyleMaintainer','admin.StyleMaintainer','397f731c-6b7c-40f9-bf35-142d4d30d55b'),
 ('be6c3124-1774-4925-90fc-0a1cdba1c52c',69,'20080114123937714mike','demo','781e8526-0795-49a9-926b-de40b8c4fb9e','admin.SecurityAdministrator','admin.SecurityAdministrator','397f731c-6b7c-40f9-bf35-142d4d30d55b');
/*!40000 ALTER TABLE `adm_securitygrouprole` ENABLE KEYS */;


--
-- Table structure for table `skyve`.`adm_securityuser`
--

DROP TABLE IF EXISTS `adm_securityuser`;
CREATE TABLE `adm_securityuser` (
  `bizId` varchar(36) NOT NULL,
  `bizVersion` int(11) NOT NULL,
  `bizLock` varchar(50) NOT NULL,
  `bizCustomer` varchar(50) NOT NULL,
  `bizFlagComment` varchar(1024) default NULL,
  `bizDataGroupId` varchar(36) default NULL,
  `bizUserId` varchar(36) NOT NULL,
  `bizKey` varchar(1024) NOT NULL,
  `userName` varchar(30) default NULL,
  `password` varchar(30) default NULL,
  `homeModule` varchar(30) default NULL,
  `inactive` bit(1) default NULL,
  `generated` bit(1) default NULL,
  `legacyId` varchar(50) default NULL,
  `contact_id` varchar(36) default NULL,
  `dataGroup_id` varchar(36) default NULL,
  PRIMARY KEY  (`bizId`),
  KEY `FKD09F28E02E038B6B` (`contact_id`),
  KEY `FKD09F28E0DA1DD2EB` (`dataGroup_id`),
  KEY `bizUserIdIndex` (`bizUserId`),
  KEY `bizCustomerIndex` (`bizCustomer`),
  CONSTRAINT `FKD09F28E02E038B6B` FOREIGN KEY (`contact_id`) REFERENCES `adm_contact` (`bizId`),
  CONSTRAINT `FKD09F28E0DA1DD2EB` FOREIGN KEY (`dataGroup_id`) REFERENCES `adm_datagroup` (`bizId`)
) ENGINE=InnoDB;

--
-- Dumping data for table `skyve`.`adm_securityuser`
-- SHA1 password01 '0RGzjA5zvIZ8S61AI2BqDg32TC8='
-- MD5 password01 'r4igrmQVibkI+osx8Pz24Q=='

--

/*!40000 ALTER TABLE `adm_securityuser` DISABLE KEYS */;
INSERT INTO `adm_securityuser` (`bizId`,`bizVersion`,`bizLock`,`bizCustomer`,bizUserId,bizKey,`userName`,`password`,`contact_id`) VALUES 
 ('781e8526-0795-49a9-926b-de40b8c4fb9e',57,'20080114123937698mike','demo','781e8526-0795-49a9-926b-de40b8c4fb9e','mike','mike','GBJue9P4Sz8+TfCU3vW33g==','3b596402-553b-4046-8660-c4c0b47e58ec');
/*!40000 ALTER TABLE `adm_securityuser` ENABLE KEYS */;


--
-- Table structure for table `skyve`.`adm_securityuser_groups`
--

DROP TABLE IF EXISTS `adm_securityuser_groups`;
CREATE TABLE `adm_securityuser_groups` (
  `owner_id` varchar(36) NOT NULL,
  `element_id` varchar(36) NOT NULL,
  KEY `FK7A38F2134A04DDAE` (`element_id`),
  KEY `FK7A38F213646FC1E1` (`owner_id`),
  CONSTRAINT `FK7A38F213646FC1E1` FOREIGN KEY (`owner_id`) REFERENCES `adm_securityuser` (`bizId`),
  CONSTRAINT `FK7A38F2134A04DDAE` FOREIGN KEY (`element_id`) REFERENCES `adm_securitygroup` (`bizId`)
) ENGINE=InnoDB;

--
-- Dumping data for table `skyve`.`adm_securityuser_groups`
--

/*!40000 ALTER TABLE `adm_securityuser_groups` DISABLE KEYS */;
INSERT INTO `adm_securityuser_groups` (`owner_id`,`element_id`) VALUES 
 ('781e8526-0795-49a9-926b-de40b8c4fb9e','397f731c-6b7c-40f9-bf35-142d4d30d55b');
/*!40000 ALTER TABLE `adm_securityuser_groups` ENABLE KEYS */;


--
-- Table structure for table `skyve`.`adm_securityuserrole`
--

DROP TABLE IF EXISTS `adm_securityuserrole`;
CREATE TABLE `adm_securityuserrole` (
  `bizId` varchar(36) NOT NULL,
  `bizVersion` int(11) NOT NULL,
  `bizLock` varchar(50) NOT NULL,
  `bizCustomer` varchar(50) NOT NULL,
  `bizFlagComment` varchar(1024) default NULL,
  `bizDataGroupId` varchar(36) default NULL,
  `bizUserId` varchar(36) NOT NULL,
  `bizKey` varchar(1024) NOT NULL,
  `parent_id` varchar(36) default NULL,
  `roleName` varchar(60) default NULL,
  PRIMARY KEY  (`bizId`),
  KEY `FKA946AD767CB4D48A` (`parent_id`),
  KEY `bizUserIdIndex` (`bizUserId`),
  KEY `bizCustomerIndex` (`bizCustomer`),
  CONSTRAINT `FKA946AD767CB4D48A` FOREIGN KEY (`parent_id`) REFERENCES `adm_securityuser` (`bizId`)
) ENGINE=InnoDB;

--
-- Dumping data for table `skyve`.`adm_securityuserrole`
--

/*!40000 ALTER TABLE `adm_securityuserrole` DISABLE KEYS */;
/*!40000 ALTER TABLE `adm_securityuserrole` ENABLE KEYS */;


--
-- Table structure for table `skyve`.`adm_userloginrecord`
--

DROP TABLE IF EXISTS `adm_userloginrecord`;
CREATE TABLE `adm_userloginrecord` (
  `bizId` varchar(36) NOT NULL,
  `bizVersion` int(11) NOT NULL,
  `bizLock` varchar(50) NOT NULL,
  `bizCustomer` varchar(50) NOT NULL,
  `bizFlagComment` varchar(1024) default NULL,
  `bizDataGroupId` varchar(36) default NULL,
  `bizUserId` varchar(36) NOT NULL,
  `bizKey` varchar(1024) NOT NULL,
  `userName` varchar(30) default NULL,
  `loginDateTime` datetime default NULL,
  `failed` bit(1) default NULL,
  PRIMARY KEY  (`bizId`),
  KEY `bizUserIdIndex` (`bizUserId`),
  KEY `bizCustomerIndex` (`bizCustomer`)
) ENGINE=InnoDB;

--
-- Dumping data for table `skyve`.`adm_userloginrecord`
--

/*!40000 ALTER TABLE `adm_userloginrecord` DISABLE KEYS */;
/*!40000 ALTER TABLE `adm_userloginrecord` ENABLE KEYS */;


--
-- Table structure for table `skyve`.`adm_usermonthlyhits`
--

DROP TABLE IF EXISTS `adm_usermonthlyhits`;
CREATE TABLE `adm_usermonthlyhits` (
  `bizId` varchar(36) NOT NULL,
  `bizVersion` int(11) NOT NULL,
  `bizLock` varchar(50) NOT NULL,
  `bizCustomer` varchar(50) NOT NULL,
  `bizFlagComment` varchar(1024) default NULL,
  `bizDataGroupId` varchar(36) default NULL,
  `bizUserId` varchar(36) NOT NULL,
  `bizKey` varchar(1024) NOT NULL,
  `userName` varchar(30) default NULL,
  `year` int(11) default NULL,
  `month` int(11) default NULL,
  `numberOfHits` int(11) default NULL,
  PRIMARY KEY  (`bizId`),
  KEY `bizUserIdIndex` (`bizUserId`),
  KEY `bizCustomerIndex` (`bizCustomer`)
) ENGINE=InnoDB;



DROP TABLE IF EXISTS ADM_Job;
CREATE TABLE `adm_job` (
  `bizId` varchar(36) NOT NULL,
  `bizVersion` int(11) NOT NULL,
  `bizLock` varchar(50) NOT NULL,
  `bizKey` varchar(1024) NOT NULL,
  `bizCustomer` varchar(50) NOT NULL,
  `bizFlagComment` varchar(1024) DEFAULT NULL,
  `bizDataGroupId` varchar(36) DEFAULT NULL,
  `bizUserId` varchar(36) NOT NULL,
  `startTime` datetime DEFAULT NULL,
  `endTime` datetime DEFAULT NULL,
  `displayName` varchar(100) DEFAULT NULL,
  `percentComplete` int(11) DEFAULT NULL,
  `status` varchar(8) DEFAULT NULL,
  `log` mediumtext,
  PRIMARY KEY (`bizId`),
  KEY `bizUserIdIndex` (`bizUserId`),
  KEY `bizKeyIndex` (`bizKey`(767)),
  KEY `bizCustomerIndex` (`bizCustomer`)
) ENGINE=InnoDB;



--
-- Dumping data for table `skyve`.`adm_usermonthlyhits`
--
DROP TABLE IF EXISTS ADM_JobSchedule;
CREATE TABLE ADM_JobSchedule (
  bizId varchar(36) NOT NULL,
  bizVersion int(11) NOT NULL,
  bizLock varchar(50) NOT NULL,
  bizKey varchar(1024) NOT NULL,
  bizCustomer varchar(50) NOT NULL,
  bizFlagComment varchar(1024) DEFAULT NULL,
  bizDataGroupId varchar(36) DEFAULT NULL,
  bizUserId varchar(36) NOT NULL,
  jobName varchar(100) DEFAULT NULL,
  cronExpression varchar(128) DEFAULT NULL,
  startTime datetime DEFAULT NULL,
  endTime datetime DEFAULT NULL,
  runAs_id varchar(36) DEFAULT NULL,
  PRIMARY KEY (bizId),
  KEY FKFA5C091F320EE857 (runAs_id),
  KEY bizUserIdIndex (bizUserId),
  KEY bizKeyIndex (bizKey(767)),
  KEY bizCustomerIndex (bizCustomer),
  CONSTRAINT FKFA5C091F320EE857 FOREIGN KEY (runAs_id) REFERENCES ADM_SecurityUser (bizId)
) ENGINE=InnoDB;


/*!40000 ALTER TABLE `adm_usermonthlyhits` DISABLE KEYS */;
/*!40000 ALTER TABLE `adm_usermonthlyhits` ENABLE KEYS */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
