
SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0;
SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0;
SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='TRADITIONAL,ALLOW_INVALID_DATES';

-- -----------------------------------------------------
-- Schema mydb
-- -----------------------------------------------------
CREATE SCHEMA IF NOT EXISTS `mydb` DEFAULT CHARACTER SET utf8 COLLATE utf8_general_ci ;
USE `mydb` ;

-- -----------------------------------------------------
-- Table `mydb`.`Sport`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `mydb`.`Sport` (
  `Name` VARCHAR(50) NOT NULL,
  `TeamSize` INT NULL,
  `Description` VARCHAR(255) NULL,
  `International` TINYINT(1) NULL,
  PRIMARY KEY (`Name`))
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `mydb`.`League`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `mydb`.`League` (
  `Name` VARCHAR(50) NOT NULL,
  `Code` INT NULL,
  `International` TINYINT(1) NULL,
  `President` VARCHAR(45) NULL,
  `Position` VARCHAR(45) NULL,
  `ContactAddress` VARCHAR(45) NULL,
  PRIMARY KEY (`Name`))
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `mydb`.`Team`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `mydb`.`Team` (
  `Sport` VARCHAR(45) NOT NULL,
  `Code` INT NULL,
  `Name` VARCHAR(45) NULL,
  `HomeCity` VARCHAR(45) NULL,
  `President` VARCHAR(45) NULL,
  `HeadCoach` VARCHAR(45) NULL,
  PRIMARY KEY (`Sport`))
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `mydb`.`Player`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `mydb`.`Player` (
  `PlayerNumber` INT NOT NULL,
  `Name` VARCHAR(45) NULL,
  `Height` INT NULL,
  `Weight` INT NULL,
  `DateOfBirth` DATE NULL,
  `Bio` VARCHAR(255) NULL,
  `Position` VARCHAR(45) NULL,
  `GroundTimeSpent` INT NULL,
  PRIMARY KEY (`PlayerNumber`))
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `mydb`.`Game`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `mydb`.`Game` (
  `Date` DATE NOT NULL,
  `StartTime` TIME NULL,
  `Winner` VARCHAR(45) NULL,
  `Outcome` VARCHAR(45) NULL,
  PRIMARY KEY (`Date`))
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `mydb`.`Venue`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `mydb`.`Venue` (
  `Address` VARCHAR(45) NOT NULL,
  `SitttingCapacity` INT NULL,
  `City` VARCHAR(45) NULL,
  `CityName` VARCHAR(45) NULL,
  `CityElevation` INT NULL,
  `CityAverageTemprature` INT NULL,
  `CityAverageHigh` INT NULL,
  `Country` VARCHAR(45) NULL,
  PRIMARY KEY (`Address`))
ENGINE = InnoDB;


SET SQL_MODE=@OLD_SQL_MODE;
SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS;
SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS;
