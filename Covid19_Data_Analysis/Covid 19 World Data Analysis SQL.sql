/* Author: Victor Diallen */

/* The data used here was collected throughout 188 days */

/* Skills used: Nested Queries, Where, Order, Group By, Case, Creating View, Creating Temporary Tables and Creating Stored Procedures */


/* Select all data */
SELECT *
FROM coviddatamod;


/* Count total of countries */
SELECT COUNT(DISTINCT Country) AS TotalCountries
FROM coviddatamod;


/* Count total of regions */
SELECT COUNT(DISTINCT Region) AS TotalRegions
FROM coviddatamod;


/* Calculate total deaths */
SELECT SUM(Deaths) AS TotalDeaths
FROM coviddatamod;


/* Calculate total recovered */
SELECT SUM(Recovered) AS TotalRecovered
FROM coviddatamod;


/* Calculate total confirmed */
SELECT SUM(Confirmed) AS TotalConfirmed
FROM coviddatamod;


/* Order data by number of deaths grouped by countries */
SELECT Country, SUM(Deaths) AS TotalDeaths
FROM coviddatamod
GROUP BY Country
ORDER BY TotalDeaths DESC;


/* Order data by number of recovered grouped by countries */
SELECT Country, SUM(Recovered) AS TotalRecovered
FROM coviddatamod
GROUP BY Country
ORDER BY TotalRecovered DESC;


/* Order data by number of confirmed grouped by countries */
SELECT Country, SUM(Confirmed) AS TotalConfirmed
FROM coviddatamod
GROUP BY Country
ORDER BY TotalConfirmed DESC;


/* Order data by number of deaths grouped by region */
SELECT Region, SUM(Deaths) AS TotalDeaths
FROM coviddatamod
GROUP BY Region
ORDER BY TotalDeaths DESC;


/* Order data by number of recovered grouped by region */
SELECT Region, SUM(Recovered) AS TotalRecovered
FROM coviddatamod
GROUP BY Region
ORDER BY TotalRecovered DESC;


/* Order data by number of confirmed grouped by region */
SELECT Region, SUM(Confirmed) AS TotalConfirmed
FROM coviddatamod
GROUP BY Region
ORDER BY TotalConfirmed DESC;


/* Countries with highest recovery rate */
SELECT Country, TotalRecovered, TotalConfirmed, CONCAT(ROUND((TotalRecovered/TotalConfirmed)*100, 2), '%') AS RecoveryRate
FROM (
	SELECT Country, SUM(Recovered) AS TotalRecovered, SUM(Confirmed) AS TotalConfirmed
    FROM coviddatamod
    GROUP BY Country) sub
ORDER BY RecoveryRate DESC;


/* Regions with highest recovery rate */
SELECT Region, TotalRecovered, TotalConfirmed, CONCAT(ROUND((TotalRecovered/TotalConfirmed)*100, 2), '%') AS RecoveryRate
FROM (
	SELECT Region, SUM(Recovered) AS TotalRecovered, SUM(Confirmed) AS TotalConfirmed
    FROM coviddatamod
    GROUP BY Region) AS sub
ORDER BY RecoveryRate DESC;


/* Countries with highest recovery rate in the Americas */
Select Country, Region, TotalRecovered, TotalConfirmed, CONCAT(ROUND((TotalRecovered/TotalConfirmed)*100, 2), '%') AS RecoveryRate
From (
	Select Country, Region, SUM(Recovered) AS TotalRecovered, SUM(Confirmed) AS TotalConfirmed
    From coviddatamod
    WHERE Region LIKE '%Americas%'
    Group by Country) sub
Order by RecoveryRate desc;


/* Countries Mortality Rate Labeled */
SELECT Country, TotalDeaths, TotalConfirmed, (TotalDeaths/TotalConfirmed)*100 as MortalityRate,
CASE
	WHEN ((TotalDeaths/TotalConfirmed)*100) > 10 THEN 'Mortality rate too high'
    WHEN ((TotalDeaths/TotalConfirmed)*100) BETWEEN 4 and 10 THEN 'Average mortality rate'
    ELSE 'Mortality rate below average'
END as MortalityLabeled
FROM (
	SELECT Country, SUM(Deaths) AS TotalDeaths, SUM(Confirmed) AS TotalConfirmed
    FROM coviddatamod
    GROUP BY Country) AS sub;


/* Create view for partial data */
CREATE VIEW PartialViewRegionMortality AS
	SELECT Region, TotalDeaths, TotalConfirmed, (TotalDeaths/TotalConfirmed)*100 as MortalityRate,
	CASE
		WHEN ((TotalDeaths/TotalConfirmed)*100) > 10 THEN 'Mortality rate too high'
		WHEN ((TotalDeaths/TotalConfirmed)*100) BETWEEN 4 and 10 THEN 'Average mortality rate'
		ELSE 'Mortality rate below average'
	END as MortalityLabeled
	FROM (
		SELECT Region, SUM(Deaths) AS TotalDeaths, SUM(Confirmed) AS TotalConfirmed
		FROM coviddatamod
		GROUP BY Region) AS sub;
        

/* Select the created partial view */
SELECT *
FROM partialviewregionmortality;
 
 
 /* Creating Temp Table to perform calculations by region */
DROP TEMPORARY TABLE IF EXISTS CovidCalc;
CREATE TEMPORARY TABLE CovidCalc(
	Region varchar(255),
    AllCases int,
    AllRecovered int,
    AllDeaths int
);

INSERT INTO CovidCalc
SELECT coviddatamod.Region, sum(coviddatamod.Confirmed), sum(coviddatamod.Recovered), sum(coviddatamod.Deaths)
FROM coviddatamod 
GROUP BY Region;


/* Select created temporary table */
SELECT * 
FROM CovidCalc


/* Creating Stored Procedures to generate a temporary table like above */
DELIMITER //
DROP PROCEDURE IF EXISTS ProcTab //
CREATE PROCEDURE ProcTab()
BEGIN
	DROP TEMPORARY TABLE IF EXISTS CovidCalc;
CREATE TEMPORARY TABLE CovidCalc(
	Region varchar(255),
    AllCases int,
    AllRecovered int,
    AllDeaths int
);

INSERT INTO CovidCalc
SELECT coviddatamod.Region, sum(coviddatamod.Confirmed), sum(coviddatamod.Recovered), sum(coviddatamod.Deaths)
FROM coviddatamod 
GROUP BY Region;

SELECT * 
FROM CovidCalc;
END //
DELIMITER ;


/* Calling created procedure */
CALL ProcTab()