-- Author: Victor Diallen --

-- Skills used: Where, Order, Group By, Case, Creating View, Creating Temporary Tables and Creating Stored Procedures

-- Select all data

Select *
From coviddata


-- Order data by number of deaths

Select *
From coviddata
Order by Deaths desc



-- Group by countries' confirmed and death cases

Select Country, Confirmed, Deaths
From coviddata
Group By Country



-- Order data by number of deaths in Europe

Select *
From coviddata
Where Region like '%europe%'
Order by Deaths desc



-- Countries with highest recovery rate in North, South and Central America

Select Country, Confirmed, Deaths, (Recovered/Confirmed)*100 as RecoveryRate 
From coviddata
Where region like '%americas%'
Order by RecoveryRate desc



-- Countries with highest mortality rate labeled

Select Country, Confirmed, Deaths, (Deaths/Confirmed)*100 as MortalityRate,
CASE
	WHEN ((Deaths/Confirmed)*100) > 10 THEN 'Mortality rate too high'
    WHEN ((Deaths/Confirmed)*100) BETWEEN 4 and 10 THEN 'Average mortality rate'
    ELSE 'Mortality rate below average'
END as MortalityLabeled
From coviddata
Order by MortalityRate desc




-- Create view for partial data

CREATE VIEW WrappedContent AS
    SELECT 
        Country,
        Confirmed,
        Deaths,
        Recovered
    FROM
        coviddata;
 
 
 
-- Creating Temp Table to perform calculations by region

DROP TEMPORARY TABLE IF EXISTS CovidCalc2;
CREATE TEMPORARY TABLE CovidCalc2(
	Region varchar(255),
    AllCases int,
    AllRecovered int,
    AllDeaths int
);

Insert into CovidCalc2
Select coviddata.Region, sum(coviddata.Confirmed), sum(coviddata.Recovered), sum(coviddata.Deaths)
From coviddata 
Group by region;

Select * 
From CovidCalc2


-- Creating Stored Procedures to generate a temporary table like above

DELIMITER //
DROP PROCEDURE IF EXISTS ProcTab;
CREATE PROCEDURE ProcTab()
BEGIN
	DROP TEMPORARY TABLE IF EXISTS CovidCalc2;
CREATE TEMPORARY TABLE CovidCalc2(
	Region varchar(255),
    AllCases int,
    AllRecovered int,
    AllDeaths int
);

Insert into CovidCalc2
Select coviddata.Region, sum(coviddata.Confirmed), sum(coviddata.Recovered), sum(coviddata.Deaths)
From coviddata 
Group by region;

Select * 
From CovidCalc2;
END //

DELIMITER ;


-- Calling created procedure

call ProcTab()