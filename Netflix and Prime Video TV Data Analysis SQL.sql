-- Author: Victor Diallen --

-- Skills used: Where, Order By, Group By, Case, Creating Temporary Tables and Creating Stored Procedures

-- Select all data from Netflix dataset

Select *
From titles_netflix


-- Select all data from Amazon dataset

Select *
From titles_amazon


-- Select all data from datasets ordered by IMBD scores in descending order

Select *
From titles_netflix			-- change to Amazon dataset if you want its results.
Order by NetflixIMDB desc


-- Select all data from datasets ordered by release year in ascending order

Select *
From titles_netflix			-- change to Amazon dataset if you want its results.
Order by release_year asc


-- Select some data from the best TV shows and movies according to IMDB that are in the comedy genre on Amazon catalog

Select AmazonTitles, description, release_year, AmazonGenres, AmazonProdCountry, AmazonIMDB, tmdb_popularity
From titles_amazon			-- change to Netflix dataset if you want its results.
Where AmazonGenres like '%comedy%'			-- change to any genre you desire
Order by AmazonIMDB desc


-- Select some data from the best TV shows and movies according to IMDB and most popular by TMDB that are in the action genre on Amazon catalog

Select AmazonTitles, description, release_year, AmazonGenres, AmazonProdCountry, AmazonIMDB, tmdb_popularity
From titles_amazon			-- change to Netflix dataset if you want its results.
Where genres like '%action%'			-- change to any genre you desire
Order by imdb_score desc, tmdb_popularity desc


-- Select best titles according to IMDB based on the same production country **note that Amazon's titles repeats themselves since its dataset it's bigger than Netflix's.

Select net.NetflixTitles, ama.AmazonTitles, net.NetflixGenres, ama.AmazonGenres, net.NetflixProdCountry, ama.AmazonProdCountry, net.NetflixIMDB, ama.AmazonIMDB
From titles_netflix net
Join titles_amazon ama
	On net.NetflixProdCountry = ama.AmazonProdCountry
Order by net.NetflixIMDB desc, ama.AmazonIMDB desc


-- Using Case clause to label IMDB's scores

Select NetflixTitles, NetflixIMDB,
CASE
	WHEN NetflixIMDB > 8 THEN 'Very good'
    WHEN NetflixIMDB BETWEEN 7 and 8 THEN 'Good'
    WHEN NetflixIMDB BETWEEN 6 and 7 THEN 'Regular'
    WHEN NetflixIMDB < 6 THEN 'Not good'
END AS 'IMDB_Label'
From titles_netflix
Order by NetflixIMDB desc


-- Creating Stored Procedures to generate a temporary table

DELIMITER //
DROP PROCEDURE IF EXISTS NetflixProc;
CREATE PROCEDURE NetflixProc()
BEGIN
	DROP TEMPORARY TABLE IF EXISTS NetflixTab;
CREATE TEMPORARY TABLE NetflixTab(
	NetflixTitles varchar(255),
    Release_Year int,
    NetflixGenres varchar(255)
);

Insert into NetflixTab
Select titles_netflix.NetflixTitles, titles_netflix.release_year, titles_netflix.NetflixGenres 
From titles_netflix
Order by titles_netflix.release_year desc ;

Select * 
From NetflixTab;
END //

DELIMITER ;


-- Calling created procedure

call NetflixProc()