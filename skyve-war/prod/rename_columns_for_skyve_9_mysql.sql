use skyve;

ALTER TABLE ADM_UserMonthlyHits CHANGE `year` hitYear int NULL;
ALTER TABLE ADM_UserMonthlyHits CHANGE `month` hitMonth int NULL;
