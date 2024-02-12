use skyve;

EXEC sp_rename 'ADM_UserMonthlyHits.year', 'hitYear', 'COLUMN';
GO
EXEC sp_rename 'ADM_UserMonthlyHits.month', 'hitMonth', 'COLUMN';
GO
