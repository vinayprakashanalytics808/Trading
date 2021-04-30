drop table #counts1
CREATE TABLE #counts1
(
    table_name varchar(255),
    row_count int
)

EXEC sp_MSForEachTable 
@command1='INSERT #counts1 (table_name, row_count) SELECT ''?'', COUNT(*) FROM ?'
SELECT table_name, row_count FROM #counts1
--where row_count = 0 
ORDER BY  row_count DESC


