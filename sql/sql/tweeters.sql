SELECT
  screen_name, 
  freq,
  (SELECT COUNT(1) 
    FROM tweets.tweets 
    WHERE date_trunc('day', created_at) >= '2019-04-01' AND
          date_trunc('day', created_at) <= '2019-04-20') / 
    CAST(freq AS DECIMAL) AS inv_prop
FROM
  (SELECT
    count(1) AS freq,
    b.screen_name
  FROM 
    tweets.tweets AS a
  LEFT JOIN tweets.users AS b
  ON a.user_id = b.user_id
  GROUP BY b.screen_name
  HAVING count(1) > 1) AS q
ORDER BY freq DESC;