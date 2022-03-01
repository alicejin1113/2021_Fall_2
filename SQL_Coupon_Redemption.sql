USE my_first_db;

/* 1. Simply looking at item_data first */
/* 1.1 To check whether one brand would fall into two brand_types */
SELECT 		brand
FROM		(SELECT 	brand, brand_type
			FROM 		item_data
			GROUP BY 	brand, brand_type) AS a
GROUP BY 	brand
HAVING 		COUNT(brand) > 1
ORDER BY 	brand;
/* No, there were no misrecorded brands. */


/* 1.2 To check whether one item would belong to two brands */
SELECT 		item_id
FROM		(SELECT 	item_id, brand
			FROM 		item_data
			GROUP BY 	item_id, brand) AS b
GROUP BY 	item_id
HAVING 		COUNT(*) > 1
ORDER BY 	item_id;
/* No, there were no misrecorded items. */
       
       
/* 1.3 To check whether one item would fall into two categories */
SELECT 		item_id
FROM		(SELECT 	item_id, category
			FROM 		item_data
			GROUP BY 	item_id, category) AS b
GROUP BY 	item_id
HAVING 		COUNT(*) > 1
ORDER BY 	item_id;
/* No, there were no misrecorded items. */


/* 2. Then take a look at coupon_item_mapping*/
/* 2.1 To check whether one coupon contains several items */
SELECT 		coupon_id, COUNT(*) AS Num_items
FROM 		coupon_item_mapping
GROUP BY 	coupon_id
ORDER BY 	Num_items DESC;
/* Indeed, one coupon contains different numbers of items ranging from 11814 to 1. */

/* So I took a deeper look at, let's say, coupon 32 */
SELECT *
FROM coupon_item_mapping
WHERE coupon_id = 23
ORDER BY item_id;


/* 3. Finally, using INNER JOIN to create the whole picture */
SELECT 			coupon_id, coupon_item_mapping.item_id, brand, brand_type, category
FROM 			coupon_item_mapping
INNER JOIN 		item_data
ON 				coupon_item_mapping.item_id = item_data.item_id
ORDER BY 		coupon_id, coupon_item_mapping.item_id, brand, brand_type, category