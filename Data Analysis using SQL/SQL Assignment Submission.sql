-- Task 1: Understanding the data in hand
-- =======================================

-- A. Describe the data in hand in your own words. (Word Limit is 500)

/*
Ans: Superstore Data contains the information about superstore customers, orders placed by them, the products sold by Superstore, shipping details of Orders, and overall market fact information.

Below are the information about different datafiles of the Superstore data that I have imported into database named "superstoresDB" into seperate tables respectively. 

#1. cust_dimen	: 	This file contains all the information about the customers of the superstore.

Below are the types, and meanings of different fields of cust_dimen":

Field Name			Type	Meaning
----------			-------	---------
Cust_id				text	This is the Unique identifier for each Customer, and this value can not be NULL
Customer_Name		text	Name of the Customer, this can be duplicate also.
Customer_Segment	text	The segment the customer they belong to. Example: CONSUMER, SMALL BUSINESS, HOME OFFICE, CORPORATE
Province			text	Area of the Customer
Region				text	The State or Region the Customer belongs

#2. orders_dimen:	This file contains the details of the orders made by customers.

Below are the types, and meanings of different fields of "orders_dimen":

Field Name			Type	Meaning
----------			-------	---------
Order_ID			int(11)	This is not the Unique identifier because of duplicate values.
Order_Date			Date	The Date when Order was placed on.
Order_Priority		text	The priority for the Order. Example: HIGH, MEDIUM, LOW, NOT SPECIFIED
Ord_id				text	This is the Unique identifier for each Order, and this value can not be NULL.

#3. prod_dimen	:	The file contains the details about all the products sold in the superstore.

Below are the types, and meanings of different fields of "prod_dimen":

Field Name				Type	Meaning
----------				-------	---------
Prod_id					text	This is the Unique identifier for each Product, and this value can not be NULL.
Product_Category		text	The gives information of category of Products. Example: OFFICE SUPPLIES, TECHNOLOGY, FURNITURE
Product_Sub_Category 	text	The Product Category further has different Sub-Category

#4. shipping_dimen:	The file contains shipping details for the placed orders by the customes.

Below are the types, and meanings of different fields of "shipping_dimen":

Field Name		Type	Meaning
----------		-------	---------
Ship_id			text	This is the Unique identifier for each Product, and this value can not be NULL.
Ship_Mode		text	The Orders are shipped by three modes : REGULAR AIR, DELIVERY TRUCK, and EXPRESS AIR
Ship_Date		Date	The Date when shipment was started.
Order_ID		int(11)	The Order reference for each Shipment.

#5. market_fact	:	This file contains the information about all the customers, their orders, products, Sales, and profits, etc.

Below are the types, and meanings of different fields of "market_fact":

Field Name				Type	Meaning
----------				------	---------
Ord_id					text	The Order reference id from shipping_dimen
Prod_id					text	The Product reference id from prod_dimen
Ship_id					text	The Shipment reference id from shipping_dimen
Cust_id					text	The Customer reference id from cust_dimen
Sales					double	The sales amount for an Order
Discount				double	The discount amount for an Order
Order_Quantity      	int(11)	The number of quantity of Product for an Order
Profit              	double	The profit amount received out of an Order for a Product
Shipping_Cost       	double	The shipment cost charged for the order
Product_Base_Margin     double	The base margin value for a Product

-------------------------------------------------------------------------------------------------------------------*/


-- B. Identify and list the Primary Keys and Foreign Keys for this dataset (Hint: If a table don’t have Primary Key or Foreign Key, then specifically mention it in your answer.)

/*
Ans. Below are the list of the Primary Keys, and Foreign Keys for the dataset. 

Note: Table_Name(Columns Names) followed by Primary Keys and Foreign Keys details.

#1. cust_dimen(Customer_Name, Province, Region, Customer_Segment, Cust_id)
	Primary keys	: 	Cust_id
	Foreign keys	: 	Not Available
	
#2. orders_dimen(Order_ID, Order_Date, Order_Priority, Ord_id)
	Primary keys	:	Ord_id
	Foreign keys	:	Not Available

#3. prod_dimen(Product_Category, Product_Sub_Category, Prod_id)
	Primary keys	:	Prod_id
	Foreign keys	:	Not Available

#4. shipping_dimen(Order_ID, Ship_Date, Ship_id, Ship_Mode)
	Primary keys	:	Ship_id
	Foreign keys	:	Not Available
	
#5. market_fact(Ord_id, Prod_id, Ship_id, Cust_id, Sales, Discount, Order_Quantity, Profit, Shipping_Cost, Product_Base_Margin)
	Primary keys	:	Not Available
	Foreign keys	:	Ord_id, Prod_id, Ship_id, Cust_id
	

-------------------------------------------------------------------------------------------------------------------*/ 


-- Task 2: Basic Analysis
-- =======================================

-- Write the SQL queries for the following:

-- A. Find the total and the average sales (display total_sales and avg_sales)

# Query to retrieve the total and the average sales from market_fact
SELECT 
    SUM(Sales) AS total_sales, AVG(Sales) AS avg_sales
FROM
    market_fact;


-- B. Display the number of customers in each region in decreasing order of no_of_customers. The result should contain columns Region, no_of_customers

# Query to retrieve the number of customers in each region in decreasing order of no_of_customers.
SELECT 
    Region, COUNT(*) AS no_of_customers
FROM
    cust_dimen
GROUP BY Region
ORDER BY no_of_customers DESC;


-- C. Find the region having maximum customers (display the region name and max(no_of_customers)

# METHOD1 : Efficient Query to retrieve the region having maximum customers
SELECT 
    Region, COUNT(*) AS no_of_customers
FROM
    cust_dimen
GROUP BY Region
ORDER BY no_of_customers DESC
LIMIT 1;


# METHOD2 : Using Nested Query to retrieve the region having maximum customers
SELECT 
    Region, COUNT(*) AS no_of_customers
FROM
    cust_dimen
GROUP BY Region
HAVING no_of_customers = (SELECT 
        MAX(no_of_customers)
    FROM
        (SELECT 
            COUNT(*) no_of_customers
        FROM
            cust_dimen
        GROUP BY Region) tmp_cust_dimen); 
							
							
-- D. Find the number and id of products sold in decreasing order of products sold (display product id, no_of_products sold)

# Query to retrieve the Product Id, and number of Products sold in decreasing order of products sold
SELECT 
    Prod_id AS 'product id',
    SUM(Order_Quantity) AS 'no_of_products_sold'
FROM
    market_fact
GROUP BY Prod_id
ORDER BY no_of_products_sold DESC;


-- E. Find all the customers from Atlantic region who have ever purchased ‘TABLES’ and the number of tables purchased (display the customer name, no_of_tables purchased)

# Query to retrieve all the customers from Atlantic region who have ever purchased ‘TABLES’ and the number of tables purchased
SELECT 
    cd.Customer_Name,
    SUM(mf.Order_Quantity) AS 'No_of_TABLES_Purchased'
FROM
    market_fact mf
        INNER JOIN
    cust_dimen cd ON cd.Cust_id = mf.Cust_id
        INNER JOIN
    prod_dimen pd ON mf.Prod_id = pd.Prod_id
WHERE
    cd.Region = 'Atlantic'
        AND pd.Product_Sub_Category = 'TABLES'
GROUP BY cd.Cust_id
ORDER BY No_of_TABLES_Purchased DESC;



-- Task 3: Advanced Analysis
-- =======================================

-- Write sql queries for the following:

-- A. Display the product categories in descending order of profits (display the product category wise profits i.e. product_category, profits)?

# Query to retrieve the product categories in descending order of profits
SELECT 
    pd.Product_Category 'product_category',
    SUM(mf.Profit) 'profits'
FROM
    market_fact mf
        INNER JOIN
    prod_dimen pd ON mf.Prod_id = pd.Prod_id
GROUP BY product_category
ORDER BY profits DESC; 


-- B. Display the product category, product sub-category and the profit within each sub-category in three columns. 


# Query to retrieve the product category, product sub-category and the profit within each sub-category
SELECT 
    pd.Product_Category 'product_category',
    pd.Product_Sub_Category 'product_sub_category',
    SUM(mf.Profit) 'profits_of_sub_category'
FROM
    market_fact mf
        INNER JOIN
    prod_dimen pd ON mf.Prod_id = pd.Prod_id
GROUP BY product_category , product_sub_category
ORDER BY profits_of_sub_category DESC; 


-- C. Where is the least profitable product subcategory shipped the most? For the least profitable product sub-category, display the region-wise no_of_shipments and the profit made in each region in decreasing order of profits (i.e. region, no_of_shipments, profit_in_each_region)
-- o Note: You can hardcode the name of the least profitable product sub-category


# Query to retrieve least profitable product sub-category
SELECT 
    pd.Product_Sub_Category,
    SUM(mf.Profit) 'Product_Sub_Category_Profits'
FROM
    market_fact mf
        INNER JOIN
    prod_dimen pd ON mf.Prod_id = pd.Prod_id
GROUP BY pd.product_sub_category
ORDER BY Product_Sub_Category_Profits
LIMIT 1;


# Query to retrieve the Regions where the least profitable product subcategory shipped the most.
SELECT 
    cd.Region AS Region,
    COUNT(mf.Ship_id) AS 'Number_of_Shipments'
FROM
    market_fact mf
        INNER JOIN
    cust_dimen cd ON mf.Cust_id = cd.Cust_id
        INNER JOIN
    prod_dimen pd ON mf.Prod_id = pd.Prod_id
WHERE
    pd.Product_Sub_Category = 'TABLES'
GROUP BY Region
ORDER BY Number_of_Shipments DESC
LIMIT 1; 


# Query to retrieve the region-wise no_of_shipments and the profit made in each region in decreasing order of profits for the least profitable product sub-category
SELECT 
    cd.Region AS Region,
    COUNT(mf.Ship_id) AS No_of_Shipments,
    SUM(mf.Profit) AS Profit_in_Each_Region
FROM
    market_fact mf
        INNER JOIN
    cust_dimen cd ON mf.Cust_id = cd.Cust_id
        INNER JOIN
    prod_dimen pd ON mf.Prod_id = pd.Prod_id
WHERE
    pd.Product_Sub_Category = 'TABLES'
GROUP BY cd.Region
ORDER BY Profit_in_Each_Region DESC; 



# Important Note:
# Submit your answers for all these tasks in a .sql file.
