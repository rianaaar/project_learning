-- Overall performance DQLab Store dari tahun 2009 - 2012 untuk jumlah order dan total sales order finished
select year(order_date) as years, sum(sales) as sales, 
count(distinct order_id) as number_of_order from dqlab_sales_store 
where order_status = "Order Finished" group by 1;

-- Overall performance DQLab by subcategory product yang akan dibandingkan antara tahun 2011 dan tahun 2012
select year(order_date) as years, product_sub_category, sum(sales) as sales 
from dqlab_sales_store where order_status='Order Finished' 
and year(order_date) in ('2011','2012') group by 1,2 order by 1,3 desc

-- Efektifitas dan efisiensi promosi yang dilakukan selama ini, dengan menghitung burn rate dari promosi yang dilakukan overall berdasarkan tahun
select year(order_date) as years, sum(sales) as sales, sum(discount_value) 
as promotion_value, round((sum(discount_value)/sum(sales)) * 100,2) as 
burn_rate_percentage from dqlab_sales_store 
where order_status = 'Order Finished' group by 1

-- Efektifitas dan efisiensi promosi yang dilakukan selama ini, dengan menghitung burn rate dari promosi yang dilakukan overall berdasarkan sub-category
select year(order_date) as years, product_sub_category, product_category, 
sum(sales) as sales, sum(discount_value) as promotion_value, 
round((sum(discount_value)/sum(sales))*100,2) as burn_rate_percentage 
from dqlab_sales_store where order_status = 'Order Finished' 
and year(order_date)='2012' group by 1,2,3 order by 1,4 desc

-- Analisa terhadap customer setiap tahunnya
select year(order_date) as years, count(distinct customer) as 
number_of_customer from dqlab_sales_store where order_status = 'Order Finished' 
group by 1
