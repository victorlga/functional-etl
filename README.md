# Functional ETL project instructions

- [Functional ETL project instructions](#functional-etl-project-instructions)
  - [Input](#input)
  - [Output](#output)
  - [Basic requirements](#basic-requirements)
  - [Optional requirements](#optional-requirements)
- [How to run](#how-to-run)
  - [Prerequisites](#prerequisites)
  - [Setting Up the Development Container](#setting-up-the-development-container)
  - [Building and Running the Project](#building-and-running-the-project)

The goal of this ETL project is to process data from a source (a CSV file), apply transformations, and store the result in another resource (a new CSV file).  

The project is based on two tables representing a small part of a management software: one table for orders and another containing the items of each order.  

In a real-world scenario, direct access to the database may be impractical due to security and performance reasons. Therefore, a daily process extracts relevant data and generates a copy in a shared file.  

The final objective is to provide the processed data to the manager, who will use this information to feed a dashboard for aggregated order visualization.

## Input

Daily, two files will be available for processing.  

The first file contains data from the **Order** table, which records the orders selected for the day. This table has a primary key (**id**) that uniquely identifies each order. In addition to the identifier, it includes the client ID (**client_id**), the order date (**order_date**), the order status, and the order origin.  

Orders can have a status of **pending**, **complete**, or **cancelled**. The origin can be **P** for **paraphysical** or **O** for **online**, meaning web-based orders.  

The second file contains data from the **OrderItem** table, which records the items of each order. A single order can have multiple items, and the relationship is established through the **order_id** field. This field, together with **product_id**, forms the primary key (unique identifier) of the table.  

Additionally, the table includes the **quantity**, the price paid at the time of purchase (**price**), and the product tax as a percentage (**tax**). Since the selling price of a product may vary over time, it is essential to store the agreed-upon price at the time of the order.  

The **Clients** and **Products** tables are not provided, as they are not relevant for the aggregated order visualization dashboard.

## Output

The manager would like to receive a CSV file containing three fields: **order_id**, **total_amount**, and **total_taxes**.  

**total_amount** represents the total order value, which is the sum of the revenue from all items in an order. Revenue is calculated by multiplying the **price** by the **quantity**.  

**total_taxes** contains the total tax paid on all items. The tax is calculated as the percentage multiplied by the revenue of each item.  

The manager would like to be able to parameterize the output based on specific **status** and **origin** values for the orders.  

Example: **status**: complete, **origin**: online (**O**).  

CSV output:  
```
order_id,total_amount,total_taxes  
1,1345.88,20.34  
5,34.54,2.35  
14,334.44,30.4  
```

## Basic requirements

- [x] The project must be implemented in **OCaml**.  
- [x] To compute the output, it is required to use **map**, **reduce**, and **filter**.  
- [x] The code must include functions for reading and writing CSV files, which will result in **impure functions**.  
- [x] Separate **impure** functions from **pure** functions in different project files.  **MADE WITH GROK**
- [x] The input data must be loaded into a **list of records**.  
- [x] It is mandatory to use **helper functions** to load the fields into a record.  
- [ ] A project **report** must be written, detailing how each step was implemented. This should serve as a guide for someone looking to recreate the project in the future. The report must also state whether **generative AI** was used or not.

## Optional requirements

- [x] Read the input data from a static file available on the internet (exposed via HTTP).  
- [x] Save the output data in an **SQLite database**.
- [x] Process the input tables separately if needed, but preferably use an **inner join** to merge the tables before transformations.
- [x] Organize the ETL project using **Dune**.
- [x] Document all functions using the **docstring format**.  **MADE WITH GROK**
- [x] Produce an additional output with **average revenue and taxes paid**, grouped by **month and year**.
- [x] Generate **comprehensive test files** for all **pure functions**.

# How to run

## Prerequisites
- Docker
- Visual Studio Code with the Remote - Containers extension
- Internet connection

## Setting Up the Development Container
1. Clone the repository:
   ```bash
   git clone https://github.com/victorlga/functional-etl
   cd functional-etl
   ```

2. Open the project in VS Code and reopen in the container:
   - Press `F1` and select **Dev Containers: Rebuild and Reopen in Container**.
   - VS Code will automatically build the container based on `.devcontainer/devcontainer.json`.

3. Once inside the container, update Opam:
   ```bash
   opam update
   ```

## Building and Running the Project
1. Navigate to the ETL directory:
   ```bash
   cd functional-etl/etl
   ```

2. Build the project:
   ```bash
   dune build
   ```

3. Execute the ETL pipeline:
   ```bash
   dune exec etl
   ```


