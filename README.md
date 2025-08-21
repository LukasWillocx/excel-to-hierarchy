# Excel to hierarchy

## Design

This application takes an excel file and returns an interactive hierahical html file. The user can define 1-6 of the first columns in the excel file as levels. These levels will be returned as tiles that can be clicked through. Any subsequent columns in the excel will be returned 
as an information flash card at the end of the defined hierarchy. 

## Example

The example below has four columns, of which the first two are defined in the app as levels. Upon creating the html the user will be prompted with eiter 'fruit' or 'vegetable' after which it will be prompted for a specific item. Upon reaching this second and final level, the remaining columns
will be displayed as information.

| Category   | Item         | Color     | Common Uses            |
|------------|-------------|-----------|------------------------|
| Fruit      | Apple       | Red/Green | Eating fresh, baking   |
| Fruit      | Banana      | Yellow    | Smoothies, snacks      |
| Fruit      | Orange      | Orange    | Juicing, snacking      |
| Fruit      | Grape       | Purple    | Wine, snacks           |
| Vegetable  | Carrot      | Orange    | Salads, soups          |
| Vegetable  | Broccoli    | Green     | Steaming, stir-fry     |
| Vegetable  | Potato      | Brown     | Mashed, fried, baked   |
| Vegetable  | Tomato      | Red       | Sauces, salads         |

## Use-case

Extremely convenient for rapidly developing a procedural decision tree with many conditional branches, intuitive to browse through and easily adaptable since it's just an excel. It was originally designed for a medical procedural context. The R shiny framework is excellent for hosting webapplications,
it allows to present the user with a minimal UI that's targetted to first-time/ single-time users.
