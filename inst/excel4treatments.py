import openpyxl
import argparse

# Define the columns that we want to merge cells in
columns_to_merge = ["ID", "Disease", "N previous treatment", "Date of the 1st visit",
                    "Date of the 2nd visit", "Date of the follow-up", "Eligible for a 2nd visit"]


# Define the fill pattern to use for highlighting cells containing "Missing"
missing_fill = PatternFill(start_color='FFFF00', end_color='FFFF00', fill_type='solid')

# Define a function to merge the empty cells in a given column
def merge_empty_cells(column):
    # Initialize variables to keep track of the first non-empty cell
    first_non_empty_cell = None
    
    # Iterate over all cells in the column
    for cell in column:
        # If the cell is empty and we haven't found a non-empty cell yet, skip it
        if str(cell.value) == "None" and first_non_empty_cell is None:
            continue
        if cell.value in columns_to_merge:
            continue
        # If the cell is empty but we have found a non-empty cell, merge it with the first non-empty cell
        elif str(cell.value) == "None" and first_non_empty_cell is not None:
            cell_to_merge = "{}{}".format(cell.column_letter, cell.row)
            first_non_empty_cell_to_merge = "{}{}".format(first_non_empty_cell.column_letter, first_non_empty_cell.row)
            sheet.merge_cells("{}:{}".format(first_non_empty_cell_to_merge, cell_to_merge))
        # If the cell is not empty, record it as the first non-empty cell
        else:
            first_non_empty_cell = cell
            
# Parse the filename argument from the command line
parser = argparse.ArgumentParser()
parser.add_argument("filename", help="the name of the Excel file to merge empty cells in")
args = parser.parse_args()

# Load the Excel file using openpyxl
wb = openpyxl.load_workbook(args.filename)

# Iterate over all sheets in the workbook
for sheet in wb:
    # Iterate over all columns in the sheet
    for col in sheet.columns:
        # If the column is one we want to merge cells in, merge the empty cells
        if col[0].value in columns_to_merge:
            merge_empty_cells(col)

    for cell in col:
        # Highlight cells containing the word "Missing" in yellow
        if cell.value == "Missing":
            cell.fill = missing_fill
        # Replace any cell containing "NA" with an empty string
        if cell.value == "NA":
            cell.value = ""

# Save the modified Excel file with the same name as the original file
wb.save(args.filename)


