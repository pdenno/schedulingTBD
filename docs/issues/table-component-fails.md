# Table presentation and editing redesign

Our current task is to improve the management of tables.
Tables are presentations of either (1) information we generate based on what human particpants have described, or (2) information they upload as spreadsheets.
Currently, we we have an Material UI (MUI) based React component to present to users tables using MUI's table component. See src/app/stbd_app/components/table.cljs.
The component is buggy and interaction with it is sluggish.

I think it is time to rethink this entire design!
MUI has two data grid components, a free version and a licensed, for fee version, MUI X.
They both seem to be rich in capablities, but today let's try to use the free version.
We are looking to reproduce the capabilities in table.cljs with a new data grid-based component.
Our goal is to for it to provide the sorts of things we need: the ability to edit cells, re-order rows, and add and delete rows (preferable anywhere, but if not, at least at the end of the table).
We will leave the current table.cljs as it is create a new file table2.clj that defines a data grid- based component and modal that we can use like the current TablemModal without much modification.
We'll call the TableModal "Table2Modal". See chat.cljs for how the current TableModal is used.


## Update!

Dialog2 mostly works!
  1) It presents the table just fine.
  2) "Delete Row" works correctly; it deletes the selected rows.
  3) "Add Row" adds rows to the bottom. (Ideally, if there were just one row selected, it would add a row below the selected row, otherwise at the bottom.)
  4) ✅ **FIXED** - Row reordering now works using Up/Down arrow buttons in the Actions column.

## Row Reordering Implementation

The initial attempt to use drag-and-drop row reordering failed because that feature is only available in the paid MUI X Pro/Premium versions, not the free community version we're using. Instead, we implemented a button-based approach that is actually more intuitive and accessible.

The solution adds an "Actions" column to the DataGrid with Up/Down arrow buttons for each row. The `move-row-up` and `move-row-down` functions handle the array manipulation to swap adjacent rows, with proper boundary checking to disable buttons at the top/bottom of the table. This approach works reliably with the free MUI DataGrid and provides clear visual feedback to users about available actions.

Remaining minor enhancement: (3) - Make "Add Row" insert below selected row when only one row is selected.
