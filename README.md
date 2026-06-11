# PAI Performance Dashboard

This folder contains an R Shiny dashboard for the `PAI Score master` sheet in `Theme wise PAI Score of Gram Panchayat (Autosaved).xlsx`.

## Run

Double-click `run_app.cmd`, or run this from PowerShell:

```powershell
& "C:\Users\DELL\Downloads\PAI Data\run_app.cmd"
```

Then open:

```text
http://127.0.0.1:3838
```

## Dashboard Sections

- `District Table`: average Overall PDI and Theme 1-9 scores by district.
- `Block Table`: average scores by district-block, with an `All districts` view and a district filter.
- `GP Table`: GP-level scores, with `All districts` / `All blocks` views and district/block filters.

Rows are colored by Overall PDI within the current selected table: red for the bottom third, orange for the middle third, and green for the top third.
