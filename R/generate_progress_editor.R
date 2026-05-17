# Generate Progress Editor - Interactive HTML page for editing progress data
# Uses gt-rendered tables with JavaScript overlay for interactivity

#' Generate the progress editor HTML page
#' 
#' @param progress_file Path to progress RDS file
#' @param layout_file Path to layout RDS file
#' @param output_file Path to write the editor HTML
#' @return Invisible NULL, writes file as side effect
generate_progress_editor <- function(
  progress_file = "inputs/progress.RDS",
  layout_file = "site/dt_pw_lay.RDS",
  output_file = "site/_site/progress_editor.html"
) {
  
  library(data.table)
  library(gt)
  library(htmltools)
  
  # Load data
  dt_progress_raw <- load_progress_data(progress_file)
  dt_layout <- readRDS(layout_file) |> as.data.table()
  
  # Get latest date
  latest_date <- max(dt_progress_raw$Date, na.rm = TRUE)
  date_display <- format(latest_date, "%B %d, %Y")
  date_iso <- format(latest_date, "%Y-%m-%d")
  message(sprintf("Generating progress editor for %s", date_iso))
  
  # Merge progress into layout for latest date only
  dt_progress <- merge_progress_into_splash(dt_progress_raw, dt_layout, date_filter = latest_date)
  
  # Get column count
  num_cols <- max(dt_layout$layCol, na.rm = TRUE)
  
  # Prepare data for JSON embedding (only data rows, not titles)
  dt_data <- dt_progress[EntryType == "Data"]
  
  # Build JSON data for JavaScript
  json_data <- sprintf('[%s]', paste(
    apply(dt_data, 1, function(row) {
      sprintf('{"id":"%s","name":"%s","level":"%s","layCol":%s,"layoutOrder":%s,"statNumPrep":%s,"statNumSequ":%s,"statNumSucc":%s}',
        row["ID"],
        gsub('"', '\\"', row["Name"]),  # escape quotes in name
        row["Level"],
        row["layCol"],
        row["LayoutOrder"],
        ifelse(is.na(row["StatNum_Prep"]), 0, row["StatNum_Prep"]),
        ifelse(is.na(row["StatNum_Sequ"]), 0, row["StatNum_Sequ"]),
        ifelse(is.na(row["StatNum_Succ"]), 0, row["StatNum_Succ"])
      )
    }),
    collapse = ","
  ))
  
  # Render gt tables for each column
  column_html <- lapply(1:num_cols, function(col_num) {
    dt_col <- dt_progress[layCol == col_num]
    
    # Render the gt table
    gt_table <- renderSplash_group_progress(dt_col)
    gt_html <- as.character(as_raw_html(gt_table))
    
    # Wrap each table in a div with column info
    sprintf('<div class="editor-column" data-col="%d">%s</div>', col_num, gt_html)
  })
  
  columns_combined <- paste(column_html, collapse = "\n")
  
  # Build HTML using paste() to avoid sprintf format string length limits
  html_content <- paste0(
'<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Progress Editor - ', date_display, '</title>
  <style>
    * { box-sizing: border-box; }
    body { 
      font-family: Helvetica, Arial, sans-serif; 
      margin: 0; 
      padding: 10px;
      background: #f5f5f5;
    }
    
    .header {
      display: flex;
      justify-content: space-between;
      align-items: center;
      padding: 10px;
      background: white;
      border-radius: 5px;
      margin-bottom: 10px;
      box-shadow: 0 1px 3px rgba(0,0,0,0.1);
    }
    
    .header h1 {
      margin: 0;
      font-size: 18px;
    }
    
    .header-controls {
      display: flex;
      gap: 10px;
      align-items: center;
    }
    
    .header-controls button {
      padding: 8px 16px;
      font-size: 14px;
      cursor: pointer;
      border: 1px solid #ccc;
      border-radius: 4px;
      background: white;
    }
    
    .header-controls button:hover {
      background: #f0f0f0;
    }
    
    .header-controls button.primary {
      background: #4CAF50;
      color: white;
      border-color: #4CAF50;
    }
    
    .header-controls button.primary:hover {
      background: #45a049;
    }
    
    .status-info {
      font-size: 13px;
      color: #666;
    }
    
    .selected-info {
      padding: 8px 12px;
      background: #fff3cd;
      border-radius: 4px;
      font-size: 13px;
    }
    
    .columns-container {
      display: flex;
      gap: 2px;
      overflow-x: auto;
      padding: 10px;
      background: white;
      border-radius: 5px;
    }
    
    .editor-column {
      flex-shrink: 0;
    }
    
    /* Highlight selected row */
    .selected-row {
      outline: 3px solid #007bff !important;
      outline-offset: -3px;
    }
    
    /* Make rows clickable */
    table tbody tr {
      cursor: pointer;
    }
    
    table tbody tr:hover {
      filter: brightness(0.95);
    }
    
    .legend {
      margin-top: 10px;
      padding: 15px;
      background: white;
      border-radius: 5px;
      font-size: 12px;
    }
    
    .legend h4 { margin: 0 0 10px 0; }
    
    .legend p { margin: 5px 0; }
    
    .legend span {
      padding: 2px 8px;
      margin: 0 4px;
      border-radius: 2px;
    }
    
    .keyboard-help {
      margin-top: 10px;
      padding: 10px;
      background: #e7f3ff;
      border-radius: 5px;
      font-size: 12px;
    }
    
    .keyboard-help kbd {
      background: #fff;
      border: 1px solid #ccc;
      border-radius: 3px;
      padding: 2px 6px;
      font-family: monospace;
    }
  </style>
</head>
<body>

<div class="header">
  <h1>Progress Editor - ', date_display, '</h1>
  <div class="header-controls">
    <div class="selected-info" id="selectedInfo">No move selected</div>
    <div class="status-info" id="changeCount">0 changes</div>
    <input type="file" id="csvUpload" accept=".csv" style="display: none;" onchange="handleCSVUpload(event)">
    <button onclick="document.getElementById(&quot;csvUpload&quot;).click()">📂 Load CSV</button>
    <button onclick="exportCSV()" class="primary">💾 Export CSV</button>
  </div>
</div>

<div class="keyboard-help">
  <strong>Navigation:</strong> <kbd>Up/Down</kbd> or <kbd>W</kbd>/<kbd>R</kbd> move between moves, <kbd>Left/Right</kbd> or <kbd>A</kbd>/<kbd>S</kbd> between columns. Click to select, double-click to open move page.
  &nbsp;|&nbsp;
  <strong>Prep:</strong> <kbd>L</kbd>/<kbd>N</kbd> up/down
  &nbsp;|&nbsp;
  <strong>Sequencing:</strong> <kbd>U</kbd>/<kbd>E</kbd> up/down
  &nbsp;|&nbsp;
  <strong>Success:</strong> <kbd>Y</kbd>/<kbd>I</kbd> up/down
</div>

<div class="columns-container" id="columnsContainer">
', columns_combined, '
</div>

<div class="legend">
  <h4>Legend</h4>
  <p><strong>Preparation:</strong>
    <span style="background-color: #fff5ee; border: 1px solid #ddd;">0: Off cards</span>
    <span style="background-color: #FFDEAD;">1: Needs guidance</span>
    <span style="background-color: #FFD700;">2: Not prepped</span>
    <span style="background-color: #FFA07A;">3: Shaky</span>
    <span style="background-color: #7FFF00;">4: Prepped</span>
  </p>
  <p><strong>Sequencing:</strong>
    <span style="background-color: #ffffff; border: 1px solid #ddd;">0: Not needed</span>
    <span style="background-color: #EE2C2C; color: white;">1: Unused</span>
    <span style="background-color: #FFD700;">2: Underused</span>
    <span style="background-color: #2F4F4F; color: white;">3: Overused</span>
    <span style="background-color: #7FFF00;">4: Good</span>
    <span style="background-color: #66CD00;">5: Great</span>
  </p>
  <p><strong>Success:</strong>
    <span style="background-color: #ffffff; border: 1px solid #ddd;">0: N/A</span>
    <span style="background-color: #FF4500; color: white;">1: Failure</span>
    <span style="background-color: #FFD700;">2: 50-50</span>
    <span style="background-color: #BCEE68;">3: Nearly there</span>
    <span style="background-color: #66CD00;">4: Great</span>
  </p>
</div>

<script>
// Embedded move data
const moveData = ', json_data, ';
const editDate = "', date_iso, '";

// Color mappings
const prepColors = ["#fff5ee", "#FFDEAD", "#FFD700", "#FFA07A", "#7FFF00"];
const sequColors = ["#ffffff", "#EE2C2C", "#FFD700", "#2F4F4F", "#7FFF00", "#66CD00"];
const succColors = ["#ffffff", "#FF4500", "#FFD700", "#BCEE68", "#66CD00"];

// State
let selectedIndex = -1;
let changes = new Set();

// Build index from move data
const moveIndex = {};
moveData.forEach((move, idx) => {
  moveIndex[move.id] = idx;
});

// Group moves by column for navigation
function getColumnMoves() {
  const cols = {};
  moveData.forEach((move, idx) => {
    const c = move.layCol;
    if (!cols[c]) cols[c] = [];
    cols[c].push(idx);
  });
  // Sort each column by layoutOrder
  Object.keys(cols).forEach(c => {
    cols[c].sort((a, b) => moveData[a].layoutOrder - moveData[b].layoutOrder);
  });
  return cols;
}

const columnMoves = getColumnMoves();
const colNumbers = Object.keys(columnMoves).map(Number).sort((a,b) => a-b);

// Match rows to move data by order within each column
function initializeRowMapping() {
  const columns = {};
  
  // Group moves by column
  moveData.forEach((move, idx) => {
    if (!columns[move.layCol]) columns[move.layCol] = [];
    columns[move.layCol].push({ move, dataIndex: idx });
  });
  
  // Sort each column by layoutOrder
  Object.keys(columns).forEach(col => {
    columns[col].sort((a, b) => a.move.layoutOrder - b.move.layoutOrder);
  });
  
  // Match to DOM rows
  document.querySelectorAll(".editor-column").forEach(colEl => {
    const colNum = parseInt(colEl.dataset.col);
    const colMoves = columns[colNum] || [];
    
    let moveIdx = 0;
    colEl.querySelectorAll("table tbody tr").forEach(tr => {
      const cells = tr.querySelectorAll("td");
      // Data rows have 5+ cells: Level, Name, Prep, Sequ, Succ
      // Title rows have an EMPTY Level cell (cells[0])
      // Data rows have content in Level like "1", "2", "3L", etc.
      if (cells.length >= 5 && moveIdx < colMoves.length) {
        const levelCell = cells[0];
        const levelText = (levelCell.textContent || levelCell.innerText || "").trim();
        
        // Only data rows have a non-empty Level
        if (levelText !== "") {
          const moveInfo = colMoves[moveIdx];
          tr.dataset.moveId = moveInfo.move.id;
          tr.dataset.dataIndex = moveInfo.dataIndex;
          moveIdx++;
        }
      }
    });
  });
}

function selectRow(index) {
  // Deselect previous
  document.querySelectorAll(".selected-row").forEach(el => {
    el.classList.remove("selected-row");
  });
  
  if (index < 0 || index >= moveData.length) {
    selectedIndex = -1;
    document.getElementById("selectedInfo").textContent = "No move selected";
    return;
  }
  
  selectedIndex = index;
  const move = moveData[index];
  
  // Find and highlight the row
  const row = document.querySelector(`tr[data-data-index="${index}"]`);
  if (row) {
    row.classList.add("selected-row");
    row.scrollIntoView({ behavior: "smooth", block: "center", inline: "center" });
  }
  
  // Update info display
  document.getElementById("selectedInfo").innerHTML = 
    `<strong>${move.name}</strong> (${move.id}) | Prep: ${move.statNumPrep} | Seq: ${move.statNumSequ} | Succ: ${move.statNumSucc}`;
}

function updateProgressCell(row, cellIndex, value, colors) {
  const cells = row.querySelectorAll("td");
  if (cells[cellIndex]) {
    cells[cellIndex].style.backgroundColor = colors[value] || colors[0];
  }
}

function updateRowColors(index) {
  const move = moveData[index];
  const row = document.querySelector(`tr[data-data-index="${index}"]`);
  if (!row) return;
  
  // Cells are: Level(0), Name(1), Prep(2), Sequ(3), Succ(4)
  updateProgressCell(row, 2, move.statNumPrep, prepColors);
  updateProgressCell(row, 3, move.statNumSequ, sequColors);
  updateProgressCell(row, 4, move.statNumSucc, succColors);
  
  // Add wrench symbol to edited moves
  const nameCell = row.querySelectorAll("td")[1];
  if (nameCell && changes.has(move.id)) {
    // Only add if not already there
    if (!nameCell.innerHTML.includes("🔧")) {
      nameCell.innerHTML = "🔧 " + nameCell.innerHTML;
    }
  }
}

function modifyValue(property, delta, maxVal) {
  if (selectedIndex < 0) return;
  
  const move = moveData[selectedIndex];
  const oldVal = move[property];
  const newVal = Math.max(0, Math.min(maxVal, oldVal + delta));
  
  if (newVal !== oldVal) {
    move[property] = newVal;
    changes.add(move.id);
    updateRowColors(selectedIndex);
    selectRow(selectedIndex);  // Update info display
    document.getElementById("changeCount").textContent = `${changes.size} changes`;
  }
}

function findPositionInColumn(dataIndex) {
  const move = moveData[dataIndex];
  const col = move.layCol;
  const colMoves = columnMoves[col];
  const posInCol = colMoves.indexOf(dataIndex);
  return { col, posInCol, colMoves };
}

function handleKeyDown(e) {
  const key = e.key.toLowerCase();
  
  // Navigation - up/down within column (W/R for Colemak)
  if (key === "arrowup" || key === "w") {
    e.preventDefault();
    if (selectedIndex < 0) {
      selectRow(0);
    } else {
      const { col, posInCol, colMoves } = findPositionInColumn(selectedIndex);
      if (posInCol > 0) {
        selectRow(colMoves[posInCol - 1]);
      }
    }
  }
  else if (key === "arrowdown" || key === "r") {
    e.preventDefault();
    if (selectedIndex < 0) {
      selectRow(0);
    } else {
      const { col, posInCol, colMoves } = findPositionInColumn(selectedIndex);
      if (posInCol < colMoves.length - 1) {
        selectRow(colMoves[posInCol + 1]);
      }
    }
  }
  // Navigation - left/right between columns (A/S for Colemak)
  else if (key === "arrowleft" || key === "a") {
    e.preventDefault();
    if (selectedIndex < 0) {
      selectRow(0);
    } else {
      const { col, posInCol } = findPositionInColumn(selectedIndex);
      const colIdx = colNumbers.indexOf(col);
      if (colIdx > 0) {
        const newCol = colNumbers[colIdx - 1];
        const newColMoves = columnMoves[newCol];
        const newPos = Math.min(posInCol, newColMoves.length - 1);
        selectRow(newColMoves[newPos]);
      }
    }
  }
  else if (key === "arrowright" || key === "s") {
    e.preventDefault();
    if (selectedIndex < 0) {
      selectRow(0);
    } else {
      const { col, posInCol } = findPositionInColumn(selectedIndex);
      const colIdx = colNumbers.indexOf(col);
      if (colIdx < colNumbers.length - 1) {
        const newCol = colNumbers[colIdx + 1];
        const newColMoves = columnMoves[newCol];
        const newPos = Math.min(posInCol, newColMoves.length - 1);
        selectRow(newColMoves[newPos]);
      }
    }
  }
  // Prep: L/N
  else if (key === "l") {
    e.preventDefault();
    modifyValue("statNumPrep", 1, 4);
  }
  else if (key === "n") {
    e.preventDefault();
    modifyValue("statNumPrep", -1, 4);
  }
  // Sequencing: U/E
  else if (key === "u") {
    e.preventDefault();
    modifyValue("statNumSequ", 1, 5);
  }
  else if (key === "e") {
    e.preventDefault();
    modifyValue("statNumSequ", -1, 5);
  }
  // Success: Y/I
  else if (key === "y") {
    e.preventDefault();
    modifyValue("statNumSucc", 1, 4);
  }
  else if (key === "i") {
    e.preventDefault();
    modifyValue("statNumSucc", -1, 4);
  }
}

function handleRowClick(e) {
  const row = e.target.closest("tr[data-data-index]");
  if (row) {
    const index = parseInt(row.dataset.dataIndex);
    selectRow(index);
  }
}

function handleRowDblClick(e) {
  const row = e.target.closest("tr[data-data-index]");
  if (row) {
    const index = parseInt(row.dataset.dataIndex);
    const move = moveData[index];
    if (move && move.id) {
      // Build the move page URL - sanitize ID for filename (only replace / with _)
      const safeId = move.id.replace(/\\//g, "_");
      const url = `moves/${safeId}.html`;
      window.open(url, "_blank");
    }
  }
}

// Text mappings for export
const prepText = ["Off the cards", "Needs guidance", "Not prepped", "Shaky", "Prepped"];
const sequText = ["Not needed", "Unused", "Underused", "Overused", "Good", "Great"];
const succText = ["NA", "Failure", "50-50", "Nearly there", "Great"];

// Reverse mappings for import
const prepTextToNum = {};
prepText.forEach((t, i) => prepTextToNum[t.toLowerCase()] = i);
const sequTextToNum = {};
sequText.forEach((t, i) => sequTextToNum[t.toLowerCase()] = i);
const succTextToNum = {};
succText.forEach((t, i) => succTextToNum[t.toLowerCase()] = i);

function handleCSVUpload(event) {
  const file = event.target.files[0];
  if (!file) return;
  
  const reader = new FileReader();
  reader.onload = function(e) {
    const text = e.target.result;
    const lines = text.split(/\\r?\\n/).filter(line => line.trim());
    
    if (lines.length < 2) {
      alert("CSV file is empty or has no data rows");
      return;
    }
    
    // Parse header
    const header = parseCSVLine(lines[0]);
    const idCol = header.findIndex(h => h.toLowerCase() === "id");
    const prepNumCol = header.findIndex(h => h.toLowerCase() === "statnum_prep");
    const sequNumCol = header.findIndex(h => h.toLowerCase() === "statnum_sequ");
    const succNumCol = header.findIndex(h => h.toLowerCase() === "statnum_succ");
    
    // Fallback to text columns if numeric not found
    const prepTextCol = header.findIndex(h => h.toLowerCase() === "preperation");
    const sequTextCol = header.findIndex(h => h.toLowerCase() === "sequencing");
    const succTextCol = header.findIndex(h => h.toLowerCase() === "success");
    
    if (idCol < 0) {
      alert("CSV must have an ID column");
      return;
    }
    
    let updated = 0;
    let notFound = 0;
    
    // Process data rows
    for (let i = 1; i < lines.length; i++) {
      const cols = parseCSVLine(lines[i]);
      if (cols.length <= idCol) continue;
      
      const id = cols[idCol];
      const idx = moveIndex[id];
      
      if (idx === undefined) {
        notFound++;
        continue;
      }
      
      const move = moveData[idx];
      let changed = false;
      
      // Update prep
      if (prepNumCol >= 0 && cols[prepNumCol] !== undefined) {
        const val = parseInt(cols[prepNumCol]);
        if (!isNaN(val) && val >= 0 && val <= 4 && val !== move.statNumPrep) {
          move.statNumPrep = val;
          changed = true;
        }
      } else if (prepTextCol >= 0 && cols[prepTextCol]) {
        const val = prepTextToNum[cols[prepTextCol].toLowerCase()];
        if (val !== undefined && val !== move.statNumPrep) {
          move.statNumPrep = val;
          changed = true;
        }
      }
      
      // Update sequ
      if (sequNumCol >= 0 && cols[sequNumCol] !== undefined) {
        const val = parseInt(cols[sequNumCol]);
        if (!isNaN(val) && val >= 0 && val <= 5 && val !== move.statNumSequ) {
          move.statNumSequ = val;
          changed = true;
        }
      } else if (sequTextCol >= 0 && cols[sequTextCol]) {
        const val = sequTextToNum[cols[sequTextCol].toLowerCase()];
        if (val !== undefined && val !== move.statNumSequ) {
          move.statNumSequ = val;
          changed = true;
        }
      }
      
      // Update succ
      if (succNumCol >= 0 && cols[succNumCol] !== undefined) {
        const val = parseInt(cols[succNumCol]);
        if (!isNaN(val) && val >= 0 && val <= 4 && val !== move.statNumSucc) {
          move.statNumSucc = val;
          changed = true;
        }
      } else if (succTextCol >= 0 && cols[succTextCol]) {
        const val = succTextToNum[cols[succTextCol].toLowerCase()];
        if (val !== undefined && val !== move.statNumSucc) {
          move.statNumSucc = val;
          changed = true;
        }
      }
      
      if (changed) {
        changes.add(id);
        updateRowColors(idx);
        updated++;
      }
    }
    
    document.getElementById("changeCount").textContent = `${changes.size} changes`;
    
    let msg = `Loaded ${file.name}\\n\\nUpdated: ${updated} moves`;
    if (notFound > 0) msg += `\\nNot found: ${notFound} IDs`;
    alert(msg);
    
    // Reset file input so same file can be re-uploaded
    event.target.value = "";
  };
  
  reader.readAsText(file);
}

// Parse a CSV line handling quoted fields
function parseCSVLine(line) {
  const result = [];
  let current = "";
  let inQuotes = false;
  
  for (let i = 0; i < line.length; i++) {
    const char = line[i];
    
    if (char === \'"\' && !inQuotes) {
      inQuotes = true;
    } else if (char === \'"\' && inQuotes) {
      if (line[i + 1] === \'"\') {
        current += \'"\';
        i++;
      } else {
        inQuotes = false;
      }
    } else if (char === "," && !inQuotes) {
      result.push(current.trim());
      current = "";
    } else {
      current += char;
    }
  }
  result.push(current.trim());
  
  return result;
}

function exportCSV() {
  const headers = ["Date", "ID", "Preperation", "Sequencing", "Success", "StatNum_Prep", "StatNum_Sequ", "StatNum_Succ"];
  const rows = [headers.join(",")];
  
  moveData.forEach(move => {
    const row = [
      editDate,
      move.id,
      prepText[move.statNumPrep] || "",
      sequText[move.statNumSequ] || "",
      succText[move.statNumSucc] || "",
      move.statNumPrep,
      move.statNumSequ,
      move.statNumSucc
    ];
    rows.push(row.map(v => `"${v}"`).join(","));
  });
  
  const csv = rows.join("\\n");
  const blob = new Blob([csv], { type: "text/csv" });
  const url = URL.createObjectURL(blob);
  
  const a = document.createElement("a");
  a.href = url;
  a.download = `progress_${editDate}.csv`;
  document.body.appendChild(a);
  a.click();
  document.body.removeChild(a);
  URL.revokeObjectURL(url);
}

// Initialize
document.addEventListener("DOMContentLoaded", () => {
  initializeRowMapping();
  document.addEventListener("keydown", handleKeyDown);
  document.getElementById("columnsContainer").addEventListener("click", handleRowClick);
  document.getElementById("columnsContainer").addEventListener("dblclick", handleRowDblClick);
});
</script>

</body>
</html>'
  )
  
  # Ensure output directory exists
  dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
  
  # Write the file
  writeLines(html_content, output_file)
  message(sprintf("Progress editor written to: %s", output_file))
  
  invisible(NULL)
}
