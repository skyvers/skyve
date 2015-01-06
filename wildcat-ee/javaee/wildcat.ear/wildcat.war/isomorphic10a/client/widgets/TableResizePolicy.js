/*
 * Isomorphic SmartClient
 * Version v10.0p_2015-01-04 (2015-01-04)
 * Copyright(c) 1998 and beyond Isomorphic Software, Inc. All rights reserved.
 * "SmartClient" is a trademark of Isomorphic Software, Inc.
 *
 * licensing@smartclient.com
 *
 * http://smartclient.com/license
 */

 





isc.Canvas.addClassMethods({

//>	@method	Canvas.applyTableResizePolicy()	(A)
// Given a set of items to be shown in a table, this method determines the sizing / positioning
// to be applied to each item.
//
// We factor the placing of titles next to elements into the table here to have them
// automatically take up columns in the output.
//
// Sets up _rowHeights and _colWidths on the items array
// Sets up _size property (2 element array for width,height) and _titleWidth on each item.
//
// @group drawing
//<
// Note:
// The "_rowTable" property stored on the passed-in items can be reused IF no new items are
// visible that were not visible before.  It is up to the calling function to clear out an old
// _rowHeights if necessary.
//
//

applyTableResizePolicy : function (items, totalWidth, totalHeight, 
                                   numCols, colWidths, rowHeights, overflowedAsFixed) {
    var logDebug = this.logIsDebugEnabled("tablePolicy"),
        logInfo = this.logIsInfoEnabled("tablePolicy"),
        logPlacement = this.logIsDebugEnabled("tablePlacement");

    // determine row and column start and end coordinates for each item based on rowSpan,
    // colSpan, startRow and endrow properties

    var rowTable = items._rowTable;
    
    // If we've previously run the resizePolicy and it is still valid, don't do the
    // work again.
    if (!this._tableResizePolicyIsValid(items)) {
    	
        // determine row and column start and end coordinates for each item based on rowSpan,
        // colSpan, startRow and endrow properties
		rowTable = items._rowTable = [];

        var currentRow = 0,
            currentCol = 0
        ;
            
		// iterate through the items, 
		//	placing them in a rowTable according to their rowSpan and colSpan entries
		for (var itemNum = 0; itemNum < items.length; itemNum++) {
			// get a pointer to the item
			var item = items[itemNum];
			
			// if the item is not visible, skip it
			// NOTE: an algorithm BEFORE this one might want to mark items as invisible
			//			based on a showIf property or something like that
			if (!item.alwaysTakeSpace && !item.visible) continue;
			
			var itemCols = item.getColSpan(), 
				itemRows = item.getRowSpan();

			// if the item has rowSpan == 0 or colSpan == 0, skip it
			//  this lets us ignore items that should be output (and thus are visible)
			//	but don't factor into the table
			if (itemRows == 0 || itemCols == 0) continue;
            
            if (itemCols == null) itemCols = 1;
            if (itemRows == null) itemRows = 1;

            var requiredCols = itemCols;
            if (itemCols == "*") requiredCols = 1;

            // add another column for a separate cell for left/right oriented titles
            // NOTE: extra cells not added for top or bottom-oriented titles
            var orientation = item.getTitleOrientation();
            if (item.showTitle &&
                (orientation == isc.Canvas.LEFT || orientation == isc.Canvas.RIGHT)) 
            {
                // NOTE: we assume colSpan * and showTitle:true means at least two columns
                requiredCols += (item.getTitleColSpan() || 1);
                if (itemCols != "*") itemCols += (item.getTitleColSpan() || 1);
            }

            var startRow = (item.isStartRow ? item.isStartRow() : item.startRow),
                endRow = (item.isEndRow ? item.isEndRow() : item.endRow);

            if (logPlacement) {
                this.logDebug("at: " + ["row" + currentRow, "col" + currentCol] + 
                              ", item: " + (item.name || item.Class) + 
                              // report colSpan "*" separately since the actual number of
                              // columns we'll take up isn't computed til later, requiredCols
                              // just represents the number of columns the item *must* have
                              (itemCols == "*" ? ", colSpan:'*'" : "") +
                              ", required cols:" + requiredCols +
                              (itemRows > 1 ? ", rowSpan:" + itemRows : "") +
                              (startRow ? ", startRow:true" : "") +
                              (endRow ? ", endRow:true" : ""),
                              "tablePlacement");
            }

            var placeRow = null, placeCol = null;

            

            if (currentCol >= numCols || (startRow && currentCol != 0)) {
                currentRow++;
                currentCol = 0;
                item._startRow = true;
                //this.logWarn("advanced to row: " + currentRow);
            } else { item._startRow = false; }

            // if we're within the table, see if we can place the item in an existing row
            // NOTE: rowSpanning items in this and previous rows means there may be several
            // partially filled rows to look through for sufficient open space for this item.
            if (currentRow < rowTable.length) {
                //this.logWarn("looking in existing rows starting at: " + currentRow);
                // find the next row with available space
                for (; currentRow < rowTable.length; currentRow++) {
                    var rowSlots = rowTable[currentRow];

                    //this.logWarn("trying row: " + currentRow);

                    // no row created yet
                    if (rowSlots == null) break;

                    // look for an open slot
                    for (; currentCol < numCols; currentCol++) {
                        if (rowSlots[currentCol] != null) continue;

                        // check that there are enough open slots in a row to accommodate the
                        // colSpan of this item.  This covers the case of cells reserved by
                        // rowSpanning items in previous rows.
                        for (var j = currentCol; j < numCols; j++) {
                            //this.logWarn("checking at open spot in column: " + currentCol);
                            // ran into an occupied slot before we found a spot
                            if (rowSlots[j] != null) break;

                            if ((j - currentCol) + 1 >= requiredCols) {
                                // there's enough room to accommodate this item starting at
                                // column i on this row.
                                // Note that we don't have to check for cells reserved in rows
                                // beneath us.  Only items from rows above us could possibly
                                // have reserved cells beneath us, and they'd have to reserve
                                // the intervening cells.
                                placeRow = currentRow;
                                placeCol = currentCol;
                                break;
                            }
                        }
                        if (placeCol != null) break;
                    }
                    if (placeCol != null) break;
                    // moving on to new row, go back to first column
                    //this.logWarn("no spot in row: " + currentRow + ", advancing");
                    currentCol = 0;
                    item._startRow = true;
                }	
                //if (placeCol != null) this.logWarn("found spot in row: " + currentRow);
            }
            // no spots in existing rows, create a new row
            if (placeCol == null) {
                //this.logWarn("created new row: " + currentRow);
                placeRow = currentRow;
                placeCol = 0;
                item._startRow = true;
                // NOTE: an item with an invalid colSpan which is > numCols will never be
                // placed on an existing row, hence hits this case and ends up at column 0 of a
                // new row.
            }
				
            currentCol = placeCol;
				
			// if colSpan is variable, now that we've picked a row we can resolve it
			if (itemCols == "*") itemCols = numCols - currentCol;
		
			// NOTE: rowSpan of "*" not supported!
			// this is because we don't know how many rows there will be, so we don't
			//	have a good way to assign the item to each row going down (?)
			if (!isc.isA.Number(itemRows)) itemRows = 1;
			
			// note the shape of this item in the rowTable (fill in the grid)
			// for each row to output
            
			for (var r = currentRow; r < currentRow + itemRows; r++) {
				// if there's not a column array in that row, create one
				if (!rowTable[r]) rowTable[r] = [];
				// for each column to output
				for (var c = currentCol; c < currentCol + itemCols; c++) {
					// stick the number of this item in the row
					rowTable[r][c] = itemNum;
				}
			}
			
			// have the item remember how many rows and columns it's actually taking up
			// as an array of numbers:   [startCol, startRow, endCol, endRow]
			//	NOTE: endCol and endRow are NOT inclusive
			item._tablePlacement = [placeCol, placeRow, 
                                    placeCol + itemCols, placeRow + itemRows];
        
            

			// advance currentCol by the number of columns taken up
			currentCol += itemCols;
			// if the item is configured to end its row, advance past the last column in the
            // row, so the next iteration of the loop will start the new row
			if (endRow) currentCol = numCols;

            if (logPlacement) {
                this.logDebug("item: " + (item.name || item.Class) + 
                              " placed at: " + ["row" + placeRow, "col" + placeCol] +
                              (item._startRow ? ", marked startRow " : "") +
                              ", rowTable: " + this.echoAll(rowTable), "tablePlacement");
            }
		}		

    	// at this point, we know the row and column coordinate where each item will be placed

        
        var emptyRows = [];
        for (var r = 0; r < rowTable.length; r++) {
            var row = rowTable[r];
            if (row == null) break;
        
            var emptyCells = 0, lastItem = null;
            for (var c = 0; c < row.length; c++) {
                // empty cell
                if (row[c] == null) {
                    emptyCells++; 
                    continue;
                }
                // cell spanned by item in previous row
                if (r > 0 && rowTable[r-1] != null && row[c] == rowTable[r-1][c]) continue;

                // occupied cell
                var itemNum = row[c],
                    item = items[itemNum];

                // if we're still in the same colSpanning item, continue
                if (item == lastItem || item == null) continue;

                // mark this item with the number of empty cells and rows that precede it
                item._emptyRows = emptyRows;
                item._emptyCells = emptyCells;
                if (logPlacement && (emptyCells > 0 || emptyRows.length > 0)) {
                    this.logDebug("itemNum:" + itemNum + " (" + (item.name || item.Class) + 
                                  ") at: " + ["row" + placeRow, "col" + placeCol] +
                                  " preceded by " +
                                  (emptyCells > 0 ? emptyCells + " empty cells" : "") +
                                  (emptyRows.length > 0 ?
                                     " " + emptyRows.length + " empty rows" : ""),
                                  "tablePlacement");
                }
                // reset the counter
                emptyCells = 0;
                emptyRows = [];
                lastItem = item;
            }
            // if we didn't encounter any items on this row, we need to skip a row
            // Record how many empty cells are in this row
            if (lastItem == null) {
                emptyRows.add(emptyCells + (numCols-row.length));
                emptyCells = 0;
            }
        }
        // if we have empty rows beyond the last item(s) in the table, reduce the
        // rowSpan specification of those items.
        
        if (emptyRows != null && rowTable.length > 0) {
            var emptyRowCount = emptyRows.length;
            var row = rowTable[rowTable.length-1];
            for (var c = 0; c < row.length; c++) {
                var itemNum = row[c];
                    item = items[itemNum];
                if (item == null) continue;
                
                var rowSpan = item._tablePlacement[3] - item._tablePlacement[1];
                rowSpan -= emptyRowCount;
                item._rowSpan = rowSpan;
            }
        
        }
	}

	// if column widths were not specified, calculate them from the rowTable
	if (!colWidths || !isc.isAn.Array(colWidths)) {	// && !items.colWidths) {
		//>DEBUG
		if (!isc.isAn.Array(colWidths)) {
			this.logWarn(" 'colWidths' not an array - Ignoring.", "tableResizePolicy");
		}
		//<DEBUG
		
        
		colWidths = [];
	}
	
	// transform any "*" or "%" items in the colWidths to things the stretchResizeList can deal
    // with.  NOTE: don't modify the passed-in Array
    colWidths = colWidths.duplicate();
	for (var c = 0; c < colWidths.length; c++) {
		//	colWidths[c] = [colMinWidth, rowMaxWidth, colMaxPercent, colStarCount];

		var width = colWidths[c];
		if (isc.isA.String(width)) {
			if (width == "*") colWidths[c] = [0, 10000, 0, 1];
			else if (width.contains("*")) colWidths[c] = [0, 10000, 0, parseInt(width)];
			else if (width.contains("%")) colWidths[c] = [0, 10000, parseInt(width), 0];
            // catch a quoted number and convert it to a real number
            else {
                var parsed = parseInt(width);
                if (parsed == width) {
                    colWidths[c] = parsed;
                } else {
                    this.logWarn("Failed to understand specified colWidth:"+ width);
                    // treat as "*"
                    colWidths[c] = [0,10000,0,1];
                }
            }
		}
	}
	
	// remember the colWidths in the items
	items.colWidths = colWidths;

	// look through all the items in each row and gather:
    // [ min pixel height,
    //   max pixel height, 
    //   largest "*" size, 
    //   largest percent size ]
	if (!rowHeights) {// && !items.rowHeights) {
		rowHeights = [];

		// for each row in the rowTable
		for (var r = 0; r < rowTable.length; r++) {
			var row = rowTable[r],
				rowMinHeight = null,
				rowMaxHeight = 100000,
				rowMaxPercent = 0,
				rowStarCount = 0
			;
			if (!row) continue;
			
			// for each column in that row
			for (var c = 0; c < row.length; c++) {
				// get the item and its preferred height
				var item = items[row[c]];
				if (!item) continue;

				var itemHeight = item.getCellHeight(overflowedAsFixed);
				
				// if the item takes up more than one row, split evenly amongst its rows ???
				var itemRows = (item._tablePlacement[3] - item._tablePlacement[1]);

                if (logDebug) this.logWarn("item at: " + [r,c] + " has height: " + itemHeight +
                                           ", item is: " + item);

                item._isVariableHeight = false;

				// if the itemHeight is a number
				if (isc.isA.Number(itemHeight)) {
					// NOTE: if the item takes up more than one row, split it evenly across its
                    // rows
					itemHeight = Math.floor(itemHeight / itemRows);

                    if (logDebug) this.logWarn("item: " + item + " has pixel size: " + itemHeight);

                    // if this is the first item to specify a pixel size, or is larger than any
                    // previous specified size or minimum size, it becomes the new minimum
					if (rowMinHeight == null || itemHeight > rowMinHeight) {
                        rowMinHeight = itemHeight;
                    }

                    // if this item specifies a pixel size larger than a previously specified
                    // max, raise the max height for the row as a whole
					if (itemHeight > rowMaxHeight) rowMaxHeight = itemHeight;

				// if the itemHeight is a string (a relative size)
				} else if (isc.isA.String(itemHeight)) {
					// if height is "*" or "2*"
					if (itemHeight.contains("*")) {
                        item._isVariableHeight = true;

						// get the starCount as a number
						// NOTE: if the item takes up more than one row, split it evenly across
                        // its rows
						var itemStarCount = (itemHeight == "*" ? 1 : parseFloat(itemHeight)) 
                                                    / itemRows;

                        if (logDebug) this.logWarn("item: " + item + " has star size: " + itemStarCount);

						rowStarCount = Math.max(rowStarCount, itemStarCount);
						
					// else if height is a percentage
					} else {
                        item._isVariableHeight = true;

						// get the percentage as a number
						// NOTE: if the item takes up more than one row, split it evenly across
                        // its rows
						var itemPercent = parseFloat(itemHeight) / itemRows;

                        if (logDebug) this.logWarn("item: " + item + " has percent size: " + itemPercent);
						
						// and remember it if it's greater than the max percent already seen in
                        // this row
						if (itemPercent > rowMaxPercent) rowMaxPercent = itemPercent;
                    }

                    // check for minHeight / maxHeight settings on flexible-sized items

					// the row must be as big as the minHeight of the item
					if (item.minHeight > rowMinHeight) {
						rowMinHeight = item.minHeight;
					}
						
                    // NOTE: minimums should win out over maximums

                    // allow an item's minHeight to win out over another item's previously
                    // specified maxHeight
					if (item.minHeight > rowMaxHeight) {
						rowMaxHeight = item.minHeight;
					}

                    // lower row maxHeight only to the largest previously specified
                    // item.minHeight (rowMinHeight)
					if (item.maxHeight < rowMaxHeight && 
						rowMinHeight < item.maxHeight) 
                    {
						rowMaxHeight = item.maxHeight
					}
				}

				// remember the characteristics of this row
				// if a percentage or star was found, remember all the values
				if (rowMaxPercent > 0 || rowStarCount > 0) {
                    // no one set a pixel size or minHeight.  Default to 0
                    if (rowMinHeight == null) rowMinHeight = 0;
					rowHeights[r] = [rowMinHeight, rowMaxHeight, rowMaxPercent, rowStarCount];
				} else {
                    if (rowMinHeight == null) {
                        // there were no specified sizes for the row (pixel, '*' or percent)
                        rowMinHeight = items._defaultRowHeight || 22;
                    }
					rowHeights[r] = rowMinHeight;
				}
			}
		}
	}
	// remember the rowHeights in the items
	items.rowHeights = rowHeights;

    if (logInfo) this.logInfo("\ntotalWidth: " + totalWidth + 
                              ", totalHeight: " + totalHeight + 
                              "\nspecified sizes:\n" +
                              "cols:" + this.echoAll(items.colWidths) + 
                              ", rows: " + this.echoAll(items.rowHeights),
                              "tablePolicy");

	// get real row and column sizes
	items._colWidths = colWidths = isc.Canvas.stretchResizeList(items.colWidths, totalWidth);
	items._rowHeights = rowHeights = isc.Canvas.stretchResizeList(items.rowHeights, totalHeight);

    if (logInfo) this.logInfo("\nderived sizes:\n" +
                              "cols:" + this.echoAll(items._colWidths) + 
                              ", rows: " + this.echoAll(items._rowHeights),
                              "tablePolicy");

    // we have widths and heights for each column and row.  Now apply those sizes to the items,
    // which may span multiple columns or rows
    // NOTE: we currently only support "*" sizes, not percents
	for (itemNum = 0; itemNum < items.length; itemNum++) {
		item = items[itemNum];
		if (!item.visible) continue;
        var isACanvas = isc.isA.Canvas(item),
            isACanvasItem = !isACanvas && isc.isA.CanvasItem(item),
            width = isACanvasItem ? (item.canvas && item.canvas._userWidth) || item.width : item.getWidth(),
			height = isACanvas ? item.getHeight() : item.getCellHeight(overflowedAsFixed),
            orientation = item.getTitleOrientation(),
            placement = item._tablePlacement,
            // We need the derived title width in order to manage title cell clipping properly
            // in form items. If we're not showing a title, of course this will be zero.
            titleWidth = 0;

        if (placement == null) continue;

        if (item.showTitle) {
            if (orientation == isc.Canvas.LEFT) {
                titleWidth = colWidths[placement[0]];
            } else {
                titleWidth = colWidths[placement[2]];
            }
        }

		// account for variable width items.  NOTE: we don't support percent widths on items
		if (width == "*" || width == "100%") {
			width = 0;
            
			var colSpan = item.getTitleColSpan() || 1,
                skipBefore = (item.showTitle && orientation == isc.Canvas.LEFT) ? colSpan : 0,
				skipAfter = (item.showTitle && orientation == isc.Canvas.RIGHT) ? colSpan : 0,
                startCol = placement[0] + skipBefore,
                endCol = Math.min(colWidths.length, placement[2] - skipAfter)
                
            ;

            //this.logWarn("item ID: " + item.ID + ", startCol: " + startCol +
            //             ", endCol: " + endCol + ", colWidths: " + colWidths);
    
			for (var c = startCol; c < endCol; c++) {
				width += colWidths[c];
			}
            
		}

		// account for variable height items
		if (item._isVariableHeight) {
			height = 0;
            var startRow = placement[1], endRow = placement[3];

            // NOTE: don't need logic for extra cells for titles, because extra cells aren't
            // added for top or bottom-oriented titles
			for (var c = startRow; c < endRow; c++) {
				height += rowHeights[c];
			}
		}

		// remember the width and height of the item
		item._size = [width, height];
        // Remember the width of the item title
        item._titleWidth = titleWidth
	}
},

// This method should determine whether
// - tableResizePolicy has been run on this table already
// - any items visibility have changed since the policy was run
// - any items have been moved within the items array (or items removed / new items introduced)
_tableResizePolicyIsValid : function (items) {
    
    if (!items._rowTable) return false;
    return true;
},

// Helper method to mark an already run policy as invalid.
invalidateTableResizePolicy : function (items) {
    delete items._rowTable;
    delete items._rowHeights;
	delete items._colWidths;
},


//>	@method	Canvas.stretchResizeList()	(A)
// 		Given a list of inputs sizes as:
//			a number 
//				or
//			[minSize, maxSize, maxPercent, starCount]
//		and a totalSize, figure out the size of the dynamically sized items
//		according to the totalSize.
//
//		You can use percentages or fixed sizes to go beyond the totalSize
//
//		@group	drawing
//		@param	inputSizes		(array)		array of sizes (see above)
//		@param	totalSize		(number)	total sizes for the 
//
//		@return	(number[])				output sizes (all numbers)
//<
stretchResizeList : function (inputSizes, totalSize) {
	var totalPercent = 0,  // amount "%" items amount to
		starCount = 0,     // number of "*" star items
		totalFixed = 0,    // total space taken up by fixed-size items
		outputSizes = inputSizes.duplicate();

	for (var i = 0; i < inputSizes.length; i++) {
		var size = outputSizes[i];

		if (isc.isA.Number(size)) {
			// fixed size item
			size = Math.max(size,1); // assure at least 1
			totalFixed += size; 
			outputSizes[i] = size;
		} else {
            // variable (% / * / both) sized item
			var rowPercent = size[2],
				rowStarCount = size[3]
			;
			// if a percent without a "*"
			if (rowStarCount == 0) {
				// percentage -- add it to the percentage total
				totalPercent += rowPercent;
			}
			// tracked total amount of "*"s
			starCount += rowStarCount;
		}
	}

	// at this point, 
	// - totalFixed is the total of the fixed, absolute sizes
	// - totalPercent is the total percentage numbers (that aren't stars)
	// - starCount is the total number of stars across all rows (even if those rows also have
    //   percents specified)


    // - "stars" are translated to percents, sharing all remaining percent points (of 100)
    //   not allocated to specified percent sizes
    // - stars and percents share all space not allocated to static numbers
    // - if there are any percents or stars, all space is always filled
	if (starCount) {
        var starPercent = 0;
        if (totalPercent < 100) {
            // star sized items share the remaining percent size
            starPercent = (100 - totalPercent) / starCount;
        }

	    // assign a percentage to each star item
    	//	if a row has both a star and a percentage, keep the larger item
        
	    for (var r = 0; r < inputSizes.length; r++) {
    		var size = outputSizes[r];

	    	if (isc.isA.Number(size)) continue; // skip fixed size items
	    	var rowPercent = size[2],
			    rowStarCount = size[3],
		    	rowStarPercent = rowStarCount * starPercent;
    		// if the total percentage from stars is greater than the fixed percent
	    	if (rowPercent < rowStarPercent) {
		    	// change the fixed percent
			    size[2] = rowStarPercent;
    		}

    		// if this item had stars, it has not yet been included in totalPercent (even if it
            // specified both star and percent), so now include it's percent in totalPercent.
            // NB: We rely on "totalPercent" to be correct when we subsequently divy the
            // remainingSpace among items with percents; if it's wrong over/underflow will
            // occur.  However totalPercent does not need to equal 100 because percents are
            // just treated as proportions.
            if (rowStarCount > 0) totalPercent += size[2];
    	}
    }

	// at this point, 
	// - totalFixed is still the total of the fixed, absolute sizes
	// - totalPercent is the total percentage (including what used to be stars)
	// - we have no stars left

    // if nothing has variable size, we're done
	if (totalPercent <= 0) return outputSizes;

	var remainingSpace = Math.max(0, totalSize - totalFixed);

    //this.logWarn("remaining space: " + remainingSpace + 
    //             ", totalPercent: " + totalPercent);
	
	// apply mins and maximums.  Note if an item gets set to its min or max, the behavior is
    // exactly as though the item had originally specified that fixed size.  remainingSpace is
    // reduced along with the totalPercent it was being divided by.  Note that when this
    // happens for a min, all other items get smaller, or for a max, all other items get
    // larger, so we have to recheck any previous mins or maxs.
	for (var r = 0; r < inputSizes.length; r++) {
		var	pixelsPerPercent = Math.max(0, remainingSpace / totalPercent),
			size = outputSizes[r];
			
		if (isc.isA.Number(size)) continue;
			
		var min = size[0];
        if (min == 0) continue;

		var itemPercent = size[2],
			itemPixels = pixelsPerPercent * itemPercent;

		if (itemPixels < min) {
			outputSizes[r] = min;
			remainingSpace -= min;
			totalPercent -= itemPercent;
            // NOTE: we really only have to go back to the last non-zero minimum
			r = 0;
		}
	}
		
	// check maximums
	for (var r = 0; r < inputSizes.length; r++) {
		var	pixelsPerPercent = Math.max(0, remainingSpace / totalPercent),
			size = outputSizes[r];
		
		if (isc.isA.Number(size)) continue;

		var max = size[1],
			itemPercent = size[2],
			itemPixels = pixelsPerPercent * itemPercent;

		if (itemPixels > max) {
			outputSizes[r] = max;
			remainingSpace -= max;
			totalPercent -= itemPercent;
            // NOTE: we really only have to go back to the last non-infinite maximum
			r = 0;
		}
	}
		
	// at this point, all remaining variable-sized items fall within their max and min.  (it's
    // also possible that all variable-sized items have been resolved to their max or min,
    // indicating overflow or underflow)
	pixelsPerPercent = Math.max(0, remainingSpace / totalPercent);
	for (var r = 0; r < inputSizes.length; r++) {
		size = outputSizes[r];
		if (isc.isA.Number(size)) continue;
		
		// get the percent of the total outstanding percent that goes to this item
		var itemPercent = size[2];
		outputSizes[r] = Math.floor(itemPercent * pixelsPerPercent);
	}

	// XXX do something about "remaining" pixels ???
	// return the output sizes array
	return outputSizes;
}

});	// END isc.Canvas.addMethods()

