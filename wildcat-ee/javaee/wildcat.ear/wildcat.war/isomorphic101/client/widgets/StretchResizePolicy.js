/*
 * Isomorphic SmartClient
 * Version SNAPSHOT_v10.1p_2015-12-10 (2015-12-10)
 * Copyright(c) 1998 and beyond Isomorphic Software, Inc. All rights reserved.
 * "SmartClient" is a trademark of Isomorphic Software, Inc.
 *
 * licensing@smartclient.com
 *
 * http://smartclient.com/license
 */
isc.Canvas.addClassMethods({

//>	@classMethod	Canvas.applyStretchResizePolicy()	(A)
// Add a sizing policy to a series of size specifications, either a number in pixels, a % or a
// "*".
//
// Return value is an array of sizes for each object.  No object receives less than 1 pixel
//<




_$percent : "%",
_$listPolicy : "listPolicy",
applyStretchResizePolicy : function (sizes, totalSize, minSize, modifyInPlace, propertyTarget) {
    //!OBFUSCATEOK
	if (!sizes) return;
	
	var percentTotal = 0,	// total percentage sizes
		starCount = 0,		// number of "*"-sized items
		staticSize = 0,		// amount that's taken up by static images
		size = 0,			// temp variable to hold the size
        resultSizes = (modifyInPlace ? sizes : []),  // the calculated sizes
        logEnabled = this.logIsDebugEnabled(this._$listPolicy),
        minSize = (minSize || 1);

    //>DEBUG  preserve the original sizes array for logging purposes
    if (logEnabled && modifyInPlace) sizes = sizes.duplicate();
    //<DEBUG

	// count up all non-static sizes
    
	for (var i = 0; i < sizes.length; i++) {
        size = sizes[i];
        if (size == null || isc.is.emptyString(size)) sizes[i] = size = isc.star;

        if (isc.isA.Number(size)) {
            resultSizes[i] = size;
        } else {
			if (size == isc.star) {
				// a stretch item -- increment the number of stretch items
				starCount++;
				size = 0;
			} else if (size.indexOf(this._$percent) >= 0) {
                if (propertyTarget != null && propertyTarget.fixedPercents) {
                    // treat percents as a fixed percentage of the available space
                    var percentage = parseInt(size);
                    size = resultSizes[i] = Math.round((percentage/100)*totalSize);
                } else {
    				// percentage -- add it to the percentage total
	    			percentTotal += parseInt(size);
		    		size = 0;
                }
			} else {
                // look for a numeric property on the propertyTarget, if passed
                if (propertyTarget != null && isc.isA.Number(propertyTarget[size])) {
                    size = resultSizes[i] = propertyTarget[size];
                } else if (propertyTarget != null && size === "otherScrollbarSize") {
                    size = resultSizes[i] = propertyTarget.getOtherScrollbarSize();
                } else {
    			    // handle a number specified as a string, possibly with extra junk
                    
                    var parsedSize = parseInt(size);
                    if (isc.isA.Number(parsedSize) && parsedSize >= 0) {
                        resultSizes[i] = size = parsedSize;
                    } else {
                        // try doing an eval
                        
                        try {
                            size = isc.eval(size);
                        } catch (e) {
                            var logTarget = propertyTarget && propertyTarget.logWarn 
                                            ? propertyTarget : this;
                                            
                            logTarget.logWarn("StretchResizePolicy: " +
                                " unable to convert size:" + size +
                                " to a valid size - reported error: '"+ e + 
                                "'\n Complete set of sizes:"+ sizes);
                            size = null;
                        }
                        // and if it didn't come out to a non-negative number, set it to 0
                        if (!isc.isA.Number(size) || size < 0) size = 0;
                        resultSizes[i] = size;
                    }
                }
            }
        }

		// make sure everything is zero or more pixels in size
		size = Math.max(size,0);
		
		// add the size to the staticSize
		staticSize += size;
	}

    // - "stars" are translated to percents, sharing all remaining percent points (of 100)
    //   not allocated to specified percent sizes
    // - stars and percents share all space not allocated to static numbers
    // - if there are any percents or stars, all space is always filled

    var starPercent = 0;
	if (starCount) {
        if (percentTotal >= 100) {
            // percents sum over 100, so star-sized items receive 0% of remaining space, hence
            // will be sized to the minimum size minimum.  Add the minimum size to staticSize
            // to prevent overflow when percents sum to exactly 100 and there is also a "*",
            // since this might not be expected.
            staticSize += (starCount * minSize);
        } else {
            // star sized items share the remaining percent size
            starPercent = (100 - percentTotal) / starCount;
		    percentTotal = 100;
        }
	}

	if (percentTotal > 0) {
		// translate all "*" / "%" sized items to pixel sizes

        var remainingSpace = totalSize - staticSize,
			pixelsPerPercent = Math.max(0, remainingSpace / percentTotal),
			lastStretch = null;

		for (i = 0; i < sizes.length; i++) {
			size = sizes[i];
			
			if (isc.isA.String(size)) {
                var stretchSize;
				if (size == isc.star) {
					stretchSize = starPercent * pixelsPerPercent;
				} else if (size.indexOf(this._$percent) >= 0) {
                    
                    stretchSize = parseInt(size) * pixelsPerPercent;
				} else {
                    // the remaining case - a non-star non-percent string - was translated to
                    // a static size in the first pass (which will be zero if we didn't
                    // understand it)
                    continue;
                }
                stretchSize = Math.max(Math.floor(stretchSize), minSize);
                remainingSpace -= stretchSize;
                // NOTE: remember the last variable-sized item for later (to add leftover pixels)
				lastStretch = i;
                resultSizes[i] = stretchSize;
			}
		}
		// add any remaining pixels to the last stretch item
        if (remainingSpace > 0) resultSizes[lastStretch] += remainingSpace;
	}

    //>DEBUG
    if (logEnabled) {
        this.logDebug("stretchResize" + (propertyTarget ? " for " + propertyTarget.ID : "") +
                     " with totalSize: " + totalSize + 
                     ",  desired sizes: " + sizes +
                     ",  calculated sizes: " + resultSizes, "listPolicy");
    }
	//<DEBUG

	// return the sizes array
	return resultSizes;
}

});	// END isc.Canvas.addMethods()

