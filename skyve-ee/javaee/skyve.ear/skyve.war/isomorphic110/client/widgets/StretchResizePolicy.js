/*
 * Isomorphic SmartClient
 * Version v11.0p_2016-03-31 (2016-03-31)
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



_calculateStaticSize : function (sizes, resultSizes, totalSize, propertyTarget) {
    //!OBFUSCATEOK

    // count up all non-static sizes
    
    var starCount    = 0, // number of "*"-sized items
        percentCount = 0, // number of percentage-sized items
        percentTotal = 0, // total percentage sizes
        staticSize   = 0; // amount that's taken up by static images

    var layoutInfo = isc.isA.Layout(propertyTarget) && !propertyTarget.isA("ListGrid") ?
                                    propertyTarget._layoutInfo : null;

	for (var i = 0; i < sizes.length; i++) {
        var size = sizes[i];
        if (size == null || isc.is.emptyString(size)) sizes[i] = size = isc.star;

        if (isc.isA.Number(size)) {
            resultSizes[i] = size;
            
            if (layoutInfo) {
                var overflowPixels = layoutInfo[i]._adaptiveOverflow;
                if (overflowPixels > 0) size += overflowPixels;
            }
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
                    percentCount++;
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

    return {
        starCount:    starCount,
        percentCount: percentCount,
        percentTotal: percentTotal,
        staticSize:   staticSize
    };
},

isStretchResizePolicy : function (lengthPolicy) {
    if (!isc.isA.String(lengthPolicy)) return false;
    return lengthPolicy == isc.star || lengthPolicy.indexOf(this._$percent) >= 0;
},


_$percent: "%",
_$listPolicy: "listPolicy",
applyStretchResizePolicy : function (sizes, totalSize, minSize, modifyInPlace, propertyTarget) {
    //!OBFUSCATEOK
	if (!sizes) return;

    // ensure that we've got a valid minimum size
    if (minSize == null || minSize <= 0) minSize = 1;

    var resultSizes = modifyInPlace ? sizes : [],  // the calculated sizes
        logEnabled = this.logIsDebugEnabled(this._$listPolicy);

    //>DEBUG  preserve the original sizes array for logging purposes
    if (logEnabled && modifyInPlace) sizes = sizes.duplicate();
    //<DEBUG

    // calculate the static size so we know what's available for stretch resizing
    var results = this._calculateStaticSize(sizes, resultSizes, totalSize, propertyTarget),
        starCount    = results.starCount,
        percentTotal = results.percentTotal,
        staticSize   = results.staticSize;
    
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
            staticSize += starCount * minSize;
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

		for (var i = 0; i < sizes.length; i++) {
			var size = sizes[i];
			
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
},



// new size calculation approach that accounts for member minimum and maximum sizes
applyNewStretchResizePolicy : function (sizes, totalSize, commonMinSize, modifyInPlace,
                                        propertyTarget, callerMinSizes)
{
    if (!sizes) return;

    // ensure that we've got a valid common minimum size
    if (commonMinSize <= 0) commonMinSize = 1;

    var resultSizes = modifyInPlace ? sizes : [], // the calculated sizes
        logEnabled = this.logIsDebugEnabled(this._$listPolicy);

    //>DEBUG  preserve the original sizes array for logging purposes
    var logMessage;
    if (logEnabled) {
        logMessage = "stretchResize" + (propertyTarget ? " for " + propertyTarget.ID : "") +
                     " with totalSize: " + totalSize + ",  desired sizes: " + sizes;
    }
    //<DEBUG

    // calculate the static size so we know what's available for stretch resizing
    var results = this._calculateStaticSize(sizes, resultSizes, totalSize, propertyTarget),
        starCount    = results.starCount,
        percentCount = results.percentCount,
        percentTotal = results.percentTotal,
        staticSize   = results.staticSize;

    
    var minSizes = [],
        maxSizes = [];

    if (isc.isA.Layout(propertyTarget) && !propertyTarget.ignoreStretchResizeMemberSizeLimits) {
        var members, vertical;

        
        if (propertyTarget.isA("ListGrid")) {
            members  = propertyTarget.fields || [];
            vertical = false;
        } else {
            members  = propertyTarget.members,
            vertical = propertyTarget.vertical;
        }

        // set up min/max arrays for clamping stretch members
        for (var i = 0; i < members.length; i++) {
            var member = members[i];
            if (vertical) {
                minSizes[i] = member.minHeight;
                maxSizes[i] = member.maxHeight;
            } else {
                minSizes[i] = member.minWidth;
                maxSizes[i] = member.maxWidth;
            }
            // add caller constraints; callerMinSizes array may be null
            var callerMinSize = commonMinSize;
            if (callerMinSizes && callerMinSizes[i] > callerMinSize) {
                callerMinSize = callerMinSizes[i];
            }
            // merge caller constraints into the member-specific minSizes
            if (minSizes[i] == null || minSizes[i] < callerMinSize) {
                minSizes[i] = callerMinSize;
            }
        }
    }

    
    if (modifyInPlace) sizes = sizes.duplicate();
    else {
        var undef, normalizedSizes = [];
        for (var i = 0; i < sizes.length; i++) {
            normalizedSizes[normalizedSizes.length] = resultSizes[i] !== undef ?
                                                      resultSizes[i] : sizes[i];
        }
        sizes = normalizedSizes;
    }

    
    var resultFrozen = [],
        remainingSpace = 0;

    while (starCount + percentCount > 0) {

        var sumOfViolations = 0,
            memberViolation = [];

        var percentMemberPercent = percentTotal;

        // - "stars" are translated to percents, sharing all remaining percent points (of 100)
        //   not allocated to specified percent sizes
        // - stars and percents share all space not allocated to static numbers
        // - if there are any percents or stars, all space is always filled

        var starPercent = 0,
            starStaticSize = 0;
        if (starCount) {
            if (percentMemberPercent >= 100) {
                // percents sum over 100, so star-sized items receive 0% of remaining space,
                // hence will be sized to the minimum size minimum.  Accumulate the minimum size
                // into starStaticSize to prevent overflow when percents sum to exactly 100 and
                // there is also a "*", since this might not be expected.
                for (var i = 0; i < sizes.length; i++) {
                    if (!resultFrozen[i] && sizes[i] == isc.star) {
                        starStaticSize += minSizes[i] != null ? minSizes[i] : commonMinSize;
                    }
                }
            } else {
                // star sized items share the remaining percent size
                starPercent = (100 - percentMemberPercent) / starCount;
                percentMemberPercent = 100;
            }
        }

        if (percentMemberPercent > 0) {
            // track remaining space for use after the last iteration
            remainingSpace = totalSize - staticSize - starStaticSize;

            // translate all "*" / "%" sized items to pixel sizes
            var pixelsPerPercent = Math.max(0, remainingSpace / percentMemberPercent);

            for (var i = 0; i < sizes.length; i++) {
                if (resultFrozen[i]) continue;

                var size = sizes[i];
            
                if (isc.isA.String(size)) {
                    var stretchSize;
                    if (size == isc.star) {
                        stretchSize = starPercent * pixelsPerPercent;
                    } else if (size.indexOf(this._$percent) >= 0) {
                        
                        stretchSize = parseInt(size) * pixelsPerPercent;
                    } else {
                        // the remaining case - a non-star non-percent string - was translated
                        // to a static size in the first pass (which will be zero if we didn't
                        // understand it)
                        continue;
                    }
                    stretchSize = Math.floor(stretchSize);

                    
                    var minSize = minSizes[i] != null ? minSizes[i] : commonMinSize;
                    if (stretchSize < minSize) {
                        sumOfViolations += memberViolation[i] = minSize - stretchSize;
                        stretchSize = minSize;
                    }
                    
                    var maxSize = Math.max(maxSizes[i], minSize);
                    if (maxSize != null && stretchSize > maxSize) {
                        sumOfViolations += memberViolation[i] = maxSize - stretchSize;
                        stretchSize = maxSize;
                    }

                    // deduct clamped size from remaining space
                    remainingSpace -= resultSizes[i] = stretchSize;
                }
            }
        }

        
        if (sumOfViolations != 0) {
            for (var i = 0; i < sizes.length; i++) {
                // nothing to do for members with no violation
                if (memberViolation[i] == null) continue;
                // nothing to do for max (min) violation is sum is positive (negative)
                if (sumOfViolations > 0 && memberViolation[i] <= 0 ||
                    sumOfViolations < 0 && memberViolation[i] >= 0) continue;

                // update accounting to reflect member is frozen
                var size = sizes[i];
                if (size == isc.star) {
                    starCount--;
                } else {
                    percentCount--;
                    percentTotal -= parseInt(size);
                }
                staticSize += resultSizes[i];
                resultFrozen[i] = true;
            }
        } else break;
    }

    
    if (remainingSpace > 0) {
        for (var i = sizes.length - 1; i >= 0; i--) {
            // only stretch members are eligible to receive it
            if (!this.isStretchResizePolicy(sizes[i])) continue;
            // if the remaining space doesn't break a maxWidth/maxHeight, we're done
            if (maxSizes[i] == null || maxSizes[i] >= resultSizes[i] + remainingSpace) {
                resultSizes[i] += remainingSpace;
                break;
            }
        }
        if (i < 0) {
            this.logWarn("stretchResize" + (propertyTarget ? " for " + propertyTarget.ID : "") +
                         " with totalSize: " + totalSize + " and calculated sizes: " + 
                         resultSizes + "; cannot assign remainingSpace: " + remainingSpace + 
                         " due to member max size limits", "listPolicy");
        }
    }

    //>DEBUG
    if (logEnabled) {
        this.logDebug(logMessage + ",  calculated sizes: " + resultSizes, "listPolicy");
    }
    //<DEBUG

    // return the sizes array
    return resultSizes;
}

}); // END isc.Canvas.addMethods()
