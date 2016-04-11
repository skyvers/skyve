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
//------------------------------------------------------------------------------------------
//  detect native drawing capabilities by browser version
//------------------------------------------------------------------------------------------
isc.Browser.hasCANVAS = isc.Browser.geckoVersion >= 20051107 || isc.Browser.safariVersion >= 181 ||
                        isc.Browser.isIE9 || isc.Browser.isEdge || 
                        (!isc.Browser.isIE && typeof(document.createElement("canvas").getContext) === "function");
isc.Browser.hasSVG = (window.SVGSVGElement != null &&
                      (!isc.Browser.isMoz || isc.Browser.version >= 4));

isc.Browser.hasVML = isc.Browser.isIE && isc.Browser.version >= 5;
isc.Browser.defaultDrawingType = 
                              //isc.Browser.hasSVG ? "svg" :
                              isc.Browser.hasCANVAS ? "bitmap" :
                              isc.Browser.hasVML ? "vml" :
                              "none";

isc._$xlinkNS = "http://www.w3.org/1999/xlink";
isc._$svgNS = "http://www.w3.org/2000/svg";


//------------------------------------------------------------------------------------------

isc.ClassFactory.defineClass("VMLRenderer");

isc.VMLRenderer.addClassProperties({

    _$solid: "solid",
    _$strokedFalse: " stroked='f'",
    _$filledFalse: " filled='f'",
    _$butt: "butt",
    _$flat: "flat",
    _$none: "none",

    _$STROKE: "STROKE",
    _$FILL: "FILL",
    _$SHADOW: "SHADOW",
    _$SKEW: "SKEW",
    _$TEXTPATH: "TEXTPATH",
    _$PATH: "PATH",

    
    _dashstyleRegExp: /^(solid|dot|dash|dashdot|shortdot|(short|long)dash(dot){0,2}|\d+( +\d+)*)$/i,

    _joinstyleRegExp: /^(round|bevel|miter)$/i,
    _endcapRegExp: /^(flat|square|round)$/i,
    _arrowRegExp: /^(none|block|classic|diamond|oval|open|chevron|doublechevron)$/i,

    _fuchsiaRegExp: /#?ff00ff|fuchsia/i,

    _fontStyleRegExp: /^(normal|italic|oblique)$/i,
    _textAlignRegExp: /^(left|right|center|justify)$/i,

    _quote : function (quote, str) {
        
        if (str.indexOf("\\") != -1) {
            str = str.replace("\\", "\\\\");
        }
        if (str.indexOf(quote) != -1) {
            str = str.replace(quote, "\\" + quote);
        }
        return quote + str + quote;
    },

    _singleQuoteRegExp: /\\([\\'])/g,
    _doubleQuoteRegExp: /\\([\\"])/g,
    _quoteReplace : function (match, c) {
        return c;
    },
    // Note that `str` is expected not to include the initial and final quote characters.
    _unquote : function (quote, str) {
        
        var r = (quote == "'" ?
                    isc.VMLRenderer._singleQuoteRegExp : isc.VMLRenderer._doubleQuoteRegExp);
        r.lastIndex = 0;
        return (r.test(str) ? str.replace(r, isc.VMLRenderer._quoteReplace) : str);
    },

    
    _headFontFamilyRegExp: /(?:\s*(?:(["'])((?:(?!\1)[^\\]|\\.)*)\1|([^,]*)\1)\s*)/g,
    _tailFontFamilyRegExp: /(?:,\s*(?:(["'])((?:(?!\1)[^\\]|\\.)*)\1|([^,]*)\1)\s*)/g,

    _genericFontFamiliesRegExp: /^((sans-)?serif|monospace|cursive|fantasy)$/i,

    // A constructor function to initialize the properties in the `config` objects passed to
    // a VMLRenderer's methods:
    shapeConfig : function () {
        // Numbers:
        this.left = this.top = this.width = this.height = 0;
        this.arcsize = this.startangle = this.endangle = 0;
        this.fromX = this.fromY = this.toX = this.toY = 0;
        this.control1X = this.control1Y = this.control2X = this.control2Y = 0;
        this.lineWidth = 0;
        this.lineOpacity = this.fillOpacity = 0;
        this.zIndex = null;

        // Strings:
        this.alt = null;
        this.src = null;
        this.lineColor = this.linePattern = this.startArrow = this.endArrow = this.lineCap = null;
        this.fillColor = null;
        this.textAlign = this.fontFamily = this.fontSize = this.fontWeight = this.fontStyle = null;
        this.visibility = null;
        this.contents = null;

        // Arrays:
        this.points = null;
        this._tempBoundingBox = null;

        // Booleans:
        this.stroked = this.filled = false;
        this.closePolyline = false;
        this.useMatrixFilter = false;

        // AffineTransforms:
        this.transform = null;

        // Functions:
        this.getBoundingBox = null;
        this._normalizeLinearGradient = null;
        this._getPathVML = null;

        // Objects:
        this.fillGradient = null;
        this.shadow = null;
    },
    shapeConfigCommonPropertiesList: [
        "left", "top", "width", "height", "lineWidth", "lineOpacity", "fillOpacity", "zIndex",
        "src", "lineColor", "linePattern", "startArrow", "endArrow", "lineCap", "fillColor",
        "fontFamily", "fontWeight", "fontStyle", "contents", "points", "_tempBoundingBox",
        "fillGradient", "shadow"],
    // There are currently 46 properties. This needs to be less than or equal to 53 in order
    // for bit masking to work.
    shapeConfigAllPropertiesList: [
        "left", "top", "width", "height", "arcsize", "startangle", "endangle", "fromX",
        "fromY", "toX", "toY", "control1X", "control1Y", "control2X", "control2Y",
        "lineWidth", "lineOpacity", "fillOpacity", "zIndex", "alt", "src", "lineColor", "linePattern",
        "startArrow", "endArrow", "lineCap", "fillColor", "textAlign", "fontFamily",
        "fontSize", "fontWeight", "fontStyle", "contents", "points", "_tempBoundingBox",
        "stroked", "filled", "closePolyline", "useMatrixFilter", "transform",
        "getBoundingBox", "_normalizeLinearGradient", "_getPathVML", "fillGradient", "shadow",
        "visibility"],
    drawItemToShapeConfig : function (drawItem, shapeConfig) {
        isc.getProperties(drawItem, isc.VMLRenderer.shapeConfigCommonPropertiesList, shapeConfig);
        if (drawItem.lineCap == isc.VMLRenderer._$butt) {
            shapeConfig.lineCap = isc.VMLRenderer._$flat;
        }
        if (!drawItem.startArrow) {
            shapeConfig.startArrow = isc.VMLRenderer._$none;
        }
        if (!drawItem.endArrow) {
            shapeConfig.endArrow = isc.VMLRenderer._$none;
        }
        shapeConfig.stroked = drawItem._hasStroke();
        if (!drawItem.vmlLineEventsOnly) {
            shapeConfig.filled = true;
            shapeConfig.fillOpacity = 0;
        } else {
            shapeConfig.filled = drawItem._hasFill();
        }
        shapeConfig.transform = drawItem._getLocalTransform();
        shapeConfig._drawItem = drawItem;
        shapeConfig.getBoundingBox = function (includeStroke, outputBox) {
            var drawItem = this._drawItem;
            return drawItem.getBoundingBox.apply(drawItem, arguments);
        };
        shapeConfig._normalizeLinearGradient = function (def, boundingBox) {
            var drawItem = this._drawItem;
            return drawItem._normalizeLinearGradient.apply(drawItem, arguments);
        };
        shapeConfig._getPathVML = function (path) {
            var drawItem = this._drawItem;
            return drawItem._getPathVML.apply(drawItem, arguments);
        };
        shapeConfig.visibility = (drawItem.hidden ? "hidden" : "visible");
        shapeConfig.zIndex = drawItem.getZIndex(true);
    },

    // Various adapters to convert a VML predefined shape (e.g. <rect/>) into a <shape/>:
    _roundrectGetPathVML : function (path) {
        var left = this.left,
            width = this.width,
            right = left + width,
            top = this.top,
            height = this.height,
            bottom = top + height,
            arcsize = this.arcsize;

        if (arcsize == 0) {
            path.m(left, top);
            path.l(right, top);
            path.l(right, bottom);
            path.l(left, bottom);
            path.x();
        } else {
            var radius = arcsize * Math.min(width, height);
            path.al(left + radius, top + radius, radius, radius, 90, 90);
            path.l(left, bottom - radius);
            path.ae(left + radius, bottom - radius, radius, radius, 180, 90);
            path.l(right - radius, bottom);
            path.ae(right - radius, bottom - radius, radius, radius, 270, 90);
            path.l(right, top + radius);
            path.ae(right - radius, top + radius, radius, radius, 0, 90);
            path.x();
        }
    },
    _lineGetPathVML : function (path) {
        path.m(this.fromX, this.fromY);
        path.l(this.toX, this.toY);
    },
    _polylineGetPathVML : function (path) {
        
        var points = this.points;
        path.m(points[0][0], points[0][1]);
        path.ls(points, 1, points.length);
        if (this.closePolyline) {
            path.l(points[0][0], points[0][1]);
        }
    },
    _curveGetPathVML : function (path) {
        path.m(this.fromX, this.fromY);
        path.c(
            this.control1X, this.control1Y, this.control2X, this.control2Y,
            this.toX, this.toY);
    },
    _arcGetPathVML : function (path) {
        var left = this.left,
            width = this.width,
            right = this.left + this.width,
            top = this.top,
            height = this.height,
            bottom = this.top + this.height;
        path.al(
            (left + right) / 2, (top + bottom) / 2, width / 2, height / 2,
            this.startangle, this.endangle);
    },
    // Defines the path to use when rendering text using a <textpath/>:
    _textGetPathVML : function (path) {
        var boundingBox = this.getBoundingBox(false, this._tempBoundingBox),
            // VML identifies the path with the vertical middle of the text.
            x0 = boundingBox[0], y0 = (boundingBox[1] + boundingBox[3]) / 2,
            x1 = boundingBox[2], y1 = y0;
        path.m(x0, y0);
        path.l(x1, y1);
        path.e();
    },

    _vmlPathBuilder: {
        _vmlRenderer: null,
        _scale: 1,

        _reset : function (vmlRenderer, dx, dy) {
            
            this._vmlRenderer = vmlRenderer;

            
            this._scale = this._vmlRenderer.scale;

            
            this._dx = dx;
            this._dy = dy;

            return this;
        },

        m : function (x, y, relative) {
            
            var scale = this._scale;
            this._vmlRenderer.buffer.append(
                (relative ? "t" : "m"),
                Math.round(scale * (x + this._dx)), ",", Math.round(scale * (y + this._dy)));
        },
        l : function (x, y, relative) {
            
            var scale = this._scale;
            this._vmlRenderer.buffer.append(
                (relative ? "r" : "l"),
                Math.round(scale * (x + this._dx)), ",", Math.round(scale * (y + this._dy)));
        },
        ls : function (points, start, end, relative) {
            
            var scale = this._scale,
                dx = this._dx, dy = this._dy,
                buffer = this._vmlRenderer.buffer,
                point = points[start];
            buffer.append(
                (relative ? "r" : "l"),
                Math.round(scale * (point[0] + dx)), ",", Math.round(scale * (point[1] + dy)));
            for (var i = start + 1; i < end; ++i) {
                var point = points[i];
                buffer.append(
                    ",", Math.round(scale * (point[0] + dx)), ",",
                    Math.round(scale * (point[1] + dy)));
            }
        },
        c : function (cp1x, cp1y, cp2x, cp2y, p2x, p2y, relative) {
            
            var scale = this._scale,
                dx = this._dx, dy = this._dy,
                command = (relative ? "v" : "c");
            this._vmlRenderer.buffer.append(
                command,
                Math.round(scale * (cp1x + dx)), ",", Math.round(scale * (cp1y + dy)), ",",
                Math.round(scale * (cp2x + dx)), ",", Math.round(scale * (cp2y + dy)), ",",
                Math.round(scale * (p2x + dx)), ",", Math.round(scale * (p2y + dy)));
        },
        cs : function (points, start, end, relative) {
            
            var scale = this._scale,
                dx = this._dx, dy = this._dy,
                buffer = this._vmlRenderer.buffer,
                cp1 = points[start], cp2 = points[start + 1], p2 = points[start + 2],
                command = (relative ? "v" : "c");
            buffer.append(
                command,
                Math.round(scale * (cp1[0] + dx)), ",", Math.round(scale * (cp1[1] + dy)), ",",
                Math.round(scale * (cp2[0] + dx)), ",", Math.round(scale * (cp2[1] + dy)), ",",
                Math.round(scale * (p2[0] + dx)), ",", Math.round(scale * (p2[1] + dy)));
            for (var i = start + 3; i < end; i += 3) {
                var cp1 = points[i], cp2 = points[i + 1], p2 = points[i + 2];
                buffer.append(
                    ",", Math.round(scale * (cp1[0] + dx)), ",", Math.round(scale * (cp1[1] + dy)), ",",
                    Math.round(scale * (cp2[0] + dx)), ",", Math.round(scale * (cp2[1] + dy)), ",",
                    Math.round(scale * (p2[0] + dx)), ",", Math.round(scale * (p2[1] + dy)));
            }
        },
        x : function () { this._vmlRenderer.buffer.append("x"); },
        
        ae : function (centerX, centerY, xRadius, yRadius, startAngle, diffAngle) {
            
            var scale = this._scale,
                dx = this._dx, dy = this._dy,
                precision = this._vmlRenderer.precision,
                buffer = this._vmlRenderer.buffer;
            buffer.append(
                "ae",
                Math.round(scale * (centerX + dx)), ",",
                Math.round(scale * (centerY + dy)), ",",
                Math.round(scale * xRadius), ",", Math.round(scale * yRadius), ",",
                
                Math.round(65536 * startAngle), ",", Math.round(65536 * diffAngle));
        },
        aes : function (args, start, end) {
            
            var scale = this._scale,
                dx = this._dx, dy = this._dy,
                precision = this._vmlRenderer.precision,
                buffer = this._vmlRenderer.buffer,
                centerPoint = args[start],
                r = args[start + 1],
                startAngle = args[start + 2],
                diffAngle = args[start + 3] - startAngle,
                roundedR = Math.round(scale * r);
            buffer.append(
                "ae",
                Math.round(scale * (centerPoint[0] + dx)), ",",
                Math.round(scale * (centerPoint[1] + dy)), ",",
                roundedR, ",", roundedR, ",", Math.round(65536 * startAngle), ",",
                Math.round(65536 * diffAngle));
            for (var i = start + 4; i < end; i += 4) {
                var centerPoint = args[i],
                    r = args[i + 1],
                    startAngle = args[i + 2],
                    diffAngle = args[i + 3] - startAngle;
                buffer.append(
                    ",", Math.round(scale * (centerPoint[0] + dx)), ",",
                    Math.round(scale * (centerPoint[1] + dy)), ",", roundedR, ",", roundedR,
                    ",", Math.round(65536 * startAngle), ",", Math.round(65536 * diffAngle));
            }
        },
        al : function (centerX, centerY, xRadius, yRadius, startAngle, diffAngle) {
            
            var scale = this._scale,
                dx = this._dx, dy = this._dy,
                precision = this._vmlRenderer.precision,
                buffer = this._vmlRenderer.buffer;
            buffer.append(
                "al",
                Math.round(scale * (centerX + dx)), ",",
                Math.round(scale * (centerY + dy)), ",",
                Math.round(scale * xRadius), ",", Math.round(scale * yRadius), ",",
                Math.round(65536 * startAngle), ",", Math.round(65536 * diffAngle));
        },
        e : function () { this._vmlRenderer.buffer.append("e"); },
        t : function (x, y) { this.m(x, y, true); },
        r : function (x, y) { this.l(x, y, true); },
        rs : function (points, start, end) { this.ls(points, start, end, true); },
        v : function (cp1x, cp1y, cp2x, cp2y, p2x, p2y) {
            this.c(cp1x, cp1y, cp2x, cp2y, p2x, p2y, true);
        },
        vs : function (points, start, end) { this.cs(points, start, end, true); },
        nf : function () { this._vmlRenderer.buffer.append("nf"); },
        ns : function () { this._vmlRenderer.buffer.append("ns"); }
        
    },

    
    _fitBlockCoordinateSystemOutput: {
        left: 0, top: 0, width: 0, height: 0,
        coordorigin0: 0, coordorigin1: 0, coordsize0: 0, coordsize1: 0
    },
    _fitBlockCoordinateSystem : function (sx, sy, dx, dy) {
        
        var output = isc.VMLRenderer._fitBlockCoordinateSystemOutput;

        
        var minInt = -21474836, maxInt = 21474836;

        isc.VMLRenderer._fitBlockCoordinateSystemHelper(sy, dy, minInt, maxInt, output);
        output.top = output.left;
        output.height = output.width;
        output.coordorigin1 = output.coordorigin0;
        output.coordsize1 = output.coordsize0;

        isc.VMLRenderer._fitBlockCoordinateSystemHelper(sx, dx, minInt, maxInt, output);

        return output;
    },

    
    _fitBlockCoordinateSystemHelper : function (sx, dx, minInt, maxInt, output) {
        

        if (sx == 0) {
            output.left = Math.round(dx);
            output.width = 0;
            output.coordorigin0 = 0;
            output.coordsize0 = 1;
            return;
        }

        // Method used to save the best approximations:
        var state = isc.VMLRenderer._checkBlookCoordinateSystemState,
            check = isc.VMLRenderer._checkBlookCoordinateSystem;
        state.alpha = sx;
        state.beta = -dx;
        state.minP = state.minQ = minInt;
        state.maxP = state.maxQ = maxInt;
        state.count = 0;

        var maxN = 30;
        if (dx == 0) {
            var alpha = sx;

            // First handle the case where `alpha` is a half-integer.
            if (alpha != Math.floor(alpha) && 2 * alpha == Math.floor(2 * alpha)) {
                var p = 0, q = 0;
                if (2 * alpha < maxInt) {
                    p = 2 * alpha;
                    q = 2;
                } else if (-2 * alpha > minInt) {
                    p = -2 * alpha;
                    q = -2;
                } else if (Math.floor(alpha) < maxInt) {
                    p = Math.floor(alpha);
                    q = 1;
                } else {
                    p = -Math.floor(alpha);
                    q = -1;
                }
                check(state, p, q);
                check(state, 0, 0);
            } else {
                var etanm2 = alpha, etanm1 = -1,
                    pnm2 = 0, pnm1 = 1,
                    qnm2 = 1, qnm1 = 0,
                    p = 0, q = 0,
                    sign = (-minInt > maxInt ? -1 : 1),
                    maxQ = Math.min(-minInt, maxInt);
                check(state, 0, 0);
                for (var n = 0; n < maxN && q <= maxQ && etanm1 != 0; ++n) {
                    var an = Math.floor(-etanm2 / etanm1),
                        etan = etanm2 + an * etanm1;
                    p = pnm2 + an * pnm1;
                    q = qnm2 + an * qnm1;
                    check(state, sign * p, sign * q);
                    pnm2 = pnm1; pnm1 = p;
                    qnm2 = qnm1; qnm1 = q;
                    etanm2 = etanm1; etanm1 = etan;
                }
            }
        } else {
            outer: for (var j = -1; j <= 1; j += 2) { // `j` is +/-1.
                
                var minP = minInt, maxP = maxInt,
                    minQ = 0, maxQ = maxInt;
                if (j == 1) {
                    minP = -maxInt;
                    maxP = maxQ = -minInt;
                }

                var alpha = sx, beta = -(j * dx),
                    n = 0,
                    etanm2 = alpha, etanm1 = -1,
                    pnm2 = 0, pnm1 = 1,
                    qnm2 = 1, qnm1 = 0,
                    prevQ = 0,
                    p = 0, q = 0,
                    sigma = -beta,
                    prevEqAn = false;

                for ( ; n < maxN && q <= maxQ && etanm1 != 0 && sigma != 0; ++n) {
                    var a = Math.floor(-etanm2 / etanm1),
                        pn = pnm2 + a * pnm1,
                        qn = qnm2 + a * qnm1,
                        etan = etanm2 + a * etanm1;

                    prevQ = q;
                    if (q <= qnm1) {
                        
                        var b = Math.floor(-(sigma + etanm2) / etanm1);
                        p += pnm2 + b * pnm1;
                        q += qnm2 + b * qnm1;
                        sigma += etanm2 + b * etanm1;
                        prevEqAn = true;
                        if (check(state, j * p, j * q)) break outer;
                    } else {
                        p -= pnm1;
                        q -= qnm1;
                        sigma -= etanm1;
                        prevEqAn = false
                    }
                    pnm2 = pnm1; pnm1 = pn;
                    qnm2 = qnm1; qnm1 = qn;
                    etanm2 = etanm1; etanm1 = etan;
                }

                if (prevEqAn && sigma != 0) {
                    
                    if (check(state, j * (p + pnm2), j * (q + qnm2))) break outer;
                }
                if (etanm1 != 0 && !(n < maxN && sigma != 0)) {
                    
                    var eta = etanm1;
                    for (var q = prevQ + 1; n < maxN && q <= maxQ && (eta != 0 || sigma != 0); ++n, ++q) {
                        var qalpha = q * alpha,
                            r = Math.round(qalpha),
                            qalphambeta = qalpha - beta,
                            p = Math.round(qalphambeta);
                        eta = qalpha - r;
                        sigma = qalphambeta - p;
                        if (check(state, j * p, j * q)) break outer;
                    }
                }
            }

            

            
            check(state, 0, Math.round(-dx / sx));
        }

        output.left = state.bestLeft;
        output.width = state.bestWidth;
        output.coordorigin0 = state.bestCoordorigin0;
        output.coordsize0 = state.bestCoordsize0;
    },
    _checkBlookCoordinateSystemState: {
        // Problem statement
        alpha: 0, beta: 0, minP: 0, maxP: 0, minQ: 0, maxQ: 0,

        // Storage for best individual solutions
        count: 0,
        bestEpsilon: 0, secondBestEpsilon: 0,
        bestP: 0, secondBestP: 0,
        bestQ: 0, secondBestQ: 0,

        // Storage for best fitting combination of solutions
        bestDelta: 0,
        bestLeft: 0, bestWidth: 0, bestCoordorigin0: 0, bestCoordsize0: 0
    },
    _checkBlookCoordinateSystem : function (state, p, q) {
        // Retain the best two approximations.
        if (state.minP <= p && p <= state.maxP && state.minQ <= q && q <= state.maxQ) {
            var epsilon = Math.abs(q * state.alpha - p - state.beta),
                foundBest = (
                    state.count == 0 ||
                    (!(p == state.bestP && q == state.bestQ) &&
                     (state.count == 1 || !(p == state.secondBestP && q == state.secondBestQ)) &&
                     epsilon <= state.bestEpsilon)),
                foundSecondBest = (
                    !foundBest &&
                    !(p == state.bestP && q == state.bestQ) &&
                    (state.count == 1  ||
                        (!(p == state.secondBestP && q == state.secondBestQ) &&
                         epsilon <= state.secondBestEpsilon)));

            if (foundBest || foundSecondBest) {
                for (var i = 0; i < state.count; ++i) {
                    var left = p,
                        width = (i == 0 ? state.bestP : state.secondBestP) - p,
                        coordorigin0 = q,
                        coordsize0 = (i == 0 ? state.bestQ : state.secondBestQ) - q;

                    
                    if (coordsize0 < 0) {
                        left += width;
                        coordorigin0 += coordsize0;
                        width = -width;
                        coordsize0 = -coordsize0;
                    }
                    var delta = 0;
                    if (coordsize0 != 0) {
                        delta = Math.abs(width / coordsize0 - state.alpha);
                    }
                    if (state.count < 2 ||
                        state.bestCoordsize0 == 0 ||
                        (coordsize0 != 0 && delta < state.bestDelta))
                    {
                        state.bestDelta = delta;
                        state.bestLeft = left;
                        state.bestWidth = width;
                        state.bestCoordsize0 = coordsize0;
                        state.bestCoordorigin0 = coordorigin0;
                    }
                }

                ++state.count;
                if (foundBest) {
                    state.secondBestEpsilon = state.bestEpsilon;
                    state.secondBestP = state.bestP;
                    state.secondBestQ = state.bestQ;
                    state.bestEpsilon = epsilon;
                    state.bestP = p;
                    state.bestQ = q;
                } else { // foundSecondBest
                    state.secondBestEpsilon = epsilon;
                    state.secondBestP = p;
                    state.secondBestQ = q;
                }
            }
        }
        
        
        return (state.count >= 2 && state.bestDelta < 1e-6 && state.bestWidth > 1);
    }
});

isc.VMLRenderer.addProperties({
    addPropertiesOnCreate: false,
    init : function (drawPane, scale, precision) {
        // The DrawPane is used to look up fill gradients and for its startTagVML(),
        // endTagVML(), and measureLabel() methods.
        this.drawPane = drawPane;
        this.startTagVML = drawPane.startTagVML;
        this.endTagVML = drawPane.endTagVML;
        var globalShapeTypeId = this._globalShapeTypeId = "#" + drawPane.getID() + "_global_rotation";
        this._globalShapeTypeRef = " TYPE='" + globalShapeTypeId + "'";
        var globalTextShapeTypeId = this._globalTextShapeTypeId = "#" + drawPane.getID() + "_global_rotation_zoom";
        this._globalTextShapeTypeRef = " TYPE='" + globalTextShapeTypeId + "'";

        // `buffer` is the StringBuffer to which to write out the VML.
        this.buffer = null;

        
        this.scale = scale;
        this.precision = Math.ceil(precision);

        // Number of decimal digits written out in the rotation CSS of an <image/>:
        this.rotationPrecision = 6;
        // Number of decimal digits written out in the entries of a Matrix filter:
        this.matrixFilterPrecision = 6;

        // Number of decimal digits written out in the entries of the offset attributes of a
        // <skew/> subelement and a <shadow/> subelement:
        this.skewOffsetPrecision = 6;
        this.shadowOffsetPrecision = 0;

        // Number of decimal digits written out to the <fill/> subelements' `angle` attributes:
        this.fillAnglePrecision = 3;

        this._inInit = true;
        this.updatedGlobalTransform();
        delete this._inInit;

        
    },

    _isTranslation : function (transform) {
        
        
        return (
            transform == null || (
                transform.m00 == 1 && transform.m01 == 0 &&
                transform.m10 == 0 && transform.m11 == 1));
    },

    
    _usePredefinedShape : function (transform) {
        return (this._globalRotation == 0 && this._isTranslation(transform));
    },

    
    _referenceGlobalShapeType : function (isText, isImage, transform) {
        return (
            
            !isImage &&
            
            (isText || this._globalRotation != 0) &&
            
            this._isTranslation(transform));
    },

    updatedGlobalTransform : function () {
        var zoomLevel = this.drawPane.zoomLevel || 0,
            rotation = this.drawPane.rotation || 0,
            viewPortWidth = this.drawPane._viewPortWidth || 0,
            viewPortHeight = this.drawPane._viewPortHeight || 0,
            translate0 = 0, translate1 = 0,
            undef;

        
        var globalRotationWasZero = (this._globalRotation == 0);
        this._globalRotation = rotation;

        // These variables are used in _getSkewMatrixAndOffset().
        if (isc.isAn.Array(this.drawPane.translate)) {
            translate0 = this.drawPane.translate[0] || 0;
            translate1 = this.drawPane.translate[1] || 0;
        }
        var zoomLevelChanged = (this._zoomLevel != zoomLevel);
        this._zoomLevel = zoomLevel;
        this._translate0 = translate0;
        this._translate1 = translate1;
        var theta = rotation * Math.PI / 180;
        this._sin = Math.sin(theta);
        this._cos = Math.cos(theta);
        var s = Math.sin(2 * theta),
            c = Math.cos(2 * theta);
        this._alpha = (c + 1) / 2;
        this._beta = s / 2;
        this._gamma = (c - 1) / 2;
        this._viewPortWidth = viewPortWidth;
        this._viewPortHeight = viewPortHeight;

        
        var invZoomLevel = 1 / zoomLevel,
            info = isc.VMLRenderer._fitBlockCoordinateSystem(
                invZoomLevel, invZoomLevel, 0, 0);
        
        this._textWidth = info.width;
        this._textCoordsize = info.coordsize0;

        if (this._inInit) {
            return;
        }

        
        var drawItems = this.drawPane.drawItems,
            numDrawItems = (drawItems ? drawItems.length : 0),
            stack = [{
                drawItems: drawItems,
                numDrawItems: numDrawItems,
                index: 0
            }],
            stackLength = 1;

        while (stackLength > 0) {
            var s = stack[stackLength - 1];
            drawItems = s.drawItems;
            numDrawItems = s.numDrawItems;

            for (var i = s.index; i < numDrawItems; ++i) {
                var drawItem = drawItems[i];
                if (isc.isA.DrawGroup(drawItem)) {
                    s.index = i + 1;
                    drawItems = drawItem.drawItems;
                    stack[stackLength++] = {
                        drawItems: drawItems,
                        numDrawItems: (drawItems ? drawItems.length : 0),
                        index: 0
                    };
                    // Continue at the top of the outer loop.
                    i = numDrawItems + 1;
                } else if (drawItem != null) {
                    var handle = drawItem._vmlRendererHandle;
                    if (handle != null) {
                        
                        var shouldFlush = (handle._diffFlags1 == 0 && handle._diffFlags2 == 0);
                        handle.setProperty(
                            "transform", undef, true, globalRotationWasZero, zoomLevelChanged);
                        if (shouldFlush) {
                            handle.flush();
                        }
                    }
                }
            }
            if (i == numDrawItems) {
                --stackLength;
            }
        }
    },

    _writeIdAndAlt : function (id, alt) {
        if (id != null) {
            this.buffer.append(" ID='", id, "'");
        }
        if (alt != null) {
            this.buffer.append(" ALT='", String.asAttValue(alt, false, false), "'");
        }
        
    },

    
    _writeStyle : function (config, tagName, isText, isImage, leaveStyleOpen) {
        
        var buffer = this.buffer;

        
        if (this._referenceGlobalShapeType(isText, isImage, config.transform)) {
            buffer.append(
                isText ? this._globalTextShapeTypeRef : this._globalShapeTypeRef);
        }

        
        var isGroup = (tagName == "GROUP");
        if (isGroup || tagName == "SHAPE") {
            
            var n = (50 * this.scale).toFixed(0);
            buffer.append(
                " COORDORIGIN='-", n, ",-", n, "' COORDSIZE='", n, ",", n,
                "' STYLE='left:-", n, "px;top:-", n, "px;width:", n, "px;height:", n, "px;");
        } else {
            buffer.append(" STYLE='");
        }
        buffer.append("position:absolute;");
        if (!isGroup) {
            buffer.append(
                "visibility:", String._asCSSDeclarationValue(config.visibility), ";");
        }
        if (config.zIndex != null) buffer.append("z-index:", +config.zIndex, ";");
        if (!leaveStyleOpen) {
            buffer.append("'");
        }
    },

    
    _writeTextGroupStart : function (buffer) {
        
        var width = this._textWidth, coordsize = this._textCoordsize;
        buffer.append(
            this.startTagVML("GROUP"),
            " STYLE='left:0px;top:0px;width:", width, "px;height:", width,
            "px;' COORDORIGIN='0,0' COORDSIZE=", coordsize, ",", coordsize, "'>");
    },
    // This is called for each text shape whenever the global zoomLevel is changed:
    _updateTextGroup : function (vmlHandle) {
        
        var width = this._textWidth.toFixed(0),
            coordsize = this._textCoordsize.toFixed(0),
            vmlHandleStyle = vmlHandle.style;
        vmlHandleStyle.width = vmlHandleStyle.height = width + "px";
        vmlHandle.coordsize = coordsize + "," + coordsize;
        // The CSS `left` will stay 0px and the the `coordorigin` will stay "0,0".
    },

    _writeImageGroupStart : function (buffer, config) {
        

        
        var sin = this._sin, cos = this._cos,
            cx = this._viewPortWidth / 2, cy = this._viewPortHeight / 2,
            zoomLevel = this._zoomLevel,
            translate0 = this._translate0, translate1 = this._translate1,
            transform = config.transform,
            l00 = transform.m00, l01 = transform.m01, l02 = transform.m02,
            l10 = transform.m10, l11 = transform.m11, l12 = transform.m12,
            // Let `t` be the global transform right-mulitplied by the local transform:
            t = isc.AffineTransform.create(
                    zoomLevel * (l00 * cos - l10 * sin),
                    zoomLevel * (l01 * cos - l11 * sin),
                    zoomLevel * (l02 * cos - l12 * sin + cx * (1 - cos) + cy * sin) + translate0,
                    zoomLevel * (l00 * sin + l10 * cos),
                    zoomLevel * (l01 * sin + l11 * cos),
                    zoomLevel * (l02 * sin + l12 * cos - cx * sin + cy * (1 - cos)) + translate1),
            decomp = isc.AffineTransform._decomposeTransform(t, 0, 0),
            flipY = (decomp.h11 < 0),
            alpha = decomp.theta,
            sinAlpha = Math.sin(alpha), cosAlpha = Math.cos(alpha),
            h00 = decomp.h00, h01 = decomp.h01,
            h10 = decomp.h10, h11 = decomp.h11,
            h00ph11 = h00 + h11,
            delta = (h00 - h11) * Math.cos(2 * alpha) - (h01 + h10) * Math.sin(2 * alpha),
            l02mcx = l02 - cx,
            l12mcy = l12 - cy,
            // The center of rotation of the <image/>:
            ex = (config.left + config.width / 2),
            ey = (config.top + config.height / 2),

            // `alpha` is the unknown rotation, and `sx`, `sy` are the unknown scale and `dx`,
            // `dy` are the unknown translation.
            sx = (h00ph11 + delta) / (2 * zoomLevel),
            sy = (h00ph11 - delta) / (2 * zoomLevel),
            dx = (
                cos * l02mcx - sin * l12mcy + cx +
                sx * ((cosAlpha - 1) * ex - sinAlpha * ey) +
                -((cos - 1) * translate0 - sin * translate1) / zoomLevel),
            dy = (
                sin * l02mcx + cos * l12mcy + cy +
                sy * (sinAlpha * ex + (cosAlpha - 1) * ey) +
                -(sin * translate0 + (cos - 1) * translate1) / zoomLevel),

            
            scale = this.scale,
            info = isc.VMLRenderer._fitBlockCoordinateSystem(sx, sy, scale * dx, scale * dy);

        buffer.append(
            this.startTagVML("GROUP"), " STYLE='position:absolute;left:", info.left,
            "px;top:", info.top, "px;width:", info.width, "px;height:", info.height, "px;");
        if (flipY) {
            buffer.append("flip:y;");
        }
        buffer.append(
            "' coordorigin='", info.coordorigin0, ",", info.coordorigin1,
            "' coordsize='", info.coordsize0, ",", info.coordsize1, "'>");

        
        return alpha * 180 / Math.PI;
    },

    _getSkewMatrixAndOffsetOutput: { matrix: null, offset: null },
    _getSkewMatrixAndOffset : function (transform, isText) {
        
        var output = this._getSkewMatrixAndOffsetOutput;

        
        var sin = this._sin, cos = this._cos,
            cx = this._viewPortWidth / 2, cy = this._viewPortHeight / 2,
            zoomLevel = this._zoomLevel,
            translate0 = this._translate0, translate1 = this._translate1,
            l00 = transform.m00, l01 = transform.m01, l02 = transform.m02,
            l10 = transform.m10, l11 = transform.m11, l12 = transform.m12,
            l02mcx = l02 - cx, l12mcy = l12 - cy,

            m00 = cos * l00 - sin * l10,
            m01 = cos * l01 - sin * l11,
            m10 = sin * l00 + cos * l10,
            m11 = sin * l01 + cos * l11,
            m02 = (
                zoomLevel * (l02mcx * cos - l12mcy * sin + cx) +
                (1 - cos) * translate0 + sin * translate1),
            m12 = (
                zoomLevel * (l02mcx * sin + l12mcy * cos + cy) +
                -sin * translate0 + (1 - cos) * translate1);

        if (isText && zoomLevel != 1) {
            
            m00 *= zoomLevel;
            m01 *= zoomLevel;
            m10 *= zoomLevel;
            m11 *= zoomLevel;
        }

        
        output.matrix = isc.StringBuffer.concat(
            Math.round(65536 * m00), "f,",
            Math.round(65536 * m01), "f,",
            Math.round(65536 * m10), "f,",
            Math.round(65536 * m11), "f,0,0");

        
        output.offset = isc.StringBuffer.concat(
            m02.toFixed(this.skewOffsetPrecision), "px,",
            m12.toFixed(this.skewOffsetPrecision), "px");

        return output;
    },

    _getXTranslation : function (transform) {
        if (transform == null) return 0;
        
        var sin = this._sin, cos = this._cos,
            cx = this._viewPortWidth / 2, cy = this._viewPortHeight / 2,
            zoomLevel = this._zoomLevel,
            translate0 = this._translate0, translate1 = this._translate1,
            ex = (cx * (zoomLevel - 1) + translate0),
            ey = (cy * (zoomLevel - 1) + translate1);
        return transform.m02 + ((cos - 1) * ex + sin * ey) / zoomLevel;
    },

    _getYTranslation : function (transform) {
        if (transform == null) return 0;
        
        var sin = this._sin, cos = this._cos,
            cx = this._viewPortWidth / 2, cy = this._viewPortHeight / 2,
            zoomLevel = this._zoomLevel,
            translate0 = this._translate0, translate1 = this._translate1,
            ex = (cx * (zoomLevel - 1) + translate0),
            ey = (cy * (zoomLevel - 1) + translate1);
        return transform.m12 + (-sin * ex + (cos - 1) * ey) / zoomLevel;
    },

    _writeSkewEl : function (buffer, config, prevTagClosed, isText) {
        
        // Write out the skew subelement if necessary.
        var transform = config.transform;
        if (!this._isTranslation(transform)) {
            if (!prevTagClosed) {
                buffer.append(">");
            }
            var info = this._getSkewMatrixAndOffset(transform, isText);
            buffer.append(
                this.startTagVML(isc.VMLRenderer._$SKEW), " on='t' matrix='", info.matrix,
                "' offset='", info.offset, "' />");
            return true;
        } else {
            return prevTagClosed;
        }
    },

    

    _writeStrokeAttrs : function (config) {
        if (config.stroked) {
            this.buffer.append(
                " strokecolor='", String.asAttValue(config.lineColor, false, false),
                "' strokeweight='", Math.round(config.lineWidth * this.drawPane.zoomLevel), "px'");
        } else {
            this.buffer.append(isc.VMLRenderer._$strokedFalse);
        }
    },
    _writeStrokeEl : function (buffer, config, prevTagClosed) {
        
        if (config.stroked) {
            var lineOpacity = config.lineOpacity,
                linePattern = config.linePattern,
                startArrow = config.startArrow,
                endArrow = config.endArrow,
                joinStyle = config.joinStyle,
                lineCap = config.lineCap;
            if (!(isc.isA.String(linePattern) && isc.VMLRenderer._dashstyleRegExp.test(linePattern))) {
                linePattern = isc.VMLRenderer._$solid;
            }
            if (!(isc.isA.String(joinStyle) && isc.VMLRenderer._joinstyleRegExp.test(joinStyle))) {
                joinStyle = "miter";
            }
            if (!(isc.isA.String(lineCap) && isc.VMLRenderer._endcapRegExp.test(lineCap))) {
                lineCap = "round";
            }
            if (!(isc.isA.String(startArrow) && isc.VMLRenderer._arrowRegExp.test(startArrow))) {
                startArrow = isc.VMLRenderer._$none;
            }
            if (!(isc.isA.String(endArrow) && isc.VMLRenderer._arrowRegExp.test(endArrow))) {
                endArrow = isc.VMLRenderer._$none;
            }

            // If the stroke styling deviates from the defaults then we need to write out a
            // <stroke/> element.
            //
            // This is currently always the case for DrawItem types other than DrawCurve, DrawLine,
            // DrawOval because DrawItem.lineJoin is not a supported config property, and because
            // the default DrawItem join style is "miter" (for consistency with the SVG and
            // CanvasRenderingContext2D default stroke-linejoin/lineJoin of "miter"), VML's
            // default join style of "round" is different:
            // http://www.w3.org/TR/NOTE-VML#_Toc416858395
            //
            // For 90 degree angles, a miterlimit greater than or equal to sqrt(2) will prevent
            // the join style from switching to "bevel"; however, a DrawRect can be skewed to the
            // point where the miterlimit can make a difference.
            if ((joinStyle != "round" &&
                 !isc.isA.DrawCurve(config._drawItem) &&
                 !isc.isA.DrawLine(config._drawItem) &&
                 !isc.isA.DrawOval(config._drawItem)) ||
                lineOpacity != 1 ||
                linePattern != isc.VMLRenderer._$solid ||
                lineCap != "round" ||
                startArrow != isc.VMLRenderer._$none ||
                endArrow != isc.VMLRenderer._$none)
            {
                if (!prevTagClosed) {
                    buffer.append(">");
                }
                buffer.append(
                    this.startTagVML(isc.VMLRenderer._$STROKE),
                    " OPACITY='", Math.round(65536 * lineOpacity),
                    "f' DASHSTYLE='", linePattern,
                    "' MITERLIMIT='4' JOINSTYLE='", joinStyle,
                    "' ENDCAP='", lineCap,
                    "' STARTARROW='", startArrow,
                    "' STARTARROWWIDTH='wide' ENDARROW='", endArrow,
                    "' ENDARROWWIDTH='wide'>",
                    this.endTagVML(isc.VMLRenderer._$STROKE));
                return true;
            }
        }
        return prevTagClosed;
    },
    _writeFillAttrs : function (config) {
        if (config.filled) {
            var fillColor = config.fillColor;
            if (fillColor) {
                this.buffer.append(
                    " fillcolor='", String.asAttValue(fillColor, false, false), "'");
            }
        } else {
            this.buffer.append(isc.VMLRenderer._$filledFalse);
        }
    },
    _writeFillEl : function (buffer, config, prevTagClosed) {
        
        var fillOpacity = config.fillOpacity,
            fillGradient = config.fillGradient;
        if (config.filled && (fillOpacity != 1 || fillGradient != null)) {
            if (!prevTagClosed) {
                buffer.append(">");
            }
            buffer.append(this.startTagVML(isc.VMLRenderer._$FILL));
            var def = (isc.isA.String(fillGradient) ? this.drawPane.getGradient(fillGradient) : fillGradient);
            if (def != null && !isc.isAn.emptyObject(def)) {
                var simpleOrLinear = (
                        isc.DrawItem._isSimpleGradient(def) || isc.DrawItem._isLinearGradient(def)),
                    definesStartAndEndColors = (def.startColor && def.endColor);

                if (simpleOrLinear) {
                    var boundingBox = config.getBoundingBox(false, config._tempBoundingBox),
                        vector = config._normalizeLinearGradient(def, boundingBox),
                        angle = (
                            def.direction ||
                            (Math.atan2(vector[3] - vector[1], vector[2] - vector[0]) * 180 / Math.PI));
                    
                    angle = (270 - angle) % 360;
                    if (angle < 0) {
                        angle += 360;
                    }
                    buffer.append(
                        " TYPE='gradient' ANGLE='", angle.toFixed(this.fillAnglePrecision),
                        "' COLORS='");
                } else {
                    buffer.append(
                        " TYPE='gradienttitle' OPACITY='1.0' FOCUS='100%' FOCUSSIZE='0,0' FOCUSPOSITION='0.5,0.5' METHOD='linear' COLORS='");
                }
                
                var fuchsiaRegExp = isc.VMLRenderer._fuchsiaRegExp;
                if (simpleOrLinear && def.startColor && def.endColor) {
                    var startColor = def.startColor,
                        endColor = def.endColor;
                    if (fuchsiaRegExp.test(startColor)) startColor = "#ff01ff";
                    if (fuchsiaRegExp.test(endColor)) endColor = "#ff01ff";
                    buffer.append("0% ", startColor, ", 100% ", endColor);
                } else {
                    var colorStops = def.colorStops,
                        numColorStops = colorStops && colorStops.length;

                    for (var i = 0; i < numColorStops; ++i) {
                        if (i > 0) {
                            buffer.append(", ");
                        }
                        var colorstop = colorStops[i];
                        if (isc.isA.String(colorstop.offset) && String(colorstop.offset).endsWith("%")) {
                            buffer.append(colorstop.offset);
                        } else {
                            buffer.append(parseFloat(colorstop.offset) * 100, "%");
                        }
                        var color = colorstop.color;
                        if (fuchsiaRegExp.test(color)) color = "#ff01ff";
                        buffer.append(" ", color);
                    }
                }
            } else {
                buffer.append(" ON='true");
            }
            buffer.append(
                "' OPACITY='", Math.round(65536 * fillOpacity), "f'>",
                this.endTagVML(isc.VMLRenderer._$FILL));
            return true;
        } else {
            return prevTagClosed;
        }
    },

    _writeShadowEl : function (buffer, shadow, prevTagClosed) {
        
        
        if (shadow) {
            if (!prevTagClosed) {
                buffer.append(">");
            }
            var shadowOpacity = 1.0;
            if (isc.isA.Number(shadow.blur) && shadow.blur >= 0) {
                // 25% opacity at `shadow.blur = 10`.
                var mu = Math.LN2 / 5;
                shadowOpacity = Math.exp(-mu * shadow.blur);
            }
            buffer.append(
                this.startTagVML(isc.VMLRenderer._$SHADOW),
                
                " ON='t' OPACITY='", Math.round(65536 * shadowOpacity),
                "f' COLOR='", String.asAttValue(shadow.color, false, false),
                "' OFFSET='", shadow.offset[0].toFixed(this.shadowOffsetPrecision), "pt,",
                shadow.offset[1].toFixed(this.shadowOffsetPrecision), "pt'>",
                this.endTagVML(isc.VMLRenderer._$SHADOW));
            return true;
        } else {
            return prevTagClosed;
        }
    },

    _writeImagedataEl : function (buffer, config, prevTagClosed) {
        
        if (!prevTagClosed) {
            buffer.append(">");
        }
        buffer.append(
            this.startTagVML("IMAGEDATA"),
            " SRC='", String.asAttValue(config.src, false, false), "'/>");
        return true;
    },

    
    _normalizeContentLines : function (contents) {
        if (!isc.isA.String(contents)) {
            contents = String(contents);
        }
        var r = isc.DrawLabel._newlineRegExp;
        r.lastIndex = 0;
        return (r.test(contents) ? contents.replace(r, "\n") : contents);
    },

    
    _selectFromFontFamily : function (quote, forCSSDecl, fontFamily) {
        
        var emptyString = isc.emptyString;
        if (!isc.isA.String(fontFamily)) {
            return emptyString;
        }
        var quoteFn = isc.VMLRenderer._quote,
            unquote = isc.VMLRenderer._unquote,
            genericFontFamiliesRegExp = isc.VMLRenderer._genericFontFamiliesRegExp,
            headRegExp = isc.VMLRenderer._headFontFamilyRegExp,
            tailRegExp = isc.VMLRenderer._tailFontFamilyRegExp;
        headRegExp.lastIndex = tailRegExp.lastIndex = 0;

        var hasVMLBug = true,
            first = true,
            output = null;
        if (!hasVMLBug) {
            output = isc.StringBuffer.create();
        }

        for (var r = headRegExp, match = null; (match = r.exec(fontFamily)) != null; r = tailRegExp) {
            
            if (match[0].length == 0 && !(match.index < r.lastIndex)) {
                ++r.lastIndex;
            }

            var q = match[1], quotedFont = match[2], unquotedFont = match[3],
                quoted = (q != null && q != emptyString),
                font = emptyString,
                isGeneric = false;
            if (quoted) {
                font = unquote(q, quotedFont);
            } else {
                font = unquotedFont.trim();
                
                isGeneric = (font != emptyString && genericFontFamiliesRegExp.test(font));
            }

            if (font != emptyString) {
                if (hasVMLBug) {
                    // Check whether the font is available.  If the font is a generic font family
                    // then we cannot know which exact font the browser will substitute for it,
                    // so try a default font specified in a DrawLabel class property.
                    var fontToCheck = font;
                    if (isGeneric) {
                        if (font == "serif") {
                            fontToCheck = isc.DrawLabel.defaultSerifFont;
                        } else if (font == "sans-serif") {
                            fontToCheck = isc.DrawLabel.defaultSansSerifFont;
                        } else if (font == "monospace") {
                            fontToCheck = isc.DrawLabel.defaultMonospaceFont;
                        } else if (font == "cursive") {
                            fontToCheck = isc.DrawLabel.defaultCursiveFont;
                        } else {
                            
                            fontToCheck = isc.DrawLabel.defaultFantasyFont;
                        }
                        // If the default font is null then the generic font family name will be
                        // output below.
                        if (!isc.isA.String(font)) {
                            font = null;
                        }
                    }
                    if (fontToCheck != null && this._isFontAvailable(fontToCheck)) {
                        if (forCSSDecl) {
                            fontToCheck = String._asFontFamilyValue(fontToCheck);
                        }
                        return quote + fontToCheck + quote;
                    } else if (isGeneric) {
                        // We need to return something here so just return the generic font family
                        // and hope that it works.
                        return font;
                    }
                } else { // !hasVMLBug
                    if (!first) {
                        output.append(",");
                    }
                    if (isGeneric) {
                        output.append(font);
                    } else if (forCSSDecl) {
                        output.append(quote, String._asFontFamilyValue(font), quote);
                    } else {
                        output.append(quoteFn(quote, font));
                    }
                    first = false;
                }
            }
        }

        if (hasVMLBug) {
            // If none of the fonts were available and there was no generic font family
            // specified then return null.
            return null;
        } else {
            var outputStr = output.release(false);
            return (outputStr == emptyString ? null : outputStr);
        }
    },

    
    _controlFonts: ["serif", "sans-serif", "monospace"],
    _isFontAvailableProps: {
        fontFamily: "",
        fontWeight: "normal",
        fontSize: 72,
        fontStyle: "normal"
    },
    _controlText: "mmmmmmmmmmlli",
    _controlFontMeasurements: null,
    _isFontAvailableCacheMaxSize: 30,
    _isFontAvailable : function (font) {
        
        var controlText = this._controlText,
            controlFonts = this._controlFonts,
            drawPane = this.drawPane,
            props = this._isFontAvailableProps;

        var control = this.drawPane._controlFontMeasurements;
        if (control == null) {
            control = this.drawPane._controlFontMeasurements = new Array(6);
            for (var i = 0; i < controlFonts.length; ++i) {
                props.fontFamily = controlFonts[i];
                var m = drawPane.measureLabel(controlText, props);
                control[2 * i] = m.width;
                control[2 * i + 1] = m.height;
            }
        }

        var cache = this.drawPane._isFontAvailableCache;
        if (cache == null) {
            cache = this.drawPane._isFontAvailableCache = new this.drawPane._lruCache(
                this._isFontAvailableCacheMaxSize);
        } else if (this.drawPane._lruCacheContains(cache, font)) {
            return this.drawPane._lruCacheGet(cache, font);
        }

        var notAvailable = true;
        for (var i = 0; notAvailable && i < controlFonts.length; ++i) {
            props.fontFamily = "'" + String._asFontFamilyValue(font) + "'," + controlFonts[i];
            var controlWidth = control[2 * i],
                controlHeight = control[2 * i + 1],
                test = drawPane.measureLabel(controlText, props);
            if (test.width != controlWidth || test.height != controlHeight) {
                
                notAvailable = false;
            }
        }
        var available = !notAvailable;
        this.drawPane._lruCachePut(cache, font, available);
        return available;
    },

    _writeTextpathEl : function (buffer, config, prevTagClosed) {
        
        if (!prevTagClosed) {
            buffer.append(">");
        }
        buffer.append(this.startTagVML(isc.VMLRenderer._$TEXTPATH), " ON='t' XSCALE='t'");
        var fontFamily = config.fontFamily,
            fontWeight = config.fontWeight,
            fontSize = config.fontSize,
            fontStyle = config.fontStyle,
            textAlign = config.textAlign;
        if (fontFamily != null && !isc.isA.String(fontFamily)) {
            fontFamily = null;
        } else {
            // Pass in `true` so that the CSS declaration value is escaped.
            fontFamily = this._selectFromFontFamily("'", true, fontFamily);
        }
        if (fontWeight != null && !isc.isA.String(fontWeight)) {
            fontWeight = null;
        }
        if (fontSize != null && !isc.isA.String(fontSize)) {
            fontSize = null;
        }
        if (!(isc.isA.String(fontStyle) && isc.VMLRenderer._fontStyleRegExp.test(fontStyle))) {
            fontStyle = null;
        }
        if (!(isc.isA.String(textAlign) && isc.VMLRenderer._textAlignRegExp.test(textAlign))) {
            textAlign = null;
        }
        if (fontFamily != null || fontWeight != null || fontSize != null || fontStyle != null) {
            var styleQuote = isc.DrawPane._getEnclosingQuote(fontFamily);
            buffer.append(" STYLE=", styleQuote);
            if (fontWeight != null) {
                buffer.append("font-weight:", String._asCSSDeclarationValue(fontWeight), ";");
            }
            if (fontSize != null) {
                buffer.append("font-size:", String._asCSSDeclarationValue(fontSize), ";");
            }
            if (fontStyle != null) {
                buffer.append("font-style:", fontStyle, ";");
            }
            if (textAlign != null) {
                buffer.append("text-align:", textAlign, ";");
            }
            if (fontFamily != null) {
                buffer.append("font-family:", fontFamily, ";");
            }
            buffer.append(styleQuote);
        }
        var contents = config.contents;
        if (contents != null) {
            contents = String(contents);
            buffer.append(
                " STRING='",
                String.asAttValue(this._normalizeContentLines(contents), false, false),
                "'");
        }
        buffer.append("/>");
        return true;
    },

    _writeRemainingVML : function (config, tagName, prevTagClosed) {
        
        if (prevTagClosed == null) {
            prevTagClosed = false;
        }
        var buffer = this.buffer;
        this._writeStrokeAttrs(config);
        this._writeFillAttrs(config);
        // NB: draw() relies on this structure to access stroke (firstChild) and fill (nextSibling)
        // elements without IDs
        prevTagClosed = this._writeStrokeEl(buffer, config, prevTagClosed);
        prevTagClosed = this._writeFillEl(buffer, config, prevTagClosed);
        prevTagClosed = this._writeShadowEl(buffer, config.shadow, prevTagClosed);
        var isText = (tagName == "TEXT");
        if (isText || tagName != "IMAGE") {
            prevTagClosed = this._writeSkewEl(buffer, config, prevTagClosed, isText);
        }
        if (isText) {
            prevTagClosed = this._writeTextpathEl(buffer, config, prevTagClosed);
            buffer.append(this.startTagVML(isc.VMLRenderer._$PATH), " TEXTPATHOK='t'/>");
        }
        if (prevTagClosed) {
            buffer.append(this.endTagVML(tagName == "TEXT" ? "SHAPE" : tagName));
        } else {
            buffer.append("/>");
        }
    },

    _writeTextboxContentEl : function (buffer, config) {
        
        if (config.contents != null) {
            buffer.append("<NOBR");
            if (config.lineColor != null) {
                buffer.append(" STYLE='color:", String._asCSSDeclarationValue(config.lineColor), "'");
            }
            buffer.append(">", isc.makeXMLSafe(config.contents), "</NOBR>");
        }
    },

    rect : function (id, config) {
        config.arcsize = 0;
        return this.roundrect(id, config, "rect");
    },

    roundrect : function (id, config, type) {
        type = type || "roundrect";
        var transform = config.transform;
        config._getPathVML = isc.VMLRenderer._roundrectGetPathVML;
        if (!this._usePredefinedShape(transform)) {
            return this.shape(id, config, type);
        }

        var scale = this.scale,
            dx = this._getXTranslation(transform),
            dy = this._getYTranslation(transform),
            left = Math.round(scale * (config.left + dx)),
            top = Math.round(scale * (config.top + dy)),
            width = Math.round(scale * config.width),
            height = Math.round(scale * config.height);

        // Reduce "roundrect" elements with zero "arcsize" to "rect" elements.
        var tagName = (config.arcsize == 0 ? "RECT" : "ROUNDRECT");
        this.buffer.append(this.startTagVML(tagName));
        this._writeIdAndAlt(id, config.alt);
        this._writeStyle(config, tagName, false, false, true);
        var buffer = this.buffer;
        buffer.append(
            "left:", left, "px;top:", top, "px;width:", width, "px;height:", height, "px;'");
        if (config.arcsize != 0) {
            buffer.append(" ARCSIZE='", config.arcsize.toFixed(this.precision), "'");
        }
        this._writeRemainingVML(config, tagName);
        return isc.VMLRendererHandle.create(type, this, id, config);
    },

    line : function (id, config) {
        var transform = config.transform;
        config._getPathVML = isc.VMLRenderer._lineGetPathVML;
        if (!this._usePredefinedShape(transform)) {
            return this.shape(id, config, "line");
        }
        var scale = this.scale,
            dx = this._getXTranslation(transform),
            dy = this._getYTranslation(transform),
            fromX = Math.round(scale * (config.fromX + dx)),
            fromY = Math.round(scale * (config.fromY + dy)),
            toX = Math.round(scale * (config.toX + dx)),
            toY = Math.round(scale * (config.toY + dy));

        this.buffer.append(this.startTagVML("LINE"));
        this._writeIdAndAlt(id, config.alt);
        this._writeStyle(config, "LINE", false, false, false);
        this.buffer.append(" FROM='", fromX, ",", fromY, "' TO='", toX, ",", toY, "'");
        this._writeRemainingVML(config, "LINE");
        return isc.VMLRendererHandle.create("line", this, id, config);
    },

    polyline : function (id, config) {
        
        var transform = config.transform;
        config._getPathVML = isc.VMLRenderer._polylineGetPathVML;
        if (!this._usePredefinedShape(transform)) {
            return this.shape(id, config, "polyline");
        }

        var buffer = this.buffer,
            scale = this.scale,
            dx = this._getXTranslation(transform),
            dy = this._getYTranslation(transform);
        buffer.append(this.startTagVML("POLYLINE"));
        this._writeIdAndAlt(id, config.alt);
        this._writeStyle(config, "POLYLINE", false, false, false);
        var points = config.points,
            numPoints = points.length;
        if (numPoints > 0) {
            buffer.append(" POINTS='");
            this._appendPolylinePointsText(buffer, points, config.closePolyline, dx, dy);
            buffer.append("'");
        }
        this._writeRemainingVML(config, "POLYLINE");
        return isc.VMLRendererHandle.create("polyline", this, id, config);
    },
    _appendPolylinePointsText : function (buffer, points, closePolyline, dx, dy) {
        
        var scale = this.scale,
            numPoints = points.length,
            point0 = points[0],
            x0 = Math.round(scale * (point0[0] + dx)),
            y0 = Math.round(scale * (point0[1] + dy));
        buffer.append(x0, " ", y0);
        for (var i = 1; i < numPoints; ++i) {
            var point = points[i],
                x = Math.round(scale * (point[0] + dx)),
                y = Math.round(scale * (point[1] + dy));
            buffer.append(" ", x, " ", y);
        }
        if (closePolyline) {
            buffer.append(" ", x0, " ", y0);
        }
    },

    curve : function (id, config) {
        var transform = config.transform;
        config._getPathVML = isc.VMLRenderer._curveGetPathVML;
        if (!this._usePredefinedShape(transform)) {
            return this.shape(id, config, "curve");
        }

        var scale = this.scale,
            dx = this._getXTranslation(transform),
            dy = this._getYTranslation(transform),
            fromX = Math.round(scale * (config.fromX + dx)),
            fromY = Math.round(scale * (config.fromY + dy)),
            toX = Math.round(scale * (config.toX + dx)),
            toY = Math.round(scale * (config.toY + dy)),
            control1X = Math.round(scale * (config.control1X + dx)),
            control1Y = Math.round(scale * (config.control1Y + dy)),
            control2X = Math.round(scale * (config.control2X + dx)),
            control2Y = Math.round(scale * (config.control2Y + dy));

        this.buffer.append(this.startTagVML("CURVE"));
        this._writeIdAndAlt(id, config.alt);
        this._writeStyle(config, "CURVE", false, false, false);
        this.buffer.append(
            " FROM='", fromX, ",", fromY, "' TO='", toX, ",", toY,
            "' CONTROL1='", control1X, ",", control1Y,
            "' CONTROL2='", control2X, ",", control2Y, "'");
        this._writeRemainingVML(config, "CURVE");
        return isc.VMLRendererHandle.create("curve", this, id, config);
    },

    oval : function (id, config) {
        config.startangle = 0;
        config.endangle = 360;
        return this.arc(id, config, "oval");
    },

    arc : function (id, config, type) {
        type = type || "arc";
        var transform = config.transform;
        config._getPathVML = isc.VMLRenderer._arcGetPathVML;
        if (!this._usePredefinedShape(transform)) {
            return this.shape(id, config, type);
        }

        var scale = this.scale,
            dx = this._getXTranslation(transform),
            dy = this._getYTranslation(transform),
            left = Math.round(scale * (config.left + dx)),
            top = Math.round(scale * (config.top + dy)),
            width = Math.round(scale * config.width),
            height = Math.round(scale * config.height);

        var isOval = (config.startangle == 0 && config.endangle == 360),
            tagName = (isOval ? "OVAL" : "ARC");
        this.buffer.append(this.startTagVML(tagName));
        this._writeIdAndAlt(id, config.alt);
        this._writeStyle(config, tagName, false, false, true);
        this.buffer.append(
            "left:", left, "px;top:", top, "px;width:", width, "px;height:", height, "px;'");
        if (!isOval) {
            this.buffer.append(
                "STARTANGLE='", config.startangle.toFixed(this.precision), "'",
                " ENDANGLE='", config.endangle.toFixed(this.precision), "'");
        }
        this._writeRemainingVML(config, tagName);
        return isc.VMLRendererHandle.create(type, this, id, config);
    },

    image : function (id, config) {
        
        var buffer = this.buffer,
            scale = this.scale,
            transform = config.transform,
            useMatrixFilter = config.useMatrixFilter,
            translateViaLeftTop = this._usePredefinedShape(transform),
            useParentGroup = !(useMatrixFilter || translateViaLeftTop),
            width = config.width,
            height = config.height,
            dx = 0, dy = 0;

        var m00 = 0, m01 = 0, m10 = 0, m11 = 0, angle = 0;
        if (useMatrixFilter) {
            
            var sin = this._sin, cos = this._cos,
                cx = this._viewPortWidth / 2, cy = this._viewPortHeight / 2,
                zoomLevel = this._zoomLevel,
                translate0 = this._translate0, translate1 = this._translate1,
                l00 = transform.m00, l01 = transform.m01, l02 = transform.m02,
                l10 = transform.m10, l11 = transform.m11, l12 = transform.m12;
            m00 = l00 * cos - l10 * sin;
            m01 = l01 * cos - l11 * sin;
            m10 = l00 * sin + l10 * cos;
            m11 = l01 * sin + l11 * cos;
            var ex = (zoomLevel * cx + translate0), ey = (zoomLevel * cy + translate1),
                zl02mcx = zoomLevel * (l02 - cx), zl12mcy = zoomLevel * (l12 - cy),
                detL = (l00 * l11 - l01 * l10),
                denom = detL * zoomLevel;
            dx = (
                cos * (l11 * ex - l01 * ey) +
                sin * (l01 * ex + l11 * ey) +
                l11 * zl02mcx - l01 * zl12mcy - detL * translate0) / denom;
            dy = (
                sin * -(l00 * ex + l10 * ey) +
                cos * -(l10 * ex - l00 * ey) +
                l00 * zl12mcy - l10 * zl02mcx - detL * translate1) / denom;

            
            var fx = translate0 / zoomLevel,
                fy = translate1 / zoomLevel,
                gx = dx + config.left + width / 2 + fx,
                gy = dy + config.top + height / 2 + fy;
            dx += (m00 - 1) * gx + m01 * gy - ((cos - 1) * fx - sin * fy);
            dy += m10 * gx + (m11 - 1) * gy - (sin * fx + (cos - 1) * fy);

            
            dx -= ((Math.abs(m00) - 1) * width + Math.abs(m01) * height) / 2;
            dy -= (Math.abs(m10) * width + (Math.abs(m11) - 1) * height) / 2;

        } else if (translateViaLeftTop) {
            dx = this._getXTranslation(transform);
            dy = this._getYTranslation(transform);

        } else {
            
            angle = this._writeImageGroupStart(buffer, config);
        }

        var left = Math.round(scale * (config.left + dx)),
            top = Math.round(scale * (config.top + dy)),
            width = Math.round(scale * width),
            height = Math.round(scale * height);

        buffer.append(this.startTagVML("IMAGE"));
        this._writeIdAndAlt(id, config.alt);
        this._writeStyle(config, "IMAGE", false, true, true);
        if (useMatrixFilter) {
            
            var matrixFilterPrecision = this.matrixFilterPrecision;

            
            var quote, innerQuote;
            if (isc.Browser.isIE8) {
                quote = "\"";
                innerQuote = "\\'";
                buffer.append("-ms-");
            } else {
                quote = "";
                innerQuote = "\"";
            }
            buffer.append(String.asAttValue(
                "filter:" + quote +
                "progid:DXImageTransform.Microsoft.Matrix(Enabled=true,M11=" +
                m00.toFixed(matrixFilterPrecision) +
                ",M12=" + m01.toFixed(matrixFilterPrecision) +
                ",M21=" + m10.toFixed(matrixFilterPrecision) +
                ",M22=" + m11.toFixed(matrixFilterPrecision) +
                ",SizingMethod=" + innerQuote + "auto expand" + innerQuote + ")" +
                quote), ";");

        } else if (useParentGroup) {
            buffer.append(
                "rotation:" + angle.toFixed(this.rotationPrecision) + ";");
        }
        buffer.append(
            "left:", left, "px;top:", top, "px;width:", width, "px;height:", height,
            "px;' SRC='", String.asAttValue(config.src, false, false), "'");
        this._writeRemainingVML(config, "IMAGE");
        if (useParentGroup) {
            buffer.append(this.endTagVML("GROUP"));
        }
        return isc.VMLRendererHandle.create("image", this, id, config);
    },

    text : function (id, config) {
        config._getPathVML = isc.VMLRenderer._textGetPathVML;
        return this.shape(id, config, "text");
    },

    shape : function (id, config, type) {
        type = type || "shape";
        var transform = config.transform,
            buffer = this.buffer,
            isText = (type == "text");
        if (isText) {
            this._writeTextGroupStart(buffer);
        }
        buffer.append(this.startTagVML("SHAPE"));
        this._writeIdAndAlt(id, config.alt);
        this._writeStyle(config, "SHAPE", isText, false, false);
        buffer.append(" PATH='");
        var dx = 0, dy = 0;
        if (this._isTranslation(transform)) {
            dx = this._getXTranslation(transform);
            dy = this._getYTranslation(transform);
        }
        var pathBuilder = isc.VMLRenderer._vmlPathBuilder._reset(this, dx, dy);
        config._getPathVML(pathBuilder);
        buffer.append("'");
        this._writeRemainingVML(config, (isText ? "TEXT" : "SHAPE"));
        if (isText) {
            buffer.append(this.endTagVML("GROUP"));
        }
        return isc.VMLRendererHandle.create(type, this, id, config);
    },

    group : function (id, config) {
        

        

        var buffer = this.buffer;
        buffer.append(this.startTagVML("GROUP"));
        this._writeIdAndAlt(id, null);
        this._writeStyle(config, "GROUP", false, false, false);
        
        buffer.append(">");
        return isc.VMLRendererHandle.create("group", this, id, config);
    },
    endGroup : function (id) {
        
        this.buffer.append(this.endTagVML("GROUP"));
    }
});

isc.ClassFactory.defineClass("VMLRendererHandle");

isc.VMLRendererHandle.addClassProperties({

    

    _shapeConfigDuplicate : function (shapeConfig, deepCopy) {
        var clone = new isc.VMLRenderer.shapeConfig();
        isc.getProperties(shapeConfig, isc.VMLRenderer.shapeConfigAllPropertiesList, clone);
        clone._drawItem = shapeConfig._drawItem;
        if (deepCopy) {
            clone.transform = isc.VMLRendererHandle._duplicateTransform(shapeConfig.transform);
            clone.points = isc.VMLRendererHandle._duplicatePoints(shapeConfig.points);
        }
        return clone;
    },

    _duplicateTransform : function (transform) {
        return (isc.isAn.AffineTransform(transform) ? transform.duplicate() : transform);
    },

    _duplicatePoints : function (points) {
        if (isc.isAn.Array(points)) {
            var numPoints = points.length,
                clonedPoints = new Array(numPoints);
            for (var i = 0; i < numPoints; ++i) {
                var point = points[i], clonedPoint = new Array(2);
                clonedPoint[0] = point[0];
                clonedPoint[1] = point[1];
                clonedPoints[i] = clonedPoint;
            }
            return clonedPoints;
        } else {
            return points;
        }
    },

    _propertyActions: (function () {
        // Create a mapping from property name to a triple (index, mask, setter function).
        var properties = isc.VMLRenderer.shapeConfigAllPropertiesList,
            numProperties = properties.length,
            actions = {},
            actionIndex = new Array(numProperties);
        for (var i = 0; i < numProperties; ++i) {
            var prop = properties[i], mask1 = 0, mask2 = 0;
            if (i < 30) {
                mask1 = (1 << i);
            } else {
                mask2 = (1 << (i - 30));
            }
            actions[prop] = actionIndex[i] = {
                property: prop,
                index: i,
                mask1: mask1,
                mask2: mask2,
                action: null
            };
        }

        actions.left.action = function (oldConfig, newConfig) {
            
            var dx = 0;
            if (!(newConfig.useMatrixFilter && this._type == "image") &&
                this._vmlRenderer._usePredefinedShape(newConfig.transform))
            {
                dx = this._vmlRenderer._getXTranslation(newConfig.transform);
            }
            this._getVMLHandle().style.left = Math.round(this._vmlRenderer.scale * (newConfig.left + dx));
        };
        actions.top.action = function (oldConfig, newConfig) {
            
            var dy = 0;
            if (!(newConfig.useMatrixFilter && this._type == "image") &&
                this._vmlRenderer._usePredefinedShape(newConfig.transform))
            {
                dy = this._vmlRenderer._getYTranslation(newConfig.transform);
            }
            this._getVMLHandle().style.top = Math.round(this._vmlRenderer.scale * (newConfig.top + dy));
        };
        actions.width.action = function (oldConfig, newConfig) {
            this._getVMLHandle().style.width = Math.round(this._vmlRenderer.scale * newConfig.width);
        };
        actions.height.action = function (oldConfig, newConfig) {
            this._getVMLHandle().style.height = Math.round(this._vmlRenderer.scale * newConfig.height);
        };
        actions.arcsize.action = function (oldConfig, newConfig) {
            
            if ((oldConfig.arcsize == 0) != (newConfig.arcsize == 0)) {
                // Switching from <rect/> to <roundrect/>, or vice versa.
                this._regenerateVML();
            } else {
                
                this._regenerateVML();
            }
        };
        actions.startangle.action = function (oldConfig, newConfig) {
            
            var oldUseOval = (oldConfig.startangle == 0 && oldConfig.endangle == 360),
                newUseOval = (newConfig.startangle == 0 && newConfig.endangle == 360);
            if (oldUseOval != newUseOval) {
                // Switch from using <oval/> to <arc/>, and vice versa.
                this._regenerateVML();
            } else {
                this._getVMLHandle().startangle = newConfig.startangle.toFixed(this._vmlRenderer.precision);
            }
        };
        actions.endangle.action = function (oldConfig, newConfig) {
            
            var oldUseOval = (oldConfig.startangle == 0 && oldConfig.endangle == 360),
                newUseOval = (newConfig.startangle == 0 && newConfig.endangle == 360);
            if (oldUseOval != newUseOval) {
                this._regenerateVML();
            } else {
                this._getVMLHandle().endangle = newConfig.endangle.toFixed(this._vmlRenderer.precision);
            }
        };

        // `fromX` and `fromY` properties are set simultaneously.  Same with toX/Y,
        // control1X/Y, and control2X/Y.
        actions.fromX.mask1 = actions.fromY.mask1 = (actions.fromX.mask1 | actions.fromY.mask1);
        actions.fromX.mask2 = actions.fromY.mask2 = (actions.fromX.mask2 | actions.fromY.mask2);
        actions.fromX.action = actions.fromY.action = function (oldConfig, newConfig) {
            var scale = this._vmlRenderer.scale, dx = 0, dy = 0;
            if (this._vmlRenderer._isTranslation(newConfig.transform)) {
                dx = this._vmlRenderer._getXTranslation(newConfig.transform);
                dy = this._vmlRenderer._getYTranslation(newConfig.transform);
            }
            this._getVMLHandle().from = Math.round(scale * (newConfig.fromX + dx)) + " " + Math.round(scale * (newConfig.fromY + dy));
        };

        actions.toX.mask1 = actions.toY.mask1 = (actions.toX.mask1 | actions.toY.mask1);
        actions.toX.mask2 = actions.toY.mask2 = (actions.toX.mask2 | actions.toY.mask2);
        actions.toX.action = actions.toY.action = function (oldConfig, newConfig) {
            var scale = this._vmlRenderer.scale, dx = 0, dy = 0;
            if (this._vmlRenderer._isTranslation(newConfig.transform)) {
                dx = this._vmlRenderer._getXTranslation(newConfig.transform);
                dy = this._vmlRenderer._getYTranslation(newConfig.transform);
            }
            this._getVMLHandle().to = Math.round(scale * (newConfig.toX + dx)) + " " + Math.round(scale * (newConfig.toY + dy));
        };

        actions.control1X.mask1 = actions.control1Y.mask1 = (actions.control1X.mask1 | actions.control1Y.mask1);
        actions.control1X.mask2 = actions.control1Y.mask2 = (actions.control1X.mask2 | actions.control1Y.mask2);
        actions.control1X.action = actions.control1Y.action = function (oldConfig, newConfig) {
            var scale = this._vmlRenderer.scale, dx = 0, dy = 0;
            if (this._vmlRenderer._isTranslation(newConfig.transform)) {
                dx = this._vmlRenderer._getXTranslation(newConfig.transform);
                dy = this._vmlRenderer._getYTranslation(newConfig.transform);
            }
            this._getVMLHandle().control1 = Math.round(scale * (newConfig.control1X + dx)) + " " + Math.round(scale * (newConfig.control1Y + dy));
        };

        actions.control2X.mask1 = actions.control2Y.mask1 = (actions.control2X.mask1 | actions.control2Y.mask1);
        actions.control2X.mask2 = actions.control2Y.mask2 = (actions.control2X.mask2 | actions.control2Y.mask2);
        actions.control2X.action = actions.control2Y.action = function (oldConfig, newConfig) {
            var scale = this._vmlRenderer.scale, dx = 0, dy = 0;
            if (this._vmlRenderer._isTranslation(newConfig.transform)) {
                dx = this._vmlRenderer._getXTranslation(newConfig.transform);
                dy = this._vmlRenderer._getYTranslation(newConfig.transform);
            }
            this._getVMLHandle().control2 = Math.round(scale * (newConfig.control2X + dx)) + " " + Math.round(scale * (newConfig.control2Y + dy));
        };

        actions.lineWidth.action = function (oldConfig, newConfig) {
            if (this._type != "image") {
                this._getVMLHandle().strokeweight = Math.round(newConfig.lineWidth * this._vmlRenderer.drawPane.zoomLevel) + "px";
            }
        };
        actions.lineOpacity.action = function (oldConfig, newConfig) {
            if (newConfig.stroked && this._type != "image") {
                this._getVMLStrokeHandle().opacity = Math.round(65536 * newConfig.lineOpacity) + "f";
            }
        };
        actions.fillOpacity.action = function (oldConfig, newConfig) {
            if (newConfig.filled) {
                this._getVMLFillHandle().opacity = Math.round(65536 * newConfig.fillOpacity) + "f";
            }
        };
        actions.zIndex.action = function (oldConfig, newConfig) {
            this._getVMLHandle().style.zIndex = newConfig == null ? "" : newConfig.zIndex;
        };
        actions.alt.action = function (oldConfig, newConfig) {
            if (newConfig.alt && oldConfig.alt != newConfig.alt) {
                this._getVMLHandle().alt = newConfig.alt;
            } else if (oldConfig.alt && !newConfig.alt) {
                this._getVMLHandle().removeAttribute("alt");
            }
        };
        actions.src.action = function (oldConfig, newConfig) {
            if (newConfig.src && oldConfig.src != newConfig.src) {
                this._getVMLHandle().src = newConfig.src;
            } else if (oldConfig.src && !newConfig.src) {
                this._getVMLHandle().removeAttribute("src");
            }
        };
        actions.lineColor.action = function (oldConfig, newConfig) {
            if (newConfig.stroked) {
                this._getVMLHandle().strokecolor = newConfig.lineColor;
            }
        };
        actions.linePattern.action = function (oldConfig, newConfig) {
            if (newConfig.stroked && this._type != "image") {
                var linePattern = newConfig.linePattern;
                if (!(isc.isA.String(linePattern) && isc.VMLRenderer._dashstyleRegExp.test(linePattern))) {
                    linePattern = isc.VMLRenderer._$solid;
                }
                this._getVMLStrokeHandle().dashstyle = linePattern;
            }
        };
        actions.startArrow.action = function (oldConfig, newConfig) {
            if (newConfig.stroked && this._type != "image") {
                var startArrow = newConfig.startArrow;
                if (!(isc.isA.String(startArrow) && isc.VMLRenderer._arrowRegExp.test(startArrow))) {
                    startArrow = isc.VMLRenderer._$none;
                }
                this._getVMLStrokeHandle().startarrow = startArrow;
            }
        };
        actions.endArrow.action = function (oldConfig, newConfig) {
            if (newConfig.stroked && this._type != "image") {
                var endArrow = newConfig.endArrow;
                if (!(isc.isA.String(endArrow) && isc.VMLRenderer._arrowRegExp.test(endArrow))) {
                    endArrow = isc.VMLRenderer._$none;
                }
                this._getVMLStrokeHandle().endarrow = endArrow;
            }
        };
        actions.lineCap.action = function (oldConfig, newConfig) {
            if (newConfig.stroked && this._type != "image") {
                var lineCap = newConfig.lineCap;
                if (!(isc.isA.String(lineCap) && isc.VMLRenderer._endcapRegExp.test(lineCap))) {
                    lineCap = "round";
                }
                this._getVMLStrokeHandle().endcap = lineCap;
            }
        };
        actions.fillColor.action = function (oldConfig, newConfig) {
            if (newConfig.filled) {
                var newFillColor = newConfig.fillColor;
                
                this._getVMLHandle().fillcolor = !newFillColor ? "white" : newFillColor;
            }
        };

        actions.textAlign.action = function (oldConfig, newConfig) {
            if (this._type == "text") {
                var textAlign = newConfig.textAlign;
                if (!(isc.isA.String(textAlign) && isc.VMLRenderer._textAlignRegExp.test(textAlign))) {
                    textAlign = "center";
                }
                this._getVMLTextpathHandle().style.textAlign = textAlign;
            }
        };
        actions.fontFamily.action = function (oldConfig, newConfig) {
            if (this._type == "text" && oldConfig.fontFamily != newConfig.fontFamily) {
                this._getVMLTextpathHandle().style.fontFamily = this._vmlRenderer
                        ._selectFromFontFamily("'", false, newConfig.fontFamily);
            }
        };
        actions.fontSize.action = function (oldConfig, newConfig) {
            if (this._type == "text") {
                this._getVMLTextpathHandle().style.fontSize = newConfig.fontSize;
            }
        };
        actions.fontWeight.action = function (oldConfig, newConfig) {
            if (this._type == "text") {
                this._getVMLTextpathHandle().style.fontWeight = newConfig.fontWeight;
            }
        };
        actions.fontStyle.action = function (oldConfig, newConfig) {
            if (this._type == "text") {
                this._getVMLTextpathHandle().style.fontStyle = newConfig.fontStyle;
            }
        };

        actions.contents.action = function (oldConfig, newConfig) {
            if (this._type == "text" &&
                oldConfig.contents && newConfig.contents &&
                oldConfig.contents != newConfig.contents)
            {
                this._getVMLTextpathHandle().string = this._vmlRenderer._normalizeContentLines(newConfig.contents);
            }
        };

        actions.points.action = function (oldConfig, newConfig) {
            if (isc.isAn.Array(newConfig.points) && newConfig.points.length > 0) {
                var buffer = isc.StringBuffer.create(),
                    dx = 0, dy = 0;
                if (this._vmlRenderer._isTranslation(newConfig.transform)) {
                    dx = this._vmlRenderer._getXTranslation(newConfig.transform);
                    dy = this._vmlRenderer._getYTranslation(newConfig.transform);
                }
                this._vmlRenderer._appendPolylinePointsText(
                    buffer, newConfig.points, newConfig.closePolyline, dx, dy);
                
                this._getVMLHandle().points.value = buffer.release(false);
            } else {
                this._getVMLHandle().removeAttribute("points");
            }
        };

        actions._tempBoundingBox.action = function (oldConfig, newConfig) {
            
        };

        actions.stroked.action = function (oldConfig, newConfig) {
            var addStroke = !oldConfig.stroked && newConfig.stroked,
                removeStroke = !addStroke && oldConfig.stroked && !newConfig.stroked,
                vmlHandle;
            if (addStroke) {
                this._getVMLStrokeHandle(newConfig);
                vmlHandle = this._getVMLHandle();
                vmlHandle.stroked = "t";
            } else if (removeStroke) {
                isc.Element.clear(this._getVMLStrokeHandle());
                delete this._vmlStrokeHandle;
                this._getVMLHandle().stroked = "f";
            }
            if (addStroke || removeStroke) {
                var actions = isc.VMLRendererHandle._propertyActions;

                if (addStroke &&
                    ((this._diffFlags1 & actions.lineColor.mask1) |
                     (this._diffFlags2 & actions.lineColor.mask2)) != 0)
                {
                    vmlHandle.strokecolor = newConfig.lineColor;
                }

                // If the <stroke/> subelement was generated or removed here then there is no
                // need to additionally update properties than depend on `stroked`.
                var mask1 = (
                        actions.lineOpacity.mask1 |
                        actions.lineColor.mask1 |
                        actions.linePattern.mask1 |
                        actions.startArrow.mask1 |
                        actions.endArrow.mask1 |
                        actions.lineCap.mask1),
                    mask2 = (
                        actions.lineOpacity.mask2 |
                        actions.lineColor.mask2 |
                        actions.linePattern.mask2 |
                        actions.startArrow.mask2 |
                        actions.endArrow.mask2 |
                        actions.lineCap.mask2);
                this._setMask(mask1, mask2, false);
            }
        };

        actions.filled.action = function (oldConfig, newConfig) {
            var addFill = !oldConfig.filled && newConfig.filled,
                removeFill = oldConfig.filled && !newConfig.filled,
                vmlHandle;
            if (addFill) {
                this._getVMLFillHandle(newConfig);
                vmlHandle = this._getVMLHandle();
                vmlHandle.filled = "t";
            } else if (removeFill) {
                isc.Element.clear(this._getVMLFillHandle());
                delete this._vmlFillHandle;
                this._getVMLHandle().filled = "f";
            }
            if (addFill || removeFill) {
                var actions = isc.VMLRendererHandle._propertyActions;

                if (addFill &&
                    ((this._diffFlags1 & actions.fillColor.mask1)
                         | (this._diffFlags2 & actions.fillColor.mask2)) != 0)
                {
                    vmlHandle.fillcolor = newConfig.fillColor;
                }

                // `fillOpacity`, `fillColor`, and `fillGradient` properties depend on
                // `filled`.  If the <fill/> subelement was generated here or removed here
                // then there is no need to also update those three properties.
                this._setMask(
                    (actions.fillOpacity.mask1 | actions.fillColor.mask1 | actions.fillGradient.mask1),
                    (actions.fillOpacity.mask2 | actions.fillColor.mask2 | actions.fillGradient.mask2),
                    false);
            }
        };

        actions.closePolyline.action = function (oldConfig, newConfig) {
            
        };

        actions.useMatrixFilter.action = function (oldConfig, newConfig) {
            this._regenerateVML();
        };

        actions.transform.action = function (oldConfig, newConfig) {
            var dontCompare = this._dontCompareTransforms,
                globalRotationIsZero = (this._vmlRenderer._globalRotation == 0),
                globalRotationWasZero = (dontCompare ? this._globalRotationWasZero : globalRotationIsZero),
                oldTransform = oldConfig.transform,
                newTransform = newConfig.transform;
            if (dontCompare ||
                (oldTransform != newTransform && !(
                    oldTransform.m00 == newTransform.m00 &&
                    oldTransform.m01 == newTransform.m01 &&
                    oldTransform.m01 == newTransform.m02 &&
                    oldTransform.m10 == newTransform.m10 &&
                    oldTransform.m11 == newTransform.m11 &&
                    oldTransform.m12 == newTransform.m12)))
            {
                var vmlRenderer = this._vmlRenderer,
                    type = this._type,
                    // Whether the shape could be represented as one of VML's predefined shape
                    // elements:
                    predefinedShapeType = (
                        type == "rect" || type == "roundrect" || type == "line" ||
                        type == "polyline" || type == "curve" || type == "oval" ||
                        type == "arc");

                
                var oldIsTranslation = this._vmlRenderer._isTranslation(oldTransform),
                    newIsTranslation = (
                        dontCompare ? oldIsTranslation : this._vmlRenderer._isTranslation(newTransform)),
                    addSkew = (oldIsTranslation && !newIsTranslation),
                    removeSkew = (!oldIsTranslation && newIsTranslation),
                    isImage = (type == "image"),
                    isText = (type == "text"),
                    addTypeAttr = (!isImage && (isText ?
                        removeSkew :
                        newIsTranslation && globalRotationWasZero && !globalRotationIsZero)),
                    removeTypeAttr = (!isImage && (isText ?
                        addSkew :
                        newIsTranslation && !globalRotationWasZero && globalRotationIsZero)),
                    structureChanged = (addSkew || removeSkew || addTypeAttr || removeTypeAttr);
                

                if (// The shape is redrawn whenever it is switched from a predefined shape to
                    // a <shape/>, or vice versa.
                    (predefinedShapeType && structureChanged) ||
                    // Images are always redrawn.
                    type == "image")
                {
                    this._regenerateVML();
                    return;
                }

                // Whether the shape is currently rendered as a predefined shape element
                // instead of a <shape/>:
                var predefinedShape = predefinedShapeType && newIsTranslation && globalRotationIsZero;

                
                if (structureChanged || newIsTranslation) {
                    if (predefinedShape) {
                        
                        this._regenerateVML();
                        return;
                    } else {
                        
                        var actions = isc.VMLRendererHandle._propertyActions;
                        this._setFlag(actions._getPathVML.index, true);
                    }
                }

                // Add or remove the "type" attribute.
                if (addTypeAttr) {
                    this._getVMLHandle().type = (
                        type == "text" ?
                            this._vmlRenderer._globalTextShapeTypeId :
                            this._vmlRenderer._globalShapeTypeId);
                } else if (removeTypeAttr) {
                    this._getVMLHandle().removeAttribute("type");
                }

                // Add, remove, or update the <skew/> subelement.
                if (addSkew) {
                    this._getVMLSkewHandle(newConfig);
                } else if (removeSkew) {
                    isc.Element.clear(this._getVMLSkewHandle());
                    delete this._vmlSkewHandle;
                } else if (!newIsTranslation) {
                    // This is similar to VMLRenderer._writeSkewEl() except that we are
                    // updating the attribute values.
                    var skewHandle = this._getVMLSkewHandle(),
                        info = this._vmlRenderer._getSkewMatrixAndOffset(
                            newTransform, isText);
                    skewHandle.matrix = info.matrix;
                    skewHandle.offset = info.offset;
                }

                
                if (isText && this._zoomLevelChanged) {
                    this._vmlRenderer._updateTextGroup(this.getHandle());
                }
            }
        };

        actions.getBoundingBox.action = function (oldConfig, newConfig) {
            
        };
        actions._normalizeLinearGradient.action = function (oldConfig, newConfig) {
            
        };

        actions._getPathVML.action = function (oldConfig, newConfig) {
            
            var vmlRenderer = this._vmlRenderer,
                prevBuffer = vmlRenderer.buffer,
                buffer = vmlRenderer.buffer = isc.StringBuffer.create(),
                dx = 0, dy = 0;
            if (this._vmlRenderer._isTranslation(newConfig.transform)) {
                dx = this._vmlRenderer._getXTranslation(newConfig.transform);
                dy = this._vmlRenderer._getYTranslation(newConfig.transform);
            }
            var pathBuilder = isc.VMLRenderer._vmlPathBuilder._reset(vmlRenderer, dx, dy);
            newConfig._getPathVML(pathBuilder);
            this._getVMLHandle().path.value = buffer.release(false);
            vmlRenderer.buffer = prevBuffer;
        };

        actions.fillGradient.action = function (oldConfig, newConfig) {
            if (newConfig.filled) {
                var drawPane = this._vmlRenderer.drawPane,
                    oldFillGradient = oldConfig.fillGradient,
                    newFillGradient = newConfig.fillGradient,
                    oldDef = (
                        isc.isA.String(oldFillGradient) ?
                            drawPane.getGradient(oldFillGradient) : oldFillGradient),
                    newDef = (
                        isc.isA.String(newFillGradient) ?
                            drawPane.getGradient(newFillGradient) : newFillGradient);
                if (oldDef != null && isc.isAn.emptyObject(oldDef)) {
                    oldDef = null;
                }
                if (newDef != null && isc.isAn.emptyObject(newDef)) {
                    newDef = null;
                }
                if (oldDef == newDef) {
                    return;
                } else {
                    // Simply replace the <fill/> subelement.
                    var buffer = isc.StringBuffer.create();
                    this._vmlRenderer._writeFillEl(buffer, newConfig, true);
                    this._vmlFillHandle = isc.VMLRendererHandle._replaceVML(
                        this._getVMLFillHandle(), buffer.release(false));

                    
                    this._setMask(
                        (actions.fillOpacity.mask1 | actions.fillColor.mask1),
                        (actions.fillOpacity.mask2 | actions.fillColor.mask2),
                        false);

                    // Set a flag to be checked in flush().
                    this._regeneratedFillEl = true;
                }
            }
        };

        actions.shadow.action = function (oldConfig, newConfig) {
            var addShadow = (!oldConfig.shadow && newConfig.shadow),
                removeShadow = (oldConfig.shadow && !newConfig.shadow),
                replaceShadow = (
                    oldConfig.shadow && newConfig.shadow &&
                    oldConfig.shadow != newConfig.shadow);

            if (addShadow) {
                this._getVMLShadowHandle(newConfig);
            } else if (replaceShadow || removeShadow) {
                var vmlShadowHandle = this._getVMLShadowHandle();
                
                if (replaceShadow) {
                    // Regenerate the <shadow/> VML element.
                    var buffer = isc.StringBuffer.create();
                    this._vmlRenderer._writeShadowEl(buffer, newConfig.shadow, true);
                    this._vmlShadowHandle = isc.VMLRendererHandle._replaceVML(
                        vmlShadowHandle, buffer.release(false));
                } else {
                    isc.Element.clear(vmlShadowHandle);
                    delete this._vmlShadowHandle;
                }
            }
        };

        actions.visibility.action = function (oldConfig, newConfig) {
            this._getVMLHandle().style.visibility = newConfig.visibility;
        };

        var rectToShapeMask1 = (
                actions.left.mask1 | actions.top.mask1 | actions.width.mask1 | actions.height.mask1),
            rectToShapeMask2 = (
                actions.left.mask2 | actions.top.mask2 | actions.width.mask2 | actions.height.mask2),
            roundrectToShapeMask1 = rectToShapeMask1 | actions.arcsize.mask1,
            roundrectToShapeMask2 = rectToShapeMask2 | actions.arcsize.mask2,
            lineToShapeMask1 = (
                actions.fromX.mask1 | actions.fromY.mask1 | actions.toX.mask1 | actions.toY.mask1),
            lineToShapeMask2 = (
                actions.fromX.mask2 | actions.fromY.mask2 | actions.toX.mask2 | actions.toY.mask2),
            polylineToShapeMask1 = (actions.points.mask1 | actions.closePolyline.mask1),
            polylineToShapeMask2 = (actions.points.mask2 | actions.closePolyline.mask2),
            curveToShapeMask1 = (
                lineToShapeMask1 |
                actions.control1X.mask1 | actions.control1Y.mask1 |
                actions.control2X.mask1 | actions.control2Y.mask1),
            curveToShapeMask2 = (
                lineToShapeMask2 |
                actions.control1X.mask2 | actions.control1Y.mask2 |
                actions.control2X.mask2 | actions.control2Y.mask2),
            arcToShapeMask1 = (rectToShapeMask1 | actions.startangle.mask1 | actions.endangle.mask1),
            arcToShapeMask2 = (rectToShapeMask2 | actions.startangle.mask2 | actions.endangle.mask2),
            textShapeMask1 = (
                actions.left.mask1 | actions.top.mask1 | actions.textAlign.mask1 |
                actions.fontFamily.mask1 | actions.fontSize.mask1 | actions.fontWeight.mask1 |
                actions.fontStyle.mask1 | actions.contents.mask1),
            textShapeMask2 = (
                actions.left.mask2 | actions.top.mask2 | actions.textAlign.mask2 |
                actions.fontFamily.mask2 | actions.fontSize.mask2 | actions.fontWeight.mask2 |
                actions.fontStyle.mask2 | actions.contents.mask2);

        isc.VMLRendererHandle.addClassProperties({
            _propertyActionIndex: actionIndex,
            _affectsBoundingBoxMask1: (
                roundrectToShapeMask1 | arcToShapeMask1 | curveToShapeMask1 | polylineToShapeMask1 |
                textShapeMask1 | actions._getPathVML.mask1),
            _affectsBoundingBoxMask2: (
                roundrectToShapeMask2 | arcToShapeMask2 | curveToShapeMask2 | polylineToShapeMask2 |
                textShapeMask2 | actions._getPathVML.mask2),

            _rectToShapeMask1: rectToShapeMask1,
            _rectToShapeMask2: rectToShapeMask2,
            _roundrectToShapeMask1: roundrectToShapeMask1,
            _roundrectToShapeMask2: roundrectToShapeMask2,
            _lineToShapeMask1: lineToShapeMask1,
            _lineToShapeMask2: lineToShapeMask2,
            _polylineToShapeMask1: polylineToShapeMask1,
            _polylineToShapeMask2: polylineToShapeMask2,
            _curveToShapeMask1: curveToShapeMask1,
            _curveToShapeMask2: curveToShapeMask2,
            _ovalToShapeMask1: rectToShapeMask1,
            _ovalToShapeMask2: rectToShapeMask2,
            _arcToShapeMask1: arcToShapeMask1,
            _arcToShapeMask2: arcToShapeMask2,
            _textShapeMask1: textShapeMask1,
            _textShapeMask2: textShapeMask2
        });
        return actions;
    })(),

    _replaceVML : function (vmlHandle, replacementVMLString) {
        var parentElement = vmlHandle.parentElement,
            fragment = document.createDocumentFragment(),
            newVmlHandle = isc.Element.insertAdjacentHTML(
                fragment, "afterbegin", replacementVMLString, true);
        parentElement.replaceChild(newVmlHandle, vmlHandle);
        
        if (isc.Browser.isIE8) {
            var prevSibling = newVmlHandle.previousSibling;
            if (prevSibling == null) {
                newVmlHandle.outerHTML = newVmlHandle.outerHTML;
                newVmlHandle = parentElement.firstChild;
            } else {
                newVmlHandle.outerHTML = newVmlHandle.outerHTML;
                newVmlHandle = prevSibling.nextSibling;
            }
        }
        return newVmlHandle;
    }
});

isc.VMLRendererHandle.addProperties({
    _type: null,
    _vmlRenderer: null,
    _id: "",
    _config: null,
    _newConfig: null,
    _diffFlags1: 0,
    _diffFlags2: 0,

    addPropertiesOnCreate: false,
    init : function (type, vmlRenderer, id, config) {
        
        this._type = type;
        this._vmlRenderer = vmlRenderer;
        this._id = id;
        this._config = isc.VMLRendererHandle._shapeConfigDuplicate(config, true);
        this._newConfig = isc.VMLRendererHandle._shapeConfigDuplicate(this._config, false);
    },

    setProperty : function (propertyName, newValue, touchTransformHack, globalRotationWasZero, zoomLevelChanged) {
        
        if (this._type == "group" && propertyName != "zIndex") {
            // Except for zIndex, <group/>s are constant for now.
            return this;
        }

        var actions = isc.VMLRendererHandle._propertyActions,
            propertyObj = actions[propertyName];
        if (propertyObj != null) {
            var oldConfig = this._config, newConfig = this._newConfig,
                isPath = (propertyName == "_getPathVML");

            if (isPath || newConfig[propertyName] != newValue) {
                if (!isPath) {
                    if (propertyName == "transform") {
                        
                        if (touchTransformHack) {
                            this._dontCompareTransforms = true;
                            this._globalRotationWasZero = globalRotationWasZero;

                            
                            this._zoomLevelChanged = zoomLevelChanged;
                        } else {
                            this._dontCompareTransforms = false;
                            newConfig[propertyName] = isc.VMLRendererHandle._duplicateTransform(newValue);
                        }
                    } else if (propertyName == "points") {
                        newConfig[propertyName] = isc.VMLRendererHandle._duplicatePoints(newValue);
                    } else {
                        newConfig[propertyName] = newValue;
                    }
                }
                this._setFlag(
                    propertyObj.index, (isPath || (oldConfig[propertyName] != newValue)));
            }
        }
        return this;
    },

    _setFlag : function (j, flag) {
        
        if (j < 30) {
            if (flag) {
                this._diffFlags1 |= (1 << j);
            } else {
                this._diffFlags1 &= ~(1 << j);
            }
        } else {
            j -= 30;
            if (flag) {
                this._diffFlags2 |= (1 << j);
            } else {
                this._diffFlags2 &= ~(1 << j);
            }
        }
    },
    // Sets multiple flags:
    _setMask : function (mask1, mask2, flag) {
        
        if (flag) {
            this._diffFlags1 |= mask1;
            this._diffFlags2 |= mask2;
        } else {
            this._diffFlags1 &= ~mask1;
            this._diffFlags2 &= ~mask2;
        }
    },

    
    _flushFirstProperties: [
        "useMatrixFilter", "transform", "arcsize", "startangle", "endangle", "stroked",
        "filled", "fillGradient"],

    
    flush : function () {
        var actions = isc.VMLRendererHandle._propertyActions,
            oldFlags1 = this._diffFlags1,
            oldFlags2 = this._diffFlags2,
            oldConfig = this._config,
            newConfig = this._newConfig;

        if (this._type == "text") {
            
            var mask1 = isc.VMLRendererHandle._textShapeMask1;
                mask2 = isc.VMLRendererHandle._textShapeMask2;
            if (((this._diffFlags1 & mask1) | (this._diffFlags2 & mask2)) != 0) {
                this._setMask(
                    actions.left.mask1 | actions.top.mask1,
                    actions.left.mask2 | actions.top.mask2,
                    false);
                this._setFlag(actions._getPathVML.index, true);
            }
        } else if (!this._vmlRenderer._isTranslation(newConfig.transform)) {
            
            var mask1 = 0, mask2 = 0;
            switch (this._type) {
            case "roundrect":
                mask1 = isc.VMLRendererHandle._roundrectToShapeMask1;
                mask2 = isc.VMLRendererHandle._roundrectToShapeMask2;
                // no break
            case "rect":
            case "image":
                mask1 |= isc.VMLRendererHandle._rectToShapeMask1;
                mask2 |= isc.VMLRendererHandle._rectToShapeMask2;
                break;
            case "line":
                mask1 = isc.VMLRendererHandle._lineToShapeMask1;
                mask2 = isc.VMLRendererHandle._lineToShapeMask2;
                break;
            case "polyline":
                mask1 = isc.VMLRendererHandle._polylineToShapeMask1;
                mask2 = isc.VMLRendererHandle._polylineToShapeMask2;
                break;
            case "curve":
                mask1 = isc.VMLRendererHandle._curveToShapeMask1;
                mask2 = isc.VMLRendererHandle._curveToShapeMask2;
                break;
            case "arc":
                mask1 = isc.VMLRendererHandle._arcToShapeMask1;
                mask2 = isc.VMLRendererHandle._arcToShapeMask2;
                // no break
            case "oval":
                mask1 |= isc.VMLRendererHandle._ovalToShapeMask1;
                mask2 |= isc.VMLRendererHandle._ovalToShapeMask2;
                break;
            }

            if ((mask1 | mask2) != 0) { // i.e. matched on a case above
                if (((this._diffFlags1 & mask1) | (this._diffFlags2 & mask2)) != 0) {
                    this._setMask(mask1, mask2, false);
                    if (this._type == "image") {
                        this._setFlag(actions.transform.index, true);
                        this._dontCompareTransforms = true;
                    } else {
                        this._setFlag(actions._getPathVML.index, true);
                    }
                }
            }
        }

        // Temporarily hide the shape while its properties are being updated.
        if (oldConfig.visibility == "visible") {
            var visibilityAction = actions.visibility,
                visibilityChanged = (
                    ((this._diffFlags1 & visibilityAction.mask1) |
                       (this._diffFlags2 & visibilityAction.mask2)) != 0),
                newVisibility = newConfig.visibility;

            if (!visibilityChanged) {
                newConfig.visibility = "hidden";
            }
            visibilityAction.action.call(this, oldConfig, newConfig);
            if (visibilityChanged) {
                this._diffFlags1 &= ~visibilityAction.mask1;
                this._diffFlags2 &= ~visibilityAction.mask2;
            } else {
                // Schedule this temporary hiding of the shape to be undone at the end.
                oldConfig.visibility = "hidden";
                newConfig.visibility = newVisibility;
                this._diffFlags1 |= visibilityAction.mask1;
                this._diffFlags2 |= visibilityAction.mask2;
            }
        }

        // Some of the properties need to be updated first.
        this._regeneratedVML = false;
        var flushFirst = this._flushFirstProperties;
        for (var i = 0, n = flushFirst.length; i < n; ++i) {
            var action = actions[flushFirst[i]];
            if (((this._diffFlags1 & action.mask1) | (this._diffFlags2 & action.mask2)) != 0) {
                action.action.call(this, oldConfig, newConfig);
                this._diffFlags1 &= ~action.mask1;
                this._diffFlags2 &= ~action.mask2;
            }
        }

        
        var mask1 = actions.closePolyline.mask1,
            mask2 = actions.closePolyline.mask2;
        if ((this._diffFlags1 & mask1) | (this._diffFlags2 & mask2) != 0) {
            this._setFlag(actions.closePolyline.index, false);
            this._setFlag(actions.points.index, true);
        }

        
        if (newConfig.filled && !(this._regeneratedVML || this._regeneratedFillEl)) {
            
            var mask1 = isc.VMLRendererHandle._affectsBoundingBoxMask1,
                mask2 = isc.VMLRendererHandle._affectsBoundingBoxMask2,
                fillGradient = newConfig.fillGradient,
                def = (
                    isc.isA.String(fillGradient) ?
                        this._vmlRenderer.drawPane.getGradient(fillGradient) : fillGradient),
                simpleOrLinear = (
                    def != null &&
                    !isc.isAn.emptyObject(def) &&
                    (isc.DrawItem._isSimpleGradient(def) || isc.DrawItem._isLinearGradient(def)));

            if (simpleOrLinear &&
                ((this._diffFlags1 & mask1) | (this._diffFlags2 & mask2)) != 0)
            {
                var buffer = isc.StringBuffer.create();
                this._vmlRenderer._writeFillEl(buffer, newConfig, true);
                this._vmlFillHandle = isc.VMLRendererHandle._replaceVML(
                    this._getVMLFillHandle(), buffer.release(false));
                this._setMask(
                    (actions.fillOpacity.mask1 | actions.fillColor.mask1 | actions.fillGradient.mask1),
                    (actions.fillOpacity.mask2 | actions.fillColor.mask2 | actions.fillGradient.mask2),
                    false);
            }
        }

        // Update the remaining properties.
        var actionIndex = isc.VMLRendererHandle._propertyActionIndex,
            numProperties = actionIndex.length,
            flags1 = this._diffFlags1,
            flags2 = this._diffFlags2;
        for (var i = 0; !(flags1 == 0 && flags2 == 0) && i < numProperties; ++i) {
            var action = actionIndex[i];
            if (((flags1 & action.mask1) | (flags2 & action.mask2)) != 0) {
                action.action.call(this, oldConfig, newConfig);
                flags1 &= ~action.mask1;
                flags2 &= ~action.mask2;
            }
        }

        // Copy the new configuration onto the current configuration.
        for (var i = 0; !(oldFlags1 == 0 && oldFlags2 == 0) && i < numProperties; ++i) {
            var action = actionIndex[i];
            if (((oldFlags1 & action.mask1) | (oldFlags2 & action.mask2)) != 0) {
                var prop = action.property;
                oldConfig[prop] = newConfig[prop];
                oldFlags1 &= ~action.mask1;
                oldFlags2 &= ~action.mask2;
            }
        }

        this._diffFlags1 = this._diffFlags2 = 0;
        this._dontCompareTransforms = false;
    },

    _regenerateVML : function () {
        var vmlRenderer = this._vmlRenderer,
            prevBuffer = vmlRenderer.buffer,
            buffer = vmlRenderer.buffer = isc.StringBuffer.create();
        vmlRenderer[this._type](this._id, this._newConfig);
        var newVmlHandle = isc.VMLRendererHandle._replaceVML(
            this.getHandle(), buffer.release(false));
        if (this._type == "text" || (
                this._type == "image" &&
                !this._newConfig.useMatrixFilter &&
                !this._vmlRenderer._usePredefinedShape(this._newConfig.transform)))
        {
            this._vmlHandle = newVmlHandle.firstChild;
        } else {
            this._vmlHandle = newVmlHandle;
        }
        vmlRenderer.buffer = prevBuffer;
        this._diffFlags1 = this._diffFlags2 = 0;
        delete this._vmlStrokeHandle;
        delete this._vmlFillHandle;
        delete this._vmlShadowHandle;
        delete this._vmlSkewHandle;
        // Set a flag to be checked in flush().
        this._regeneratedVML = true;
    },

    getHandle : function () {
        var vmlHandle = this._getVMLHandle();
        if (this._type == "text" || (
                this._type == "image" &&
                !this._config.useMatrixFilter &&
                !this._vmlRenderer._usePredefinedShape(this._config.transform)))
        {
            vmlHandle = vmlHandle.parentElement;
            
        }
        return vmlHandle;
    },

    _getVMLHandle : function () {
        if (this._vmlHandle != null) return this._vmlHandle;
        return (this._vmlHandle = isc.Element.get(this._id));
    },

    _findVMLSubelementHandleOutput: {
        found: false,
        handle: null,
        where: "afterbegin"
    },
    _vmlSubelementOrder: [
        isc.VMLRenderer._$STROKE,
        isc.VMLRenderer._$FILL,
        isc.VMLRenderer._$SHADOW,
        isc.VMLRenderer._$SKEW,
        isc.VMLRenderer._$TEXTPATH,
        isc.VMLRenderer._$PATH],
    _findVMLSubelementHandle : function (vmlHandle, which) {
        var order = this._vmlSubelementOrder;
        
        var n = order.length,
            output = this._findVMLSubelementHandleOutput,
            whichIndex = order.indexOf(which);
        for (var i = 0, prevHandle = vmlHandle, handle = vmlHandle.firstChild; i < n; ++i) {
            var index = (handle == null ? whichIndex + 1 : order.indexOf(handle.tagName));
            
            if (index > whichIndex) {
                output.found = false;
                output.handle = prevHandle;
                output.where = (i == 0 ? "afterbegin" : "afterend");
                return output;
            } else if (index == whichIndex) {
                output.found = true;
                output.handle = handle;
                return output;
            } else {
                prevHandle = handle;
                handle = handle.nextSibling;
            }
        }
        
    },

    _getVMLStrokeHandle : function (config) {
        config = config || this._config;
        
        var vmlStrokeHandle = this._vmlStrokeHandle;
        if (vmlStrokeHandle != null) return vmlStrokeHandle;
        var vmlHandle = this._getVMLHandle();
        if (vmlHandle == null) return null;

        var info = this._findVMLSubelementHandle(vmlHandle, isc.VMLRenderer._$STROKE);
        if (info.found) {
            vmlStrokeHandle = info.handle;
        } else {
            // If not found, need to insert a <STROKE> element.
            var buffer = isc.StringBuffer.create();
            this._vmlRenderer._writeStrokeEl(buffer, config, true);
            if (buffer.getArray().length > 0) {
                vmlStrokeHandle = isc.Element.insertAdjacentHTML(
                    info.handle, info.where, buffer.release(false), true);
            }
        }
        return (this._vmlStrokeHandle = vmlStrokeHandle);
    },

    _getVMLFillHandle : function (config) {
        config = config || this._config;
        
        var vmlFillHandle = this._vmlFillHandle;
        if (vmlFillHandle != null) return vmlFillHandle;
        var vmlHandle = this._getVMLHandle();
        if (vmlHandle == null) return null;

        var info = this._findVMLSubelementHandle(vmlHandle, isc.VMLRenderer._$FILL);
        if (info.found) {
            vmlFillHandle = info.handle;
        } else {
            var buffer = isc.StringBuffer.create();
            this._vmlRenderer._writeFillEl(buffer, config, true);
            if (buffer.getArray().length > 0) {
                vmlFillHandle = isc.Element.insertAdjacentHTML(
                    info.handle, info.where, buffer.release(false), true);
            }
        }
        return (this._vmlFillHandle = vmlFillHandle);
    },

    _getVMLShadowHandle : function (config) {
        config = config || this._config;
        
        var vmlShadowHandle = this._vmlShadowHandle;
        if (vmlShadowHandle != null) return vmlShadowHandle;
        var vmlHandle = this._getVMLHandle();
        if (vmlHandle == null) return null;

        var info = this._findVMLSubelementHandle(vmlHandle, isc.VMLRenderer._$SHADOW);
        if (info.found) {
            vmlShadowHandle = info.handle;
        } else {
            var buffer = isc.StringBuffer.create();
            this._vmlRenderer._writeShadowEl(buffer, config.shadow, true);
            vmlShadowHandle = isc.Element.insertAdjacentHTML(
                info.handle, info.where, buffer.release(false), true);
        }
        return (this._vmlShadowHandle = vmlShadowHandle);
    },

    _getVMLSkewHandle : function (config) {
        config = config || this._config;
        
        var vmlSkewHandle = this._vmlSkewHandle;
        if (vmlSkewHandle != null) return vmlSkewHandle;
        var vmlHandle = this._getVMLHandle();
        if (vmlHandle == null) return null;

        var info = this._findVMLSubelementHandle(vmlHandle, isc.VMLRenderer._$SKEW);
        if (info.found) {
            vmlSkewHandle = info.handle;
        } else {
            var buffer = isc.StringBuffer.create();
            this._vmlRenderer._writeSkewEl(buffer, config, true, (this._type == "text"));
            
            vmlSkewHandle = isc.Element.insertAdjacentHTML(
                info.handle, info.where, buffer.release(false), true);
        }
        return (this._vmlSkewHandle = vmlSkewHandle);
    },

    _getVMLTextpathHandle : function (config) {
        config = config || this._config;
        
        var vmlTextpathHandle = this._vmlTextpathHandle;
        if (vmlTextpathHandle != null) return vmlTextpathHandle;
        var vmlHandle = this._getVMLHandle();
        if (vmlHandle == null) return null;

        var info = this._findVMLSubelementHandle(vmlHandle, isc.VMLRenderer._$TEXTPATH);
        if (info.found) {
            vmlTextpathHandle = info.handle;
        } else {
            var buffer = isc.StringBuffer.create();
            this._vmlRenderer._writeTextpathEl(buffer, config, true);
            
            vmlTextpathHandle = isc.Element.insertAdjacentHTML(
                info.handle, info.where, buffer.release(false), true);
        }
        return (this._vmlTextpathHandle = vmlTextpathHandle);
    }
});


isc.defineClass("SVGStringConversionContext").addClassProperties({
    _$xlink: "xlink",

    _nextConversionContextNum: 0
});
isc.SVGStringConversionContext.addProperties({
    _nextSvgDefNum: 1,

    //> @attr svgStringConversionContext.printForExport (Boolean : true : IR)
    // Whether the conversion to SVG source is being requested for print export.
    //<
    printForExport: true,

    
    _printForServerExport: true,

    xlinkPrefix: isc.SVGStringConversionContext._$xlink,


init : function () {
    this.Super("init", arguments);
    this.defSvgStrings = {};
    this.useGradientIDs = {};
    this._conversionContextNum = isc.SVGStringConversionContext._nextConversionContextNum++;
    if (this.gradientIdProperty == null) {
        this.gradientIdProperty = "_conversionContext" + this._conversionContextNum + "_"+"gradientId";
    }
},

getNextSvgDefID : function () {
    return "_conversionContext" + this._conversionContextNum + "_"+"def" + (this._nextSvgDefNum++);
}

});

isc.defineClass("VMLStringConversionContext").addMethods({
    init : function () {
        this.Super("init", arguments);
        this.drawLabelsAccumulator = [];
    }
});

//------------------------------------------------------------------------------------------
//> @class DrawPane
//
// A DrawPane is a container for drawing bitmap and vector graphics using browser's built-in
// freeform drawing capabilities.  These include the HTML5 <code>&lt;canvas&gt;</code> tag and
// <code>SVG (Scalable Vector Graphics)</code> where available, and the <code>VML (Vector Markup
// Language)</code> for legacy browsers (Internet Explorer 8 and earlier).
// <p>
// To draw in a <code>DrawPane</code> you create +link{DrawLine}s, +link{DrawOval}s,
// +link{DrawPath}s and other +link{DrawItem}-based components, and place them in the
// <code>DrawPane</code> via +link{drawPane.drawItems} or add them incrementally via
// +link{drawPane.addDrawItem()}.
// <p>
// <code>DrawItems</code> support a variety of common features, such as 
// +link{Gradient,gradient fills}, +link{drawItem.startArrow,arrowheads}, events such as
// +link{drawItem.click,click()} and built-in +link{drawItem.knobs,control knobs} for end user
// resizing and manipulation of shapes.
// <p>
// Common shapes such as +link{DrawRect,rectangles}, +link{DrawOval,ovals} and
// +link{DrawTriangle,triangles} have dedicated DrawItem subclasses.  For other shapes,
// consider:
// <ul>
// <li> +link{DrawPath} - a multi-segment line with straight segments, defined by a series
//      of +link{drawPath.points,points}
// <li> +link{DrawPolygon} - a closed shape with straight sides, defined by a series of
//      +link{drawPolygon.points,points}
// <li> +link{DrawShape} - a multi-segment line or closed shape whose sides can be defined by a
//      series of commands, including curved arcs
// </ul>
//
// <smartgwt><p><b>NOTE:</b>To use the Drawing subsystem, include the Drawing module
// in your application by including <code>&lt;inherits name="com.smartgwt.Drawing"/&gt;</code>
// in your GWT module XML.</p></smartgwt>
//
// <h3>Note on Coordinate Systems</h3>
// There are three different coordinate systems involved when a DrawItem is drawn onto a DrawPane:
// <ul>
// <li>The "local coordinate system" for a DrawItem refers to the Cartesian coordinate system
//     in which dimensional and positional values are interpreted.  For example, when a
//     +link{DrawRect} is configured with left:20, top:30, width:200, and height:100, the
//     DrawRect represents a rectangle from (20, 30) to (220, 130) in its local coordinate
//     system.  For this same DrawRect, +link{DrawRect.top,top} is going to be 30 even if
//     the shape is scaled by 3x, such that the (transformed) top coordinate in the drawing
//     coordinate system actually lies outside the visible region of the DrawPane.  Similarly,
//     no matter what rotation is applied, +link{DrawRect.top,top} will continue to be 30.
//     <p>
//     Use +link{DrawItem.getBoundingBox()} to obtain the bounding box of the item in local
//     coordinates.  Subclass properties also typically provide data in the local coordinate
//     system, such as +link{DrawRect.left}, +link{DrawRect.top}, +link{DrawRect.width},
//     +link{DrawRect.height}, +link{DrawPath.points}, and +link{DrawTriangle.points}.
//     <p>
//     There is a local coordinate system for each DrawItem.</li>
// <li>The "drawing coordinate system" refers to the Cartesian coordinate system shared by
//     all DrawItems after their local transforms, such as +link{DrawItem.scale} or
//     +link{DrawItem.rotation}, have been applied.
//     <p>
//     Since +link{DrawGroup}s pass through applied transforms to the underlying items,
//     +link{DrawGroup} properties such as +link{DrawGroup.left}, +link{DrawGroup.top},
//     +link{DrawGroup.width}, and +link{DrawGroup.height}, represent coordinates in the drawing
//     coordinate system, as does therefore +link{DrawGroup.getBoundingBox()}.  The APIs
//     +link{DrawPane.getDrawingPoint()}, +link{DrawPane.getDrawingX()}, and
//     +link{DrawPane.getDrawingY()}, also return drawing coordinates.
//     <p>
//     For DrawItems with no local transforms, the drawing coordinate system is identical to
//     the local coordinate system.</li>
// <li>The "global coordinate system" refers to the drawing coordinate system with global
//     DrawPane transforms +link{DrawPane.translate}, +link{DrawPane.zoomLevel} and
//     +link{DrawPane.rotation} applied.
//     <p>
//     Use +link{DrawItem.getResizeBoundingBox()} to obtain the bounding box of a
//     +link{DrawItem} in global coordinates.  The APIs +link{DrawItem.getPageLeft()} and 
//     +link{DrawItem.getPageTop()} reflect global coordinates rounded to the nearest pixel and
//     offset by the page-relative coordinates of the +link{DrawPane}'s top left corner.
//     (See for example +link{Canvas.getPageLeft()} and +link{Canvas.getPageTop()}.)
//     <p>
//     With the default global transforms, the global coordinate system is identical to the
//     drawing coordinate system.</li>
// </ul>
// <p>
// The view port of the DrawPane is the rectangle in the global coordinate system from (0, 0)
// that is as wide as the DrawPane's +link{Canvas.getInnerContentWidth(),inner content width}
// and as high as the DrawPane's +link{Canvas.getInnerContentHeight(),inner content height}.
// Note: In the case of a +link{FacetChart} showing a +link{FacetChart.canZoom,zoom chart},
// the view port height is decreased by the height of the zoom chart.
// <p>
// One other coordinate system in use by a DrawPane when +link{DrawPane.canDragScroll,drag-scrolling}
// is enabled is the "viewbox coordinate system". The viewbox coordinate system is the drawing
// coordinate system with the +link{DrawPane.translate} and +link{DrawPane.zoomLevel} transforms
// applied.
//
// @inheritsFrom Canvas
// @treeLocation Client Reference/Drawing
// @visibility drawing
//<
//------------------------------------------------------------------------------------------



isc.defineClass("DrawPane", "Canvas");

isc.DrawPane.addProperties({

_radPerDeg: isc.Math._radPerDeg,

overflow: "hidden",

//> @type DrawingType
// The type of drawing back-end used by a +link{DrawPane} to draw its +link{DrawPane.drawItems,draw items}
// on screen.
//
// <h3>SVG and bitmap tradeoffs</h3>
// <ul style="list-style-type: lower-alpha;">
// <li>SVG is best supported in IE9+ and Opera 12.16 and earlier, but may also be used in Chrome,
// Opera 15+, Firefox 4+, and Safari.
// <li>SVG tends to be higher quality, especially when it comes to text and drawing on high-DPI
// displays and mobile devices.
// <li>+link{DrawItem.zIndex}, +link{DrawItem.bringToFront()}, and +link{DrawItem.sendToBack()}
// are not supported on iOS devices in bitmap drawing mode due to platform limitations. These
// APIs are supported on iOS in SVG drawing mode.
// <li>Incremental updates to just a few elements of a drawing are faster in SVG, whereas
// whole drawing refreshes are faster in bitmap mode.
// <li>Browsers have limits on the maximum dimensions or area of the &lt;canvas&gt; element
// used in bitmap drawing. The limits on a &lt;canvas&gt; imposed by the browser translate to
// the same limits on the dimensions or area of a <code>DrawPane</code> using bitmap drawing.
// Internet Explorer, for example, limits the width and height of a &lt;canvas&gt; to 8192 pixels:
// +externalLink{http://msdn.microsoft.com/en-us/library/ie/ff975062(v=vs.85).aspx}; therefore,
// in IE using bitmap drawing, a <code>DrawPane</code> can be at most 8192&times;8192 in size.
// To make larger drawings, you can either switch to SVG drawing, use multiple <code>DrawPane</code>s,
// or enable +link{DrawPane.canDragScroll,drag-scrolling}.
// <li>In bitmap drawing, each pixel uses around 4 to 8 bytes of memory. Large bitmap drawings
// can therefore use a lot of memory. A 4000&times;2000 bitmap drawing will use around 31 to 61
// Megabytes of memory.<br>
// Note: To minimize memory use when using bitmap drawing for a large drawing, it may be useful
// to employ +externalLink{http://en.wikipedia.org/wiki/Slicing_(interface_design),slicing} if
// possible. For example, if the drawing is mostly a solid color except for content located at
// a few small-area places on screen, a +link{Canvas} can be created with the solid background
// color and one <code>DrawPane</code> can be created for each content area. Each <code>DrawPane</code>
// is +link{Canvas.addChild,added as a child} to the <code>Canvas</code>. The +link{Canvas.left,left}
// and +link{Canvas.top,top} properties of the <code>DrawPane</code>s are used to absolutely-position
// them within the <code>Canvas</code> parent.
// </ul>
//
// @value "svg" Use Scalable Vector Graphics (SVG). SVG is a W3C standard supported by IE9+,
// Chrome, Firefox 4+, Safari, and Opera.
// @value "bitmap" Use an HTML5 &lt;canvas&gt; element. "bitmap" drawing is supported by IE9+,
// Chrome, Firefox, Safari, and Opera.
// @value "vml" Use Vector Markup Language (VML). VML is a deprecated vector graphics technology
// supported only by Internet Explorer 6 through 9. In IE 6, 7, and 8, "vml" drawing is the only
// supported drawing back-end.
// @visibility drawing
//<


//> @attr drawPane.drawingType (DrawingType : based on browser : IRA)
// Which type of drawing back-end should be used by this <code>DrawPane</code>? A default drawing
// back-end is automatically selected based on the browser.
// <p>
// The <code>drawingType</code> can only be set to a drawing back-end type that is supported
// by the browser. It is provided for cases where the browser supports more than one drawing
// back-end. See the +link{type:DrawingType} documentation for the supported drawing back-ends
// and the list of browsers that support each type of drawing back-end.
// @visibility drawing
//<
drawingType: isc.Browser.defaultDrawingType,

//> @attr drawPane.drawingWidth (int : 1000 : IR)
// When +link{DrawPane.canDragScroll,canDragScroll} is enabled, this is the width of the area
// in viewbox coordinates that can be accessed through drag-scrolling.
// @example ZoomAndPan
// @visibility drawing
//<
drawingWidth: 1000,

//> @attr drawPane.drawingHeight (int : 1000 : IR)
// When +link{DrawPane.canDragScroll,canDragScroll} is enabled, this is the height of the area
// in viewbox coordinates that can be accessed through drag-scrolling.
// @example ZoomAndPan
// @visibility drawing
//<
drawingHeight: 1000,

//> @attr drawPane.precision (int : 1 : IRA)
// The minimum number of floating-point digits of coordinates that will be respected when the
// drawing commands are executed. This setting only has an effect in Internet Explorer 6-8.
//<
precision: 1,

//> @attr drawPane.canDragScroll (boolean : false : IR)
// Can the user drag-scroll the DrawPane?
// @see DrawPane.drawingWidth
// @see DrawPane.drawingHeight
// @example ZoomAndPan
// @visibility drawing
//<
canDragScroll: false,

dragAppearance:"none",

//> @attr drawPane.dragScrollCursor (Cursor : "all-scroll" : IRW)
// Cursor to switch to when +link{DrawPane.canDragScroll,canDragScroll} is true.
//<
dragScrollCursor: "all-scroll",

// allows the DrawPane to be placed above other widgets and not interfere with finding drop
// targets
isMouseTransparent: true, 

//> @attr drawPane.rotation (float : 0 : IRW)
// Rotation in degrees for the <code>DrawPane</code> as a whole about the center of the
// <code>DrawPane</code>. The positive direction corresponds to clockwise rotation (for example,
// 45 is rotation clockwise by 45 degrees and -10 is rotation counterclockwise by 10 degrees).
//
// @visibility drawing
//<
rotation: 0,

//> @attr drawPane.zoomLevel (float : 1 : IRW)
// Zoom for the <code>DrawPane</code> as a whole, where 1 is normal size.
//
// @visibility drawing
//<
zoomLevel: 1,

//> @attr drawPane.translate (Array[] of int : null : IR)
// Global translation. This array has two numbers. The first number is the X translation amount
// in pixels and the second number is the Y translation amount in pixels.
// @visibility drawing
//<
translate: null,

//> @attr drawPane.gradients (Array of Gradient : null : IR)
// Array of gradients that can be referenced by DrawItems placed on this DrawPane.
// Each gradient must have an ID assigned to be used for reference.
//
// @visibility drawing
//<

// Do we support fractional coords?
supportsFractionalCoordinates : function () {
    return true;
},

//> @attr drawPane.drawItems (Array of DrawItem : null : IR)
// Array of DrawItems to initially display in this DrawPane.
// @visibility drawing
//<


//canvasItems - array of Canvii that should be zoomed and panned with this DrawPane

//> @object ColorStop
// An object containing properties that is used in Gradient types
//
// @treeLocation Client Reference/Drawing/Gradients
// @visibility drawing
//<
//> @attr colorStop.offset (float: 0.0: IR) 
// The relative offset for the color.
//
// @visibility drawing
//<
//> @attr colorStop.opacity (float: 1.0: IR) 
// 0 is transparent, 1 is fully opaque
//
// @visibility drawing
//<
//> @attr colorStop.color (CSSColor: null: IR) 
// eg #ff0000 or red or rgb(255,0,0)
//
// @visibility drawing
//<

//> @object Gradient
// Defines a simple gradient vertical gradient between +link{gradient.startColor,two}
// +link{gradient.endColor,colors}, or using +link{gradient.colorStops,colorStops}.  See
// +link{SimpleGradient}, +link{LinearGradient} and +link{RadialGradient} for further
// properties to define more advanced gradients.
//
// @treeLocation Client Reference/Drawing/Gradients
// @visibility drawing
//<
//> @attr gradient.id (identifier : null : IR)
// Identifier which can be used by one or more DrawItems when gradient is assigned
// to +link{drawPane.gradients}. The ID property is optional when gradient is assigned directly
// to a DrawItem.
// <p>
// The ID must be unique within DrawPane.gradients if defined.
// @visibility drawing
//<
//> @attr gradient.colorStops (Array of ColorStop: null: IR)
// An array of color stops for this gradient.
//
// @visibility drawing
//<
//> @attr gradient.startColor (CSSColor : null : IR)
// A start color for the gradient. If both startColor and +link{Gradient.endColor,endColor}
// are set then +link{Gradient.colorStops,colorStops} is ignored.
//
// @visibility drawing
//<
//> @attr gradient.endColor (CSSColor : null : IR)
// An end color for the gradient. If both +link{Gradient.startColor,startColor} and endColor
// are set then +link{Gradient.colorStops,colorStops} is ignored.
//
// @visibility drawing
//<

//> @object SimpleGradient
// Definition of a simple linear gradient defined by 2 colors and a +link{SimpleGradient.direction,direction}.
//
// @inheritsFrom Gradient
// @treeLocation Client Reference/Drawing/Gradients
// @visibility drawing
//<
//> @attr simpleGradient.direction (float : 0.0 : IR)
// Angle of the direction vector in degrees.  The default of 0.0 causes the gradient to sweep
// from the start color on the left to the end color on the right.  Positive direction angles
// correspond to clockwise rotations of the default gradient.
// @visibility drawing
//<
//> @attr simpleGradient.startColor
// The start color of the gradient.
//
// @include Gradient.startColor
// @visibility drawing
//<
//> @attr simpleGradient.endColor
// The end color of the gradient.
//
// @include Gradient.endColor
// @visibility drawing
//<

//> @object LinearGradient
// Definition of a linear gradient between two points, (+link{LinearGradient.x1,x1}, +link{LinearGradient.y1,y1})
// and (+link{LinearGradient.x2,x2}, +link{LinearGradient.y2,y2}).
//
// @inheritsFrom Gradient
// @treeLocation Client Reference/Drawing/Gradients
// @visibility drawing
//<
//> @attr linearGradient.x1 (String : null : IR)
// X coordinate of the start point. This can be a number or a percentage of the width of the
// bounding box of the DrawItem to which it is applied.
//
// @visibility drawing
//<
//> @attr linearGradient.y1 (String : null : IR)
// Y coordinate of the start point. This can be a number or a percentage of the height of the
// bounding box of the DrawItem to which it is applied.
//
// @visibility drawing
//<
//> @attr linearGradient.x2 (String : null : IR)
// X coordinate of the end point. This can be a number or a percentage of the width of the
// bounding box of the DrawItem to which it is applied.
//
// @visibility drawing
//<
//> @attr linearGradient.y2 (String : null : IR)
// Y coordinate of the end point. This can be a number or a percentage of the height of the
// bounding box of the DrawItem to which it is applied.
//
// @visibility drawing
//<

//> @object RadialGradient
// Definition of a radial gradient.
//
// @inheritsFrom Gradient
// @treeLocation Client Reference/Drawing/Gradients
// @visibility drawing
//<
//> @attr radialGradient.cx (String: null: IR) 
// x coordinate of outer radial
//
// @visibility drawing
//<
//> @attr radialGradient.cy (String: null: IR) 
// y coordinate of outer radial
//
// @visibility drawing
//<
//> @attr radialGradient.r  (String: null: IR) 
// radius
//
// @visibility drawing
//<
//> @attr radialGradient.fx (String: null: IR) 
// x coordinate of inner radial
//
// @visibility drawing
//<
//> @attr radialGradient.fy (String: 0: IR) 
// y coordinate of inner radial
//
// @visibility drawing
//<

//> @object Shadow
//A class used to define a shadow used in a Draw&lt;Shape&gt; Types.
//
// @example drawingShadows
// @treeLocation Client Reference/Drawing
// @visibility drawing
//<
//> @attr shadow.color (CSSColor: black: IR) 
//
//@visibility drawing
//<
//> @attr shadow.blur (int: 10: IR) 
//
//@visibility drawing
//<
//> @attr shadow.offset (Point: [0,0]: IR) 
//
//@visibility drawing
//<

createQuadTree : function () {
    this.quadTree = isc.QuadTree.create({
        depth: 0,
        maxDepth: 50,
        maxChildren: 1
    });
    this.quadTree.bounds = {x:0,y:0,width:this._getViewPortWidth(),height:this._getViewPortHeight()};
    this.quadTree.root = isc.QuadTreeNode.create({
        depth: 0,
        maxDepth: 8,
        maxChildren: 4
    });
    this.quadTree.root.bounds = this.quadTree.bounds;
},

init : function () {
    if (isc.Browser.isIE && this.drawingType == "vml") {
        
        this.hideUsingDisplayNone = true;
    }
    return this.Super("init", arguments);
},

// drawPane.initWidget()
initWidget : function () {
    this.Super("initWidget", arguments);

    var precision = this.precision = Math.max(0, this.precision << 0);
    this._pow10 = Math.pow(10, precision);

    this._initGradients();
    if (this.drawingType == "svg") {
        this._isLoaded = false;

        // Each time an item is drawn, we want to continue from where we left off in terms of
        // def numbers, useGradientIDs, etc.
        this._batchDrawConversionContext = isc.SVGStringConversionContext.create({
            printForExport: false,
            _printForServerExport: false,
            gradientIdProperty: "id"
        });
    }

    // Create a list to store all DrawItems with `exemptFromGlobalTransform: true`.
    this._exemptedDrawItems = [];

    if (this.drawItems == null) {
        this.drawItems = [];
    } else {
        var initDrawItems = this.drawItems.duplicate();
        this.drawItems = [];
        for (var i = 0; i < initDrawItems.length; ++i) {
            this.addDrawItem(initDrawItems[i]);
        }
    }

    this.createQuadTree();

    if (this.canvasItems == null) {
        this.canvasItems = [];
    } else {
        var initCanvasItems = this.canvasItems.duplicate();
        this.canvasItems = [];
        for (var i = 0; i < initCanvasItems.length; ++i) {
            this.addCanvasItem(initCanvasItems[i]);
        }
    }

    // don't redraw with SVG or VML or we'll wipe out the DOM elements DrawItems use
    this.redrawOnResize = (this.drawingType=="bitmap"); 

    this._viewPortWidth = this._getViewPortWidth();
    this._viewPortHeight = this._getViewPortHeight();

    // initialize internal viewbox properties
    this._viewBoxLeft = this.translate == null ? 0 : this.translate[0];
    this._viewBoxTop = this.translate == null ? 0 : this.translate[1];
    this._viewBoxWidth = this._viewPortWidth / this.zoomLevel;
    this._viewBoxHeight = this._viewPortHeight / this.zoomLevel;
    this._prevZoomLevel = this.zoomLevel;

    if (isc.Browser.isIE && this.drawingType == "vml") {
        if (!isc.DrawPane._addedRvmlStyleSheet) {
            
            var doc = this.getDocument(),
                styleSheet;
            try {
                styleSheet = doc.createStyleSheet();
            } catch (e) {
                var styleElem = doc.createElement("style");
                doc.documentElement.appendChild(styleElem);
                styleSheet = styleElem.sheet || styleElem.styleSheet;
            }
            styleSheet.addRule(".rvml", "behavior:url(#default#VML)");
            isc.DrawPane._addedRvmlStyleSheet = true;
        }
        try {
            if (!document.namespaces.rvml) document.namespaces.add("rvml", "urn:schemas-microsoft-com:vml");

            this.startTagVML = function (tagName) {
                return '<rvml:' + tagName + ' class="rvml" ';
            };
            this.endTagVML = function (tagName) {
                return '</rvml:' + tagName + '>';
            };

        } catch (e) {
            this.startTagVML = function (tagName) {
                return '<' + tagName + ' xmlns="urn:schemas-microsoft.com:vml" class="rvml" ';
            };
            this.endTagVML = function (tagName) {
                return '</rvml:' + tagName + '>';
            };
        }

        this._vmlRenderer = isc.VMLRenderer.create(this, this._pow10, this.precision);
    }
},
_initGradients : function () {
    var gradients = this.gradients,
        gradientMap = this._gradientMap = {};

    if (gradients == null) {
        this.gradients = [];
        return;
    }

    if (!isc.isAn.Array(gradients)) {
        gradients = this.gradients = [gradients];
    }

    for (var i = 0, numGradients = gradients.length; i < numGradients; ++i) {
        var gradient = gradients[i];
        if (!gradient.id) {
            this.logWarn("Gradient provided in DrawPane.gradients does not have an ID - ignored");
            continue;
        }
        if (!gradient._constructor) gradient._constructor = isc.DrawItem._getGradientConstructor(gradient);

        if (gradientMap[gradient.id] != null) {
            this.logWarn("Duplicate gradient with ID " + gradient.id + " - replacing previous gradient");
        }
        gradientMap[gradient.id] = gradient;
    }
},

getOffsetX : function () {
    return this.Super("getOffsetX", arguments) - this.getScrollLeft();
},

getOffsetY : function () {
    return this.Super("getOffsetY", arguments) - this.getScrollTop();
},

//> @method drawPane.getDrawingX() (A)
// Returns the X coordinate in the +link{DrawPane,drawing coordinate system} of the last event.
// Note: If you need both the X and Y coordinates in the drawing coordinate system of the last event,
// it is more efficient to call +link{DrawPane.getDrawingPoint(),getDrawingPoint()} instead.
// <p>
// See the documentation of +link{DrawPane.getDrawingPoint(),getDrawingPoint()} for a clarifying
// example.
// @return (int) X coordinate in the drawing coordinate system of the last event.
// @visibility drawing
//<
getDrawingX : function () {
    return this.getDrawingPoint()[0];
},

//> @method drawPane.getDrawingY() (A)
// Returns the Y coordinate in the +link{DrawPane,drawing coordinate system} of the last event.
// Note: If you need both the X and Y coordinates in the drawing coordinate system of the last event,
// it is more efficient to call +link{DrawPane.getDrawingPoint(),getDrawingPoint()} instead.
// <p>
// See the documentation of +link{DrawPane.getDrawingPoint(),getDrawingPoint()} for a clarifying
// example.
// @return (int) Y coordinate in the drawing coordinate system of the last event.
// @visibility drawing
//<
getDrawingY : function () {
    return this.getDrawingPoint()[1];
},

//> @method drawPane.getDrawingPoint() (A)
// Returns the point of the last event in the +link{DrawPane,drawing coordinate system}.
// <p>
// To give a concrete example, suppose that this <code>DrawPane</code> has +link{DrawPane.zoomLevel,zoomLevel}
// 2 and drag-panning is not enabled (just to simplify this example). If +link{Canvas.getOffsetX(),getOffsetX()}
// and +link{Canvas.getOffsetY(),getOffsetY()} is (0, 0) (i.e. the mouse pointer is located at
// the top left point of this <code>DrawPane</code>), then getDrawingPoint() would return
// (0, 0). If getOffsetX/Y() is (20, 40), then getDrawingPoint() would return (10, 20) because
// when the 2&times; zoom level is applied, (10, 20) is translated to (20, 40) on the screen. You
// could, for example, create a new +link{DrawLine} with +link{DrawLine.startPoint,startPoint}
// (10, 20) and when this line is drawn on screen, the position of the line's start point would
// be at offset (20, 40) on screen.
// @return (Point) the point in drawing coordinates of the last event.
// @example drawingCoords
// @visibility drawing
//<
getDrawingPoint : function (event) {
    var EH = this.ns.EH,
        globalPoint = this._pageToGlobal(EH.getX(event), EH.getY(event)),
        x = globalPoint[0],
        y = globalPoint[1];

    // Convert global coordinates to drawing coordinates.
    var normalized = this.normalize(x, y);
    x = normalized[0];
    y = normalized[1];

    return [isc.DrawItem._makeCoordinate(x), isc.DrawItem._makeCoordinate(y)];
},

_toVMLCoord : function (value) {
    return (value * this._pow10) << 0;
},

// Convert coordinates in the global coordinate system to drawing coordinates.
normalize : function (x, y) {
    return this._getInverseGlobalTransform().transform(x, y);
},

// Event handling logic


getDrawItem : function (pageX, pageY, seekingHoverTarget, scEvent) {
    // Convert page coordinates to coordinates in the global coordinate system.
    var globalPoint = this._pageToGlobal(pageX, pageY),
        x = globalPoint[0],
        y = globalPoint[1];

    // Convert global coordinates to drawing coordinates.
    var normalized = this.normalize(x, y);
    x = normalized[0];
    y = normalized[1];
    var items = this.quadTree.retrieve({ x: x, y: y });

    if (items == null) return null;

    

    var itemsLength = items.length;

    for (var i = 0; i < itemsLength; ++i) {
        
        if (items[i].item != null) {
            items[i] = items[i].item;
        }
    }

    
    items.sort(isc.QuadTree._quadTreeItemCompareFunc);

    // We need to traverse from last to first so that an event will be handled by top drawItem
    // when there are several items drawn above one another.
    var blockingGroup = null;
    for (var i = itemsLength; i--; ) {
        var item = items[i],
            shape = item.shape;
        
        if ((seekingHoverTarget &&
             (shape.canHover === false || (shape.canHover == null && shape.prompt == null))) ||
            shape.hidden)
        {
            continue;
        }

        var inItem;
        
        if (scEvent != null && scEvent.nativeTarget != null && shape.drawingSVG &&
            shape._svgHandle === scEvent.nativeTarget)
        {
            inItem = true;

        } else {
            inItem = shape.isPointInPath(x, y, pageX, pageY);
        }

        

        
        var blockedByGroup = false;
        for (var dg = shape.drawGroup; !blockedByGroup && dg != null; dg = dg.drawGroup) {
            
            if (dg.useGroupRect && dg.isInBounds(x, y)) {
                blockedByGroup = true;
                blockingGroup = dg;
            }
        }
        if (blockedByGroup) {
            
            continue;
        }

        if (inItem) {
            if (shape.eventProxy != null) shape = shape.eventProxy;
            return shape;
        }
    }
    return blockingGroup;
},

getEventTarget : function (scEvent) {

//     this.logDebug("getEventTarget firing:" + scEvent.eventType + " at " +
//                  [scEvent.x,scEvent.y], "drawEvents");

    var EH = this.ns.EH,
        eventType = scEvent.eventType;
    if (EH.isMouseEvent(eventType))
    {
        var item = this.getDrawItem(scEvent.x, scEvent.y, false, scEvent);
        if (item != null) return item;
    }
    return this;
},

getHoverTarget : function (scEvent) {
    var item = this.getDrawItem(scEvent.x, scEvent.y, true, scEvent);
    if (item != null) return item;
    return this.Super("getHoverTarget", arguments);
},

_updateCursor : function (drawItem) {
    var currentCursor = null;
    if (drawItem != null) {
        currentCursor = drawItem.getCurrentCursor();
    }
    if (currentCursor == null) currentCursor = this.canDragScroll ? this.dragScrollCursor : this.getCurrentCursor();
    this._applyCursor(currentCursor);
},

prepareForDragging: function () {
    
    if (!isc.EH.dragTarget && !isc.EventHandler.rightButtonDown()) {
        var item = this.getDrawItem(isc.EH.lastEvent.x, isc.EH.lastEvent.y, false, isc.EH.lastEvent);
        if (item != null && item.canDrag) {
            isc.EH.dragTarget = item;
            isc.EH.dragOperation = "drag";
            return true;
        } else if (this.canDragScroll) {
            isc.EH.dragTarget = this;
            isc.EH.dragOperation = isc.EH.DRAG_SCROLL;
            return true;
        }
    }
    // If the user isn't attempting to drag a specific item, call Super() to allow standard Canvas
    // drag behavior.
    return this.Super("prepareForDragging", arguments);
},


dropMove : function () {},

// drawPane.clearDrawItems()
clearDrawItems : function () {
    // wipe the "_drawn" flag from our draw items
    // if we are drawn again, we'll call 'draw()' on each item to re-show as part of drawChildren()
    
    var drawItems = this.drawItems,
        numDrawItems = (drawItems == null ? 0 : drawItems.length);
    if (numDrawItems > 0) {
        for (var i = 0; i < numDrawItems; ++i) {
            var drawItem = drawItems[i];
            drawItem.clear(true, false);
        }
    }
},

// drawPane.clear()
clear : function () {
    this.clearDrawItems();

    this.Super("clear", arguments);

    var type = this.drawingType;
    if (type === "svg") {
        this._svgBody = null;
        this._svgBox = null;
        this._svgDefs = null;
        var batchDrawConversionContext = this._batchDrawConversionContext;
        batchDrawConversionContext.defSvgStrings = {};
        batchDrawConversionContext.useGradientIDs = {};
        this._isLoaded = false;

    } else if (type === "bitmap") {
        this._bitmapContext = null; // clear the cached bitmap context handle
    }
},

// private method for quadtree debug 
drawBounds : function(node) {
    isc.DrawRect.create({
      autoDraw:true,
      drawPane:this,
      left:Math.round(node.bounds.x),
      top:Math.round(node.bounds.y-node.bounds.height),
      width:Math.round(node.bounds.width),
      height:Math.round(node.bounds.height)
    },{
      lineColor:"#FF0000",
      lineOpacity:0.1,
      lineWidth:1,
      linePattern:"solid"
    });
    if(node.nodes && node.nodes.length) {
        for(var i = 0; i < node.nodes.length; ++i) {
          this.drawBounds(node.nodes[i]);
        }
    }
},

//> @method drawPane.erase()
// Call +link{DrawItem.erase()} on all DrawItems currently associated with the DrawPane.  
// <P>
// The DrawItems will continue to exist, and you can call draw() on them to make them appear again, or
// +link{drawItem.destroy,destroy} to get rid of them permanetly.  Use +link{destroyItems()} to permanently
// get rid of all DrawItems.
//
// @visibility drawing
//<


erase : function (destroy, willRedraw) {
    if (willRedraw == null) {
        willRedraw = true;
        this._maybeScheduleRedrawTEA();
    }
    if (destroy) {
        
        var drawItems = this.drawItems || [];
        for (var i = 0; i < drawItems.length; ++i) {
            // pass destroyingAll flag to prevent extra redraws
            drawItems[i].destroy(true, willRedraw); 
        }
    } else if (this.isDrawn()) {
        
        var drawItems = this.drawItems || [];
        for (var i = 0; i < drawItems.length; ++i) {
            // pass erasingAll flag to prevent extra redraws
            drawItems[i].erase(true, willRedraw);
        }
    }
    if (willRedraw) this._erasedGradients = this._gradientMap;
    var gradients = this.gradients;
    if (this.gradients != null) {
        gradients.setLength(0);
        this._gradientMap = {};
    }
    // clearing this.drawItems ensures they won't be drawn again if this pane is clear()ed and
    // draw()n again.
    if (willRedraw) this._erasedDrawItems = this.drawItems;
    this.drawItems = [];
    this._eventOnlyDrawItems = null;
    delete this._delayedDrawItems;
    this.canvasItems = [];
    this._exemptedDrawItems = [];
    this.createQuadTree();

    var type = this.drawingType;
    if (!willRedraw && type === "svg") {
        
        var svgDefs = this._svgDefs;
        if (svgDefs != null) {
            // http://stackoverflow.com/questions/3955229/remove-all-child-elements-of-a-dom-node-in-javascript/22966637#22966637
            var newSvgDefs = this._svgDefs = svgDefs.cloneNode(false);
            svgDefs.parentNode.replaceChild(newSvgDefs, svgDefs);
        }
        var batchDrawConversionContext = this._batchDrawConversionContext;
        batchDrawConversionContext.defSvgStrings = {};
        batchDrawConversionContext.useGradientIDs = {};

    } else if (type === "bitmap") {
        this.redrawBitmap();
    }
},

//> @method drawPane.destroyItems()
// Permanently +link{drawItem.destroy,destroy} all DrawItems currently associated with this DrawPane,
// leaving the DrawPane itself intact
// @visibility drawing
//<
destroyItems : function () {
    this.erase(true, false);
},

destroy : function () {
    var drawItems = this.drawItems;
    if (drawItems != null) {
        
        for (var i = 0; i < drawItems.length; ++i) {
            drawItems[i].destroy(true);
        }
        drawItems = null;
    }
    this.Super("destroy", arguments);
},

//> @method drawPane.addDrawItem()
// Adds a draw item to this <code>DrawPane</code>.  If already added to a <code>DrawPane</code>,
// the draw item is removed from its current <code>DrawPane</code> and added to this <code>DrawPane</code>.
// <p>
// NOTE: For performance reasons, this <code>DrawPane</code> may draw the new item on a
// delay to allow multiple items to be added and drawn at one time. The +link{DrawPane.refreshNow()}
// API will force the item to be drawn immediately.
// @param item (DrawItem) item to add.
// @param autoDraw (boolean) If explicitly set to false, and this drawPane is drawn, don't draw
//   the newly added item
// @visibility drawing
//<
_addCounter: 0,
addDrawItem : function (item, autoDraw, skipContainsCheck) {

    

    if (!skipContainsCheck) {
        if (item.drawGroup != null) {
            item.drawGroup.removeDrawItem(item);

        // Remove the item from an existing DrawPane.
        // We want addDrawItem() to move the item to the front.  This allows the stack order of the
        // draw items to be corrected if other draw items are added, but `item' needs to remain
        // on top.
        } else if (item.drawPane != null) {

            if (item.item != null) {
                item.drawPane.quadTree.remove(item.item);
                item.item = null;
            }

            if (item.drawPane === this) {
                // The item will be readded to the end of the drawItems array later in this method.
                item.erase();

                // If the item's DrawPane is this DrawPane, we now know that `this.drawItems' does not
                // contain the item.
                
                skipContainsCheck = true;

            // Hide the item's knobs since it's being moved from a different DrawPane.
            } else {
                if (item.knobs != null && !item.knobs.isEmpty()) {
                    item.knobs.map(item._hideKnobs, item);
                }
                item.drawPane.removeDrawItem(item);
            }
        }
    }
    item.drawPane = this;
    item.getZIndex(true);
    item._addOrder = this._addCounter++;
    if (isc.isA.DrawGroup(item)) {
        item._addedToDrawPane(this);
    } else if (item.exemptFromGlobalTransform) {
        
        this._exemptedDrawItems.push(item);
    }

    if (autoDraw == null) autoDraw = true;

    // Table of actions:
    // +----------+----------------+---------------------------+
    // | autoDraw | this.isDrawn() | Action                    |
    // +----------+----------------+---------------------------+
    // |    T     |       T        | item.draw()               |
    // |    T     |       F        | add to this.drawItems     |
    // |    F     |       T        | (no action as documented) |
    // |    F     |       F        | add to this.drawItems     |
    // +----------+----------------+---------------------------+
    if (!(this.isDrawn() || this.handleDrawn())) {
        if (skipContainsCheck || !this.drawItems.contains(item)) {
            var i = isc.Array._binarySearch(this.drawItems, item, isc.DrawItem._zIndexCompareFunc);
            if (i < 0) i = -i - 1;
            // Array._binarySearch() returns the lowest index where `item' could be inserted while
            // maintaining sort order by _zIndexCompareFunc. We want to insert `item' at the highest
            // such index.
            for (; i < this.drawItems.length && isc.DrawItem._zIndexCompareFunc(this.drawItems[i], item) == 0; ++i) {
                //empty
            }
            this.drawItems.addAt(item, i);

            item._updateTitleLabelAndBackground();
            item._drawExemptFromGlobalTransformAutoChildren();
        }
        this.shouldDeferDrawing(item);
    } else if (autoDraw && !item._drawn) {
        // just call draw on the item to plug it into this.drawItems and render it out.
        item.draw();
    }
},

// Adds a DrawItem to this DrawPane for the purpose of having it participate in event handling
// only. The DrawItem will not be drawn.
_addEventOnlyDrawItem : function (item) {
    
    item.drawPane = this;
    item._addOrder = this._addCounter++;

    if (this._eventOnlyDrawItems == null) this._eventOnlyDrawItems = [item];
    else this._eventOnlyDrawItems.add(item);
},

// removeDrawItem() - erase the item and remove from this.drawItems / clear up the item.drawPane
// pointer back to this.
// Currently used only by addDrawItem to clear up any previous drawPane the item pointed to.
removeDrawItem : function (item) {
    if (item.drawPane !== this) {
        this.logWarn("Attempt to remove item:" + isc.echoLeaf(item) + " from a DrawPane that does not contain it");
        return;
    }
    if (item._drawn) {
        item.erase();
    } else if (item.drawGroup != null) {
        item.drawGroup.removeDrawItem(item);

        

    } else {
        this.drawItems.remove(item);
        this.cancelDeferredDraw(item);
        if (item.item != null) {
            this.quadTree.remove(item.item);
            delete item.item;
        }
        if (item.exemptFromGlobalTransform) {
            
            this._exemptedDrawItems.remove(item);
        }
    }
    delete item.drawPane;
},

// Override Canvas.getTransformCSS() so that an initial rotation value is not applied as a CSS
// transform to the DrawPane's handle. We'll handle DrawPane.rotation ourselves, depending on
// drawingType.
getTransformCSS : function () {
    return null;
},


_useSvgHelper: false,

_getSvgDocument : function () {
    var svgDocument = this._svgDocument;
    if (this.getDrawnState() !== isc.Canvas.UNDRAWN && !this._useSvgHelper) {
        svgDocument = this.getDocument();
    }
    return svgDocument;
},

_getSvgBodyDOMID : function () {
    return this._getDOMID("body");
},
// outermost <svg> element
_getSvgBody : function () {
    var svgBody = this._svgBody;
    if (svgBody == null && !this._useSvgHelper) {
        svgBody = this._svgBody = this.getDocument().getElementById(this._getSvgBodyDOMID());
    }
    return svgBody;
},

_getSvgBoxDOMID : function () {
    return this._getDOMID("box");
},
// outermost <g>/group element
_getSvgBox : function () {
    var svgBox = this._svgBox;
    if (svgBox == null && !this._useSvgHelper) {
        svgBox = this._svgBox = this.getDocument().getElementById(this._getSvgBoxDOMID());
    }
    return svgBox;
},

_getSvgDefsDOMID : function () {
    return this._getDOMID("defs");
},
// the <defs> element
_getSvgDefs : function () {
    var svgDefs = this._svgDefs;
    if (svgDefs == null && !this._useSvgHelper) {
        svgDefs = this._svgDefs = this.getDocument().getElementById(this._getSvgDefsDOMID());
    }
    return svgDefs;
},

getInnerHTML : function () {
    

    var type = this.drawingType;
    if (type === "vml") {
        // Would be nice to write out the VML behavior definition here, but the style block is
        // not parsed when drawn into a canvas after page load? if there is some way to do this,
        // the style definition is <style>VML\:* {behavior:url(#default#VML);}</style>
        // For now, just writing an outermost group that we can use for zoom and pan. Note
        // that this group needs a unique ID, since multiple DrawPanes may be instantiated on
        // the same page.
        // NB: need to at least wipe out the &nbsp, which pushes all shapes right by a few pixels
        // TODO/TEST - IIRC VML is supposed to define an implicit viewbox, but experts on MSDN
        //  articles have said that it is unpredictable, so we explicitly set it to the available
        //  space in the drawpane here. A 1-1 mapping of screen pixels and vector coordinates will
        //  limit precision/resolution of scalable drawings, though. We might want to set a
        //  higher resolution coordsize here and apply scaling to all drawing operations, not sure,
        //  but something to keep in mind if we see scaling artifacts or other pixel-level
        //  problems. For some more info on the coordinate space, see:
        //  http://msdn.microsoft.com/en-us/library/bb250511.aspx
        var pow10 = this._pow10,
            left = (this._viewBoxLeft - this.scrollLeft + this.getLeftPadding()),
            top = (this._viewBoxTop - this.scrollTop + this.getTopPadding()),
            width = this._viewPortWidth,
            height = this._viewPortHeight,
            theta = (this.rotation || 0) * Math.PI / 180,
            sin = Math.sin(theta), cos = Math.cos(theta),
            m = 50,
            n = (m * pow10).toFixed(0),
            globalSkew = this._getGlobalRotationSkewMatrixAndOffset();
        return isc.StringBuffer.concat(
                
                this.startTagVML('RECT'), " ID='",
                this.getID(), "_vml_eventmask' style='position:absolute;top:0px;left:0px;width:100%;height:100%;' stroked='false' fillcolor='#ff00ff'>",
                    this.startTagVML('FILL'), " on='t' opacity='0' />",
                this.endTagVML('RECT'),

                
                this.startTagVML("SHAPETYPE"), " ID='", this.getID(), "_global_rotation'>",
                this.startTagVML("SKEW"), " ON='t' MATRIX='", globalSkew.matrix, "'",
                " OFFSET='", globalSkew.offset, "'>",
                this.endTagVML("SKEW"),
                this.endTagVML("SHAPETYPE"),
                
                this.startTagVML("SHAPETYPE"), " ID='", this.getID(), "_global_rotation_zoom'>",
                this.startTagVML("SKEW"), " ON='t' MATRIX='", globalSkew.zoomMatrix, "'",
                " OFFSET='", globalSkew.zoomOffset, "'>",
                this.endTagVML("SKEW"),
                this.endTagVML("SHAPETYPE"),

                // Define a group that scales all coordinates by a power of 10.
                this.startTagVML("GROUP"), " COORDORIGIN='-", n, ",-", n, "' COORDSIZE='", n, ",", n,
                "' STYLE='position:absolute;left:-", m, "px;top:-", m, "px;width:", m, "px;height:", m, "px;'>",

                
                this.startTagVML("GROUP"), " ID='",
                this.getID(), "_vml_box' STYLE='position:absolute;",
                "left:", Math.round(pow10 * (cos * left - sin * top)),
                "px;top:", Math.round(pow10 * (sin * left + cos * top)),
                "px;width:", pow10 * width,
                "px;height:", pow10 * height,
                "px;' coordsize='", (this._viewBoxWidth * pow10), ", ", (this._viewBoxHeight * pow10),
                "' coordorigin='0 0'>",
                this.endTagVML("GROUP"),

                this.endTagVML("GROUP"));

    } else if (type === "bitmap") {
        return isc.SB.concat(
            "<CANVAS ID='", this.getID(), "_bitmap' ",
                "WIDTH='", this._viewPortWidth, "' ",
                "HEIGHT='", this._viewPortHeight, "' ",
                "STYLE='display:block' dir='ltr'>",
            "</CANVAS>"); // Firefox *requires* a closing CANVAS tag

    } else if (type === "svg") {
        if (this._useSvgHelper) {
            this._isLoaded = false;
            return "<IFRAME HEIGHT='100%' WIDTH='100%' SCROLLING='NO' FRAMEBORDER='0' SRC='" +
                isc.Page.getHelperDir() + "DrawPane.svg?isc_dp_id=" + this.getID() +"'></IFRAME>";

        } else {
            this._isLoaded = true;
            var t = this._getGlobalTransform();
            return ("<svg id='" + this._getSvgBodyDOMID() +
                    "' width='" + this._viewPortWidth +
                    "px' height='" + this._viewPortHeight +
                    "px'>" +
                    "<defs id='" + this._getSvgDefsDOMID() + "'></defs>" +
                    "<g id='" + this._getSvgBoxDOMID() +
                    "' transform='matrix(" + t.m00 + " " + t.m10 + " " +
                                             t.m01 + " " + t.m11 + " " +
                                             t.m02 + " " + t.m12 + ")'></g>" +
                    "</svg>");
        }

    } else {
        this.logWarn("DrawPane getInnerHTML: '" + type + "' is not a supported drawingType");
        return this.Super("getInnerHTML", arguments);
        // TODO implement alternate rendering/message when no drawing technology is available
    }
},

_getGlobalRotationSkewMatrixAndOffsetOutput: { matrix: "", zoomMatrix: "", offset: "", zoomOffset: "" },
_getGlobalRotationSkewMatrixAndOffset : function () {
    var output = this._getGlobalRotationSkewMatrixAndOffsetOutput,
        cx = this._viewPortWidth / 2, cy = this._viewPortHeight / 2,
        theta = (this.rotation || 0) * Math.PI / 180,
        sin = Math.sin(theta), cos = Math.cos(theta);

    var c = Math.round(65536 * cos),
        negS = Math.round(-65536 * sin),
        s = Math.round(65536 * sin);
    output.matrix = isc.StringBuffer.concat(c, "f,", negS, "f,", s, "f,", c, "f,0,0");
    var zoomLevel = this.zoomLevel;
    c = Math.round(65536 * cos * zoomLevel);
    negS = Math.round(-65536 * sin * zoomLevel);
    s = Math.round(65536 * sin * zoomLevel);
    output.zoomMatrix = isc.StringBuffer.concat(c, "f,", negS, "f,", s, "f,", c, "f,0,0");

    var m02 = (1 - cos) * cx + sin * cy,
        m12 = -sin * cx + (1 - cos) * cy;
    output.offset = isc.StringBuffer.concat(m02.toFixed(6), "px,", m12.toFixed(6), "px");
    output.zoomOffset = output.offset;
    return output;
},


_handleResized : function () {
    this._updateViewPort();
},

// Override Canvas.innerSizeChanged() so that the DrawPane uses the current values of
// getInnerContentWidth() and getInnerContentHeight() if, for example, setBorder() is called.
innerSizeChanged : function (reason) {
    this.Super("innerSizeChanged", arguments);
    this._updateViewPort();
},

//> @type DataURLFormat
// The data URL MIME type to use when +link{DrawPane.getDataURL()} is called to convert the
// drawing to a data URL.
// @value "any" Any MIME type supported by the browser is acceptable. Note: The exact MIME type
// used may depend on the browser, and may change from version to version of SmartClient.
// @value "png" Generate an <code>image/png</code> data URL.
// @visibility drawing
//<

//> @method drawPane.getDataURL()
// Get a "data:" URL encoding the current contents of the <code>DrawPane</code>.
// <p>
// The returned "data:" URLs can be used anywhere a URL to an image is valid, for example,
// +link{Img.src}.
// <p>
// This method will directly return the data URL on modern browsers when using &lt;canvas&gt;-style
// rendering (the default), and if there are no +link{DrawImage}s in this <code>DrawPane</code>
// that load cross-domain images.
// <p>
// On legacy browers (any version of IE in "quirks" mode, all versions of IE prior to 9.0),
// or if there is a <code>DrawImage</code> that loads a cross-domain image, data
// URL generation requires a server trip and requires the SmartClient Server to be installed with
// the same set of +link{group:javaModuleDependencies,required .jars} as are required for PDF
// export of charts in legacy IE.  The method will return null and a callback must be passed,
// which fires when the data URL has been retrieved from the server.
// <p>
// If the callback is passed but no server trip is required, the callback is fired immediately.
// <p>
// For obtaining PNG or other image data for use in server-side processing (such as attaching to
// automated emails or saving to a database), see also the server-side APIs in
// com.isomorphic.contentexport.ImageExport.
// <p>
// Note: It is recommended to pass a callback instead of relying on the method returning the
// data URL directly. This is because the callback will always be called with the generated
// data URL, whereas work-arounds for browser bugs may require asynchronous generation of the
// data URL, meaning that a data URL might not be returned immediately in certain browsers for
// certain <code>DrawPane</code> contents.
//
// @param [callback] (DataURLCallback) callback to fire when the data URL is available. The callback
// is called regardless of whether the data URL is also returned.
// @param [format] (DataURLFormat) the format of the data URL. If not specified, then
// <smartclient>"all"</smartclient>
// <smartgwt>{@link com.smartgwt.client.types.DataURLFormat#ALL}</smartgwt>
// is assumed.
// @return (String) the data URL if synchronously generated.
// @example chartImageExport
// @visibility drawing
//<
getDataURL : function (callback, format) {
    var svgString,
        drawingType = this.drawingType,
        drawingSVG = drawingType === "svg",
        drawingBitmap = drawingType === "bitmap",
        dataURL = null;

    if (this._isBatchDrawing()) this.refreshNow();

    

    // If using SVG drawing and the given format is "png", then draw the draw items into a
    // temporary <canvas>.
    
    // This code path is also used when bitmap drawing and drag-scrolling is enabled. In this
    // case, the drawing usually extends past the scroll viewport, so we need to redraw the
    // draw items into a large enough <canvas>.
    if ((drawingSVG && isc.Browser.hasCANVAS && format == "png") ||
        (drawingBitmap && this.canDragScroll))
    {
        

        var canvas = this.getDocument().createElement("canvas"),
            canvasWidth,
            canvasHeight;
        if (this.canDragScroll) {
            canvasWidth = Math.max(0, this.drawingWidth * this.zoomLevel) << 0;
            canvasHeight = Math.max(0, this.drawingHeight * this.zoomLevel) << 0;
        } else {
            canvasWidth = this._viewPortWidth;
            canvasHeight = this._viewPortHeight;
        }
        canvas.width = canvasWidth;
        canvas.height = canvasHeight;
        canvas.dir = "ltr";
        var context = canvas.getContext("2d");
        if (context != null) {
            if (this.backgroundColor != null) {
                context.fillStyle = this.backgroundColor;
                context.fillRect(0, 0, canvasWidth, canvasHeight);
            }

            var origScrollLeft = this.scrollLeft,
                origScrollTop = this.scrollTop;
            // If the drawing is panned, scroll back to 0,0 so that we can draw the entire
            // drawing into the temporary <canvas>.
            if (origScrollLeft != 0 || origScrollTop != 0) {
                this.scrollTo(0, 0);
            }

            this._drawBitmapState = {
                _globalTransform: this._getGlobalTransform(),
                _scrollLeft: this.scrollLeft,
                _scrollTop: this.scrollTop,
                _first: true,
                _tmpBitmapContext: context
            };
            this._drawBitmapDrawItems(context, this.drawItems);
            delete this._drawBitmapState;

            // Restore the original scrollLeft/Top.
            if (origScrollLeft != 0 || origScrollTop != 0) {
                this.scrollTo(origScrollLeft, origScrollTop);
            }

            dataURL = canvas.toDataURL();
            if (dataURL != null &&
                dataURL.startsWith("data:image/png") &&
                (dataURL.charAt(14) === "," || dataURL.charAt(14) === ";"))
            {
                if (callback) this.fireCallback(callback, "dataURL", [dataURL]);
                return dataURL;
            }
        }
    }

    // Generate an SVG data URL if SVG drawing and either the format was unspecified or "all".
    if (drawingSVG && (format == null || format == "all")) {
        var conversionContext = isc.SVGStringConversionContext.create({
            // Leave printForExport set to true (the default), but we're not printing for
            // server export.
            _printForServerExport: false
        });
        svgString = this.getSvgString(conversionContext);
        dataURL = "data:image/svg+xml," + encodeURIComponent(svgString);
        if (callback) this.fireCallback(callback, "dataURL", [dataURL]);
        return dataURL;

        

    } else if (drawingBitmap && !this.canDragScroll) {
        var canvas = document.getElementById(this.getID() + "_bitmap");
        if (canvas != null && !!canvas.toDataURL) {
            // If the DrawPane has a backgroundColor set, then we need to create a temporary
            // <canvas> filled with the backgroundColor and draw the current canvas contents
            // over the opaque background (composing any transparency in the drawing with the
            // backgroundColor).
            // This is like GIMP's "Remove Alpha Channel" operation: http://docs.gimp.org/en/gimp-layer-alpha-remove.html
            if (this.backgroundColor != null) {
                var tmpCanvas = this.getDocument().createElement("canvas");
                tmpCanvas.width = canvas.width;
                tmpCanvas.height = canvas.height;
                var tmpContext = tmpCanvas.getContext("2d", { alpha: false });
                if (tmpContext == null) {
                    this._performServerFrameworkExportImage(svgString, callback);
                    return;
                }

                tmpContext.fillStyle = this.backgroundColor;
                tmpContext.fillRect(0, 0, canvas.width, canvas.height);
                tmpContext.drawImage(canvas, 0, 0);
                canvas = tmpCanvas;
            }

            try {
                // "When a user agent is to create a serialization of the bitmap as a file,
                // optionally with some given arguments, and optionally with a native flag set,
                // it must create an image file in the format given by the first value of arguments,
                // or, if there are no arguments, in the PNG format."
                // http://www.whatwg.org/specs/web-apps/current-work/multipage/the-canvas-element.html#a-serialization-of-the-bitmap-as-a-file
                dataURL = canvas.toDataURL();

            // HTMLCanvasElement.toDataURL() will throw a SecurityError if the canvas' origin-clean
            // flag is set to false.
            // http://www.whatwg.org/specs/web-apps/current-work/multipage/the-canvas-element.html#dom-canvas-todataurl
            } catch (e) {
                this.logWarn("HTMLCanvasElement.toDataURL() threw an exception: " + e);
                dataURL = null;
            }

            
            if (dataURL != null &&
                dataURL.startsWith("data:image/png") &&
                (dataURL.charAt(14) === "," || dataURL.charAt(14) === ";"))
            {
                if (callback) this.fireCallback(callback, "dataURL", [dataURL]);
                return dataURL;
            }
        }
    }

    this._performServerFrameworkExportImage(svgString, callback);

    return null;
},

_performServerFrameworkExportImage : function (svgString, callback) {
    if (callback) {
        //>Offline
        if (isc.Offline && isc.Offline.isOffline()) {
            this.logWarn("Skipping the exportImage RPC call because the application is not online.");
            this.fireCallback(callback, "dataURL", [null]);
            return;
        }
        //<Offline

        if (svgString == null) svgString = this.getSvgString();
        var requestProperties = {
            exportDisplay: "return"
        };
        var dp = this;
        isc.RPCManager.exportImage(svgString, requestProperties, function (imageData) {
            if (imageData == null) {
                dp.logError("RPCManager.exportImage() returned no data.");
                dp.fireCallback(callback, "dataURL", [null]);
            } else {
                
                switch (imageData.length % 4) {
                    case 1:
                        imageData += "===";
                        break;
                    case 2:
                        imageData += "==";
                        break;
                    case 3:
                        imageData += "=";
                        break;
                    case 0:
                    default:
                        break;
                }
                
                dp.fireCallback(callback, "dataURL", ["data:image/png;base64," + imageData]);
            }
        });
    }
},

//> @method Callbacks.DataURLCallback
// Callback for +link{drawPane.getDataURL()}.
// @param dataURL (String) the data URL
// @visibility drawing
//<

// DrawPane.measureLabel() caches the label measurements in an LRU cache.  This property
// specifies the maximum number of entries to keep in the cache.
measureLabelCacheMaxSize: 100,

setMeasureLabelCacheMaxSize : function (measureLabelCacheMaxSize) {
    this.measureLabelCacheMaxSize = measureLabelCacheMaxSize;
    var cache = this._measureLabelCache;
    if (cache != null) {
        var maxSize = Math.max(1, measureLabelCacheMaxSize);
        this._lruCacheSetMaxSize(cache, maxSize);
    }
},


measureLabel : function (text, labelProps) {
    if (text == null) text = "";
    else text = String(text);

    if (!labelProps) labelProps = {};
    var fontFamily = labelProps.fontFamily || isc.DrawLabel.getInstanceProperty("fontFamily"),
        fontWeight = labelProps.fontWeight || isc.DrawLabel.getInstanceProperty("fontWeight"),
        fontSize = labelProps.fontSize || isc.DrawLabel.getInstanceProperty("fontSize"),
        fontStyle = labelProps.fontStyle || isc.DrawLabel.getInstanceProperty("fontStyle"),
        cacheKey = fontSize + ":" + fontWeight + ":" + fontStyle + ":" + fontFamily + ":" + text;

    var cache = this._measureLabelCache;
    if (cache == null) {
        var maxSize = Math.max(1, this.measureLabelCacheMaxSize);
        cache = this._measureLabelCache = new this._lruCache(maxSize);
    }

    if (this._lruCacheContains(cache, cacheKey)) {
        return this._lruCacheGet(cache, cacheKey);
    }

    
    var measureCanvas = isc.DrawPane._getMeasureCanvas(),
        styleQuote = isc.DrawPane._getEnclosingQuote(fontFamily),
        contents = isc.SB.concat(
            "<span style=", styleQuote, "font-weight:", fontWeight,
            ";font-size:", fontSize, "px;font-style:", fontStyle,
            ";white-space:pre;font-family:", fontFamily, styleQuote, ">",
            text.replace(isc.DrawPane._spaceCharsRegExp, "\u0020").asHTML(), "</span>");
    measureCanvas.setContents(contents);
    measureCanvas.redraw("label measurement: " + text);
    var spanEl = measureCanvas.getHandle().firstChild;
    
    var dims = {
        width: spanEl.offsetWidth,
        height: spanEl.offsetHeight
    };
    //isc.logWarn("measureLabel:" + this.echoFull(dims) + 
    //            "\ncontent:" + measureCanvas.getContents());
    this._lruCachePut(cache, cacheKey, dims);
    return dims;
},

// Constructor functions for an LRU cache and its entries.
_lruCacheEntry : function (key, value, younger, older) {
    this.key = key;
    this.value = value;
    this.younger = younger;
    this.older = older;
},

_lruCache : function (maxSize) {
    
    this.maxSize = maxSize;
    this.size = 0;
    this.cache = {};
    this.youngest = null;
    this.oldest = null;
},

_lruCacheContains : function (cache, key) {
    return cache.cache.hasOwnProperty(key);
},

_lruCacheGet : function (cache, key) {
    

    var entry = cache.cache[key];
    
    if (entry != cache.youngest) {
        // Remove the entry from the list.
        entry.younger.older = entry.older;
        if (entry.older == null) {
            cache.oldest = entry.younger;
        } else {
            entry.older.younger = entry.younger;
        }

        // Add the entry to the head of the list.
        cache.youngest.younger = entry;
        entry.younger = null;
        entry.older = cache.youngest;
        cache.youngest = entry;
    }

    
    return entry.value;
},

_lruCachePut : function (cache, key, value) {
    
    var entry = new this._lruCacheEntry(key, value, null, cache.youngest);
    cache.cache[key] = entry;
    ++cache.size;
    if (cache.size == 1) {
        cache.youngest = cache.oldest = entry;
    } else {
        // Make `entry` the youngest entry.
        cache.youngest.younger = entry;
        cache.youngest = entry;

        // If the cache is now over the `maxSize` then remove the oldest entry.
        if (cache.size > cache.maxSize) {
            
            var oldest = cache.oldest,
                secondOldest = oldest.younger;

            cache.oldest = secondOldest;
            secondOldest.older = null;

            delete cache.cache[oldest.key];
            --cache.size;
        }
    }

    
},

_lruCacheSetMaxSize : function (cache, maxSize) {
    
    // If the cache size is over the new `maxSize` then remove the oldest entries.
    if (cache.size > maxSize) {
        var tailSize = cache.size - maxSize,
            oldCache = cache.cache;
        if (maxSize < tailSize) {
            // Copy the `maxSize` youngest entries into another cache.
            var newCache = {},
                entry = cache.youngest;
            for (var i = maxSize; i--; entry = entry.older) {
                newCache[entry.key] = oldCache[entry.key];
            }
            cache.cache = newCache;
            cache.oldest = entry.younger;
        } else {
            // Remove the `tailSize` oldest entries from the cache.
            var entry = cache.oldest;
            for (var i = tailSize; i--; entry = entry.younger) {
                delete oldCache[entry.key];
            }
            cache.oldest = entry;
        }

        cache.oldest.older = null;
        cache.size = maxSize;
    }
    cache.maxSize = maxSize;

    
},




_borderStyleAndColorMask: {
    "borderTopStyle": "border-top-style",
    "borderRightStyle": "border-right-style",
    "borderBottomStyle": "border-bottom-style",
    "borderLeftStyle": "border-left-style",
    "borderTopColor": "border-top-color",
    "borderRightColor": "border-right-color",
    "borderBottomColor": "border-bottom-color",
    "borderLeftColor": "border-left-color"
},
//> @method drawPane.getSvgString()
// Converts this DrawPane to the source of an <code>&lt;svg&gt;</code> element equivalent to the
// current drawing.
// <p>
// In Pro edition and above, the returned string can be used with
// +link{RPCManager.exportImage()} to download an image, or with server-side APIs in
// com.isomorphic.contentexport.ImageExport to obtain various kinds of images for further
// server-side processing.
//
// @return (String) the source of an <code>&lt;svg&gt;</code> element.
// @example chartImageExport
// @visibility drawing
//<
getSvgString : function (conversionContext) {
    var width = this.getInnerContentWidth(),
        height = this.getInnerContentHeight(),
        rotation,
        center,
        widthP,
        heightP;

    widthP = width;
    heightP = height;

    // When the drawing is rotated, the width and height of the <svg> element need to be set to
    // `widthP` and `heightP`, the width and height of the bounding box of a rectangle rotated
    // `this.rotation` degrees and having width `this.getWidth()` and height `this.getHeight()`.
    if (this.rotation && (center = this.getCenter()) && center.length == 2) {
        rotation = ((this.rotation % 360) + 360) % 360;

        
        var phi = rotation * this._radPerDeg,
            s = Math.abs(Math.sin(phi)),
            c = Math.abs(Math.cos(phi));
        widthP = width * c + height * s;
        heightP = width * s + height * c;
    }

    conversionContext = conversionContext || isc.SVGStringConversionContext.create();
    conversionContext.xlinkPrefix = conversionContext.xlinkPrefix || isc.SVGStringConversionContext._$xlink;
    var viewPortEdgeOffsets = this._getViewPortEdgeOffsets(conversionContext.printForExport);

    var borderWidths = this._calculateBorderSize(),
        calculatedPadding = this._calculatePadding();

    var finalWidth = (borderWidths.left + calculatedPadding.left + widthP + calculatedPadding.right + borderWidths.right -
                      viewPortEdgeOffsets.right - viewPortEdgeOffsets.left),
        finalHeight = (borderWidths.top + calculatedPadding.top + heightP + calculatedPadding.bottom + borderWidths.bottom -
                       viewPortEdgeOffsets.top - viewPortEdgeOffsets.bottom);
    var svg = isc.StringBuffer.create();
    svg.append(
        "<svg xmlns='", isc._$svgNS,
        "' xmlns:", conversionContext.xlinkPrefix, "='", isc._$xlinkNS,
        "' width='", finalWidth, "px",
        "' height='", finalHeight, "px",
        "' viewBox='0 0 ", finalWidth, " ", finalHeight,
        "' version='1.1'><metadata><!-- Generated by SmartClient ", isc.version, " --></metadata>");
    if (this.backgroundColor != null) {
        svg.append(
            "<rect x='0' y='0' width='", finalWidth,
            "' height='", finalHeight,
            "' stroke='none' fill='", this.backgroundColor, "'/>");
    }
    svg.append("<g transform='translate(", ((widthP - width) / 2 + calculatedPadding.left + borderWidths.left), " ", ((heightP - height) / 2 + calculatedPadding.top + borderWidths.top), ") scale(", this.zoomLevel, ")");
    if (rotation) {
        
        svg.append(" rotate(", rotation, " ", center[0], " ", center[1], ")");
    }
    svg.append("'><svg width='", width, "' height='", height, "'>");
    var drawItems = this.drawItems,
        numDrawItems = (drawItems == null ? 0 : drawItems.length);
    if (numDrawItems > 0) {
        svg.append("<g>");
        for (var i = 0; i < numDrawItems; ++i) {
            svg.append(drawItems[i].getSvgString(conversionContext));
        }
        svg.append("</g>");
        var defSvgStrings = conversionContext.defSvgStrings;
        if (defSvgStrings != null) {
            svg.append("<defs>");
            for (var defID in defSvgStrings) {
                if (!defSvgStrings.hasOwnProperty(defID)) continue;
                svg.append(defSvgStrings[defID]);
            }
            svg.append("</defs>");
        }
    }
    svg.append("</svg>",
               (conversionContext.printForExport ? this._getChildrenSvgString(conversionContext) : null),
               "</g>");

    // Draw the borders
    var computedStyle = isc.Element.getComputedStyle(this.getClipHandle(), this._borderStyleAndColorMask);
    var strokeDasharrayAttr,
        xOffset,
        yOffset;
    // As a special exception, if all four borders are the same, then output a single <rect> element.
    if (borderWidths.top == borderWidths.right &&
        borderWidths.right == borderWidths.bottom &&
        borderWidths.bottom == borderWidths.left &&

        computedStyle.borderTopStyle == computedStyle.borderRightStyle &&
        computedStyle.borderRightStyle == computedStyle.borderBottomStyle &&
        computedStyle.borderBottomStyle == computedStyle.borderLeftStyle &&

        computedStyle.borderTopColor == computedStyle.borderRightColor &&
        computedStyle.borderRightColor == computedStyle.borderBottomColor &&
        computedStyle.borderBottomColor == computedStyle.borderLeftColor)
    {
        if (borderWidths.top) {
            strokeDasharrayAttr = null;
            if (computedStyle.borderTopStyle == "solid") strokeDasharrayAttr = "";
            else if (computedStyle.borderTopStyle == "dashed") strokeDasharrayAttr = " stroke-dasharray='5,5'";
            else if (computedStyle.borderTopStyle == "dotted") strokeDasharrayAttr = " stroke-dasharray='1.5,2'";

            if (strokeDasharrayAttr != null && computedStyle.borderTopColor) {
                yOffset = xOffset = borderWidths.top / 2;
                svg.append(
                    "<rect x='", xOffset,
                    "' y='", yOffset,
                    "' width='", (finalWidth - borderWidths.top),
                    "' height='", (finalHeight - borderWidths.top),
                    "' stroke='", computedStyle.borderTopColor,
                    "' stroke-width='", borderWidths.top, "px'",
                    strokeDasharrayAttr, " fill='none'/>");
            }
        }
    } else {
        var dirs = [ "Left", "Bottom", "Right", "Top" ],
            borderWidth,
            borderStyle,
            borderColor,
            x,
            y;
        for (var i = 0; i < dirs.length; ++i) {
            var dir = dirs[i];
            borderWidth = borderWidths[dir];
            if (borderWidth) {
                borderStyle = computedStyle["border" + dir + "Style"];
                strokeDasharrayAttr = null;
                if (borderStyle == "solid") strokeDasharrayAttr = "";
                else if (borderStyle == "dashed") strokeDasharrayAttr = " stroke-dasharray='5,5'";
                else if (borderStyle == "dotted") strokeDasharrayAttr = " stroke-dasharray='1.5,2'";

                if (strokeDasharrayAttr != null) {
                    borderColor = computedStyle["border" + dir + "Color"];
                    if (borderColor) {
                        if (dir == "Left") {
                            x = borderWidth / 2;
                            yOffset = borderWidth / 2;
                            svg.append(
                                "<line x1='", x,
                                "' y1='", yOffset,
                                "' x2='", x,
                                "' y2='", (finalHeight - yOffset));
                        } else if (dir == "Bottom") {
                            xOffset = borderWidth / 2;
                            y = finalHeight - borderWidth / 2;
                            svg.append(
                                "<line x1='", xOffset,
                                "' y1='", y,
                                "' x2='", (finalWidth - xOffset),
                                "' y2='", y);
                        } else if (dir == "Right") {
                            x = finalWidth - borderWidth / 2;
                            yOffset = borderWidth / 2;
                            svg.append(
                                "<line x1='", x,
                                "' y1='", yOffset,
                                "' x2='", x,
                                "' y2='", (finalHeight - yOffset));
                        } else { // dir == "Top"
                            xOffset = borderWidth / 2;
                            y = borderWidth / 2;
                            svg.append(
                                "<line x1='", xOffset,
                                "' y1='", y,
                                "' x2='", (finalWidth - xOffset),
                                "' y2='", y);
                        }

                        svg.append(
                            "' stroke='", borderColor,
                            "' stroke-width='", borderWidth, "px'",
                            strokeDasharrayAttr, "/>");
                    }
                }
            } // end if (borderWidth)
        }
    }

    svg.append("</svg>");
    return svg.release(false);
},

// Returns a string of SVG for the children of this DrawPane.

_getChildrenSvgString : function (conversionContext) {
    return null;
},

//> @method drawPane.getPrintHTML() [A]
// Retrieves printable HTML for this component and all printable subcomponents.
// <P>
// By default any Canvas with children will simply collect the printable HTML of its
// children by calling getPrintHTML() on each child that is considered
// +link{canvas.shouldPrint,printable}.
// <P>
// If overriding this method for a custom component, you should <b>either</b> return a String of
// printable HTML string directly <b>or</b> return null, and fire the callback (if provided)
// using +link{Class.fireCallback}.
// <P>
// To return an empty print representation, return an empty string ("") rather than null.
// <P>
// The <code>printProperties</code> argument, if passed, must be passed to any subcomponents on
// which <code>getPrintHTML()</code> is called.
// <P>
// <b>Notes on printing</b>
// <P>
// To print a <code>DrawPane</code> for export on IE8 and earlier, it is important to pass
// +link{PrintProperties} with +link{PrintProperties.printForExport,printForExport}:true:
// <smartclient>
// <pre>var exportHTML = drawPane.getPrintHTML({ printForExport:true });</pre>
// </smartclient><smartgwt>
// <pre>final PrintProperties pp = new PrintProperties();
//pp.setPrintForExport(true);
//final String exportHTML = drawPane.getPrintHTML(pp, null);</pre>
// </smartgwt>
// @include method:canvas.getPrintHTML()
// @group printing
// @visibility drawing
//<
getPrintHTML : function (printProperties, callback) {
    var HTML,
        printingForExport = (printProperties != null && printProperties.printForExport);

    if (this._isBatchDrawing()) this.refreshNow();

    
    if (this.drawingType === "bitmap" &&
        !(isc.isA.FacetChart && isc.isA.FacetChart(this) && this.printZoomChart && this._getCanZoom() && printingForExport))
    {
        // Get a PNG data URL for this DrawPane. If we are unable to generate
        // the data URL synchronously, then getDataURL() returns early.
        var dataURL = this.getDataURL();
        if (dataURL == null) {
            // Call getDataURL() again, this time passing a callback to allow
            // the data URL to be asynchronously generated. getPrintHTML() will
            // need to "go asynchronous" as well.
            if (callback != null) {
                var drawPane = this;
                this.getDataURL(function (dataURL) {
                    var HTML = "<img src='" + dataURL + "' style='border:0px";
                    if (printingForExport) HTML += ";max-width:100%";
                    HTML += "'/>";
                    drawPane.fireCallback(callback, "HTML,callback", [HTML, callback]);
                });
            }
            HTML = null;

        } else {
            HTML = "<img src='" + dataURL + "' style='border:0px";
            
            if (printingForExport) HTML += ";max-width:100%";
            HTML += "'/>";
        }

    } else if (this.drawingType === "vml" && !printingForExport) {
        var vml_box = document.getElementById(this.getID() + "_vml_box");
        // Enclose the vml box in a relative tag so it'll flow correctly in the document
        
        HTML = ("<div style='position:relative;width:" + this._getViewPortWidth() +
                ";height:" + this._getViewPortHeight() +
                ";'>" + vml_box.parentElement.innerHTML + "</div>");

    } else {
        HTML = this.getSvgString();
    }

    if (callback != null) {
        this.fireCallback(callback, "HTML,callback", [HTML, callback]);
        return null;
    } else {
        return HTML;
    }
},

_batchDraw : function (drawItems) {
    
    if (drawItems == null) drawItems = [];
    else if (!isc.isAn.Array(drawItems)) drawItems = [drawItems];

    // Ensure that all peers of items in `drawItems' are added to `drawItems' to be batch drawn
    // as well.
    for (var ri = drawItems.length; ri > 0; --ri) {
        var item = drawItems[ri - 1],
            itemPeers = item.peers;
        if (itemPeers != null) {
            for (var rj = itemPeers.length; rj > 0; --rj) {
                var itemPeer = itemPeers[rj - 1];
                // Add `itemPeer' to `drawItems' if not already added. (If `itemPeer' were
                // added twice, then in VML and SVG drawing modes, the VML/SVG handle would be
                // added twice.)
                if (!drawItems.contains(itemPeer)) {
                    drawItems.splice(ri, 0, itemPeer);
                }
            }
        }
    }

    var numDrawItems = drawItems.length,
        type = this.drawingType;
    if (type === "svg") {
        for (var i = 0; i < numDrawItems; ++i) {
            drawItems[i].draw();
        }

    } else if (type === "bitmap") {
        this.redrawBitmapNow(true);

    } else {
        

        var beforeVMLString = "",
            afterVMLBuffer = isc.StringBuffer.create(),
            conversionContext = isc.VMLStringConversionContext.create();
        for (var i = 0; i < numDrawItems; ++i) {
            var drawItem = drawItems[i];
            
            var id = "isc_DrawItem_" + drawItem.drawItemID;
            if (drawItem.drawToBack) {
                var buffer = isc.StringBuffer.create();
                drawItem._getElementVML(buffer, id, conversionContext);
                beforeVMLString = buffer.release(false) + beforeVMLString;
            } else {
                drawItem._getElementVML(afterVMLBuffer, id, conversionContext);
            }
        }

        var vmlContainer = isc.Element.get(this.getID() + "_vml_box");
        
        if (!isc.isAn.emptyString(beforeVMLString)) {
            vmlContainer.insertAdjacentHTML("afterbegin", beforeVMLString);
        }
        var afterVMLString = afterVMLBuffer.release(false);
        if (!isc.isAn.emptyString(afterVMLString)) {
            vmlContainer.insertAdjacentHTML("beforeend", afterVMLString);
        }

        // Go through drawLabelsAccumulator.
        var drawLabelsAccumulator = conversionContext.drawLabelsAccumulator,
            numDrawLabels = (
                drawLabelsAccumulator == null ? 0 : drawLabelsAccumulator.length);
        if (numDrawLabels > 0) {
            beforeVMLString = "";
            afterVMLBuffer = isc.StringBuffer.create();
            for (var i = 0; i < numDrawLabels; ++i) {
                var drawLabel = drawLabelsAccumulator[i];
                
                // This time, do not pass the conversionContext. This is how DrawLabel knows to
                // return the <DIV> markup.
                var id = "isc_DrawItem_" + drawLabel.drawItemID;
                if (drawLabel.drawToBack) {
                    var buffer = isc.StringBuffer.create();
                    drawLabel._getElementVML(buffer, id);
                    beforeVMLString = buffer.release(false) + beforeVMLString;
                } else {
                    drawLabel._getElementVML(afterVMLBuffer, id);
                }
            }

            vmlContainer = this.getHandle();
            vmlContainer.insertAdjacentHTML("afterbegin", beforeVMLString);
            vmlContainer.insertAdjacentHTML("beforeend", afterVMLBuffer.release(false));
        }
    }

    for (var i = 0; i < numDrawItems; ++i) {
        var drawItem = drawItems[i];
        
        drawItem._completeDraw();
    }

    this._setupEventOnlyDrawItems();

    
},

_insertDefsSvgString : function (defsSvgString) {
    var svgDocument = this._getSvgDocument();
    

    var svgDefs = this._getSvgDefs();
    if ("insertAdjacentHTML" in svgDefs) {
        svgDefs.insertAdjacentHTML("beforeend", defsSvgString);

    } else if (isc.Browser._supportsSVGCreateContextualFragment) {
        var range = svgDocument.createRange();
        range.setStart(this._getSvgBody(), 0);
        var docFragment = range.createContextualFragment(defsSvgString);
        
        svgDefs.appendChild(docFragment);

    } else {
        var conversionContext = this._batchDrawConversionContext;
        

        var svgString = (
            "<svg xmlns='" + isc._$svgNS + "' xmlns:" + conversionContext.xlinkPrefix + "='" + isc._$xlinkNS + "'>" +
            "<defs>" + defsSvgString + "</defs>" +
            "</svg>"
            );

        
        var domParser = new DOMParser(),
            svgElem;
        try {
            var parsedDoc = domParser.parseFromString(svgString, "image/svg+xml");
            svgElem = parsedDoc.documentElement;
        } catch (e) {
            var newDocument = this.getDocument().implementation.createHTMLDocument("Untitled");
            newDocument.body.innerHTML = svgString;
            svgElem = newDocument.body.firstChild;
        }

        var defsContainerElem = svgDocument.importNode(svgElem.childNodes.item(0), true);

        var child;
        while ((child = defsContainerElem.firstChild) != null) {
            
            svgDefs.appendChild(child);
        }
    }
},

_setupEventOnlyDrawItems : function () {
    var eventOnlyDrawItems = this._eventOnlyDrawItems;
    if (eventOnlyDrawItems != null) {
        for (var i = 0, l = eventOnlyDrawItems.length; i < l; ++i) {
            eventOnlyDrawItems[i]._setupEventParent();
        }
    }
},

//> @method drawPane.isBatchDrawing() (A)
// Is this <code>DrawPane</code> in batch drawing mode?
// @return (boolean) whether this <code>DrawPane</code> is in batch drawing mode.
// @see DrawPane.beginBatchDrawing()
// @visibility internal
//<

isBatchDrawing : function () {
    return this._batchDrawing > 0;
},

_isBatchDrawing : function () {
    return (this._batchDrawing > 0 ||
            // might as well be in batch drawing mode if the redraw TEA is scheduled because
            // drawn DrawItems are just going to be redrawn anyway.
            this._isRedrawTEAScheduled() ||
            this._isLoaded === false) &&
           this._endingBatchDrawing !== true;
},

//> @method drawPane.beginBatchDrawing() (A)
// Begins batching the drawing of draw items that have been added to this <code>DrawPane</code>
// so that all drawing of draw items can be done at the same time. If this <code>DrawPane</code>
// is already in batch drawing mode, then calling this method has no effect.
// <p>
// <strong>NOTE:</strong> For batch drawing to work properly, each call to beginBatchDrawing()
// must be matched by a corresponding call to +link{DrawPane.endBatchDrawing()}.
// <p>
// Consider using batch drawing when several draw items are going to be drawn in a single thread.
// This can be an important optimization in that scenario, especially in Internet Explorer 8
// and earlier.
// @see DrawPane.isBatchDrawing()
// @visibility internal
//<


beginBatchDrawing : function () {
    this._batchDrawing = (this._batchDrawing || 0) + 1;
},

//> @method drawPane.endBatchDrawing() (A)
// Ends batching the drawing of draw items.
// @see DrawPane.beginBatchDrawing()
// @see DrawPane.isBatchDrawing()
// @visibility internal
//<

endBatchDrawing : function (immediate) {
    if (!this.isBatchDrawing()) this.logWarn("Not in batch drawing mode. Exiting.");
    this._endBatchDrawing(immediate);
},

_endBatchDrawing : function (immediate) {
    if (!this._isBatchDrawing() || (this._batchDrawing > 0 && --this._batchDrawing > 0)) return;
    
    if (immediate == true && !this._isBatchDrawing()) {
        this.refreshNow();
    } else this._maybeScheduleRedrawTEA();
},

//> @method drawPane.refreshNow() (A)
// Immediately draws or redraws any items of this <code>DrawPane</code> that are scheduled to be
// drawn or redrawn after a delay.
// <p>
// For performance reasons, this <code>DrawPane</code> may delay refreshing its display to
// allow for multiple draw item updates to be drawn at the same time.  If this is occurring,
// refreshNow() will immediately refresh the display instead of refreshing the display in a timer.
// @visibility drawing
//<
refreshNow : function () {
    if (this.drawingType === "bitmap") {
        this.redrawBitmapNow();
    } else {
        var delayedDrawItems = this._delayedDrawItems;
        if (delayedDrawItems != null && delayedDrawItems.length > 0) {
            this._endingBatchDrawing = true;
            try {
                this._batchDraw(delayedDrawItems);
                this._delayedDrawItems = null;
            } finally {
                this._endingBatchDrawing = false;
            }
        }
    }
},

// drawPane.drawDrawItems()
drawDrawItems : function () {
    var drawItems,
        numDrawItems;
    if (!this._isBatchDrawing() &&
        (drawItems = this.drawItems) != null &&
        (numDrawItems = drawItems.length) > 0)
    {
        this._delayedDrawItems = null;

        for (var i = 0; i < numDrawItems; ++i) {
            var drawItem = drawItems[i];
            drawItem.draw();
            drawItem._updateTitleLabelAndBackground();
            drawItem._drawExemptFromGlobalTransformAutoChildren();
        }
    }
},

// drawPane.drawChildren()
// Override drawChildren to render out our drawItems
drawChildren : function () {
    this.drawDrawItems();
    this.Super("drawChildren", arguments);
},

getBitmapContext : function (saveExistingState) {
    var bitmapContext = this._bitmapContext;
    if (bitmapContext == null) {
        var bitmapHandle = this.getDocument().getElementById(this.getID() + "_bitmap");
        if (bitmapHandle == null) {
            this.logError("DrawPane failed to get CANVAS element handle");
            return null;
        }
        bitmapContext = this._bitmapContext = bitmapHandle.getContext("2d");
        if (bitmapContext == null) {
            this.logError("DrawPane failed to get CANVAS 2d bitmap context");
            return null;
        }

        // If `saveExistingState' is true, then we will be returning the context without saving,
        // but this is okay because if there is no saved state, restore() does nothing:
        // https://html.spec.whatwg.org/multipage/scripting.html#dom-context-2d-restore

    } else {
        if (saveExistingState) bitmapContext.save();
        // Reset the saved context's transform to the identity transform.
        bitmapContext.setTransform(1, 0, 0, 1, 0, 0);
    }
    return bitmapContext;
},

// drawPane._updateParentHTML()
_updateParentHTML : function () {
    this.clearDrawItems();
    this.Super("_updateParentHTML", arguments);
    this._finishUpdateInnerHTML();
},

// drawPane._updateInnerHTML()
_updateInnerHTML : function () {
    this.clearDrawItems();
    this.Super("_updateInnerHTML", arguments);
    this._finishUpdateInnerHTML();
},

// Clears cached DrawPane handles and redraws all items
_finishUpdateInnerHTML : function () {
    if (this.drawingType === "bitmap") {
        this._bitmapContext = null; // clear the cached bitmap context handle
        if (!this._isRedrawTEAScheduled()) this.redrawBitmapNow();
    } else {
        if (this.drawingType === "svg") {
            this._svgBody = null;
            this._svgBox = null;
            this._svgDefs = null;
            var batchDrawConversionContext = this._batchDrawConversionContext;
            batchDrawConversionContext.defSvgStrings = {};
            batchDrawConversionContext.useGradientIDs = {};
        }

        this.drawDrawItems();
    }
    // otherwise ignore: don't want to lose the SVG or VML DOM
    
    var logInfoEnabled = this.logIsInfoEnabled(this._$drawing);
    if (logInfoEnabled) {
        this.logInfo("Redraw called on drawPane with drawingType:" + this.drawingType);
        // Not sure theres much more info to convey here - presumably devs should use
        // APIs directly on the drawItems to "refresh" them if desired.
    }
    this.modifyContent();
    this.setUpEvents();
    this._dirty = false;
    
    this.redrawPeers();

    // return "this" for chaining calls (canvas.redraw().moveTo(..).. )
    return this;
    
},

_isRedrawTEAScheduled : function () {
    return this._redrawTEAScheduled == true ||
           (this.drawingType == "bitmap" && this._redrawPending);
},

_maybeScheduleRedrawTEA : function () {
    if (!this._isRedrawTEAScheduled()) {
        if (this.drawingType == "bitmap") this.redrawBitmap();
        else {
            var self = this;
            isc.EH._setThreadExitAction(function () {
                self._redrawTEA();
            });
            this._redrawTEAScheduled = true;
            this._ranRedrawTEA = false;
        }
    }
},

_redrawTEA : function () {
    if (this._ranRedrawTEA !== false) return;
    this._ranRedrawTEA = true;

    
    if (!this.destroyed) {
        var erasedDrawItems = this._erasedDrawItems;
        if (erasedDrawItems != null) {
            for (var i = 0, len = erasedDrawItems.length; i < len; ++i) {
                var erasedDrawItem = erasedDrawItems[i];
                if (erasedDrawItem._erasedSVGHandle) {
                    erasedDrawItem._erasedSVGHandle.parentNode.removeChild(
                        erasedDrawItem._erasedSVGHandle);
                    delete erasedDrawItem._erasedSVGHandle;
                }
            }
            this._erasedDrawItems = null;
        }

        var erasedGradients = this._erasedGradients;
        if (erasedGradients != null) {
            var svgDocument;
            if (this.drawingType === "svg" &&
                (svgDocument = this._getSvgDocument()) != null)
            {
                for (var gradientID in erasedGradients) {
                    if (!erasedGradients.hasOwnProperty(gradientID)) continue;

                    var gradient = erasedGradients[gradientID];
                    var svgDef = gradient._svgDef;
                    if (svgDef != null) gradient._svgDef = null;
                    else svgDef = svgDocument.getElementById(gradientID);

                    if (svgDef != null) {
                        svgDef.parentNode.removeChild(svgDef);
                    }
                }
            }
            this._erasedGradients = null;
        }

        this._endingBatchDrawing = true;
        this._batchDraw(this.drawItems);
        this._endingBatchDrawing = false;
    }

    this._redrawTEAScheduled = false;
},


redrawBitmap : function () {
    // defer redraw until end of current thread
    if (this._redrawPending || this._redrawingDelayedDrawItems) return;
    this._redrawPending = true;
    var self = this;
    isc.EH._setThreadExitAction(function () {
        if (!self._redrawPending) return;
        self.redrawBitmapNow();
    });
},

// @return (AffineTransform)
_getGlobalTransform : function () {
    var t = this._transform;
    if (t != null) return t;

    t = isc.AffineTransform.create();
    if (this.translate) {
        t.translate(this.translate[0] - this.scrollLeft, this.translate[1] - this.scrollTop);
    } else {
        t.translate(-this.scrollLeft, -this.scrollTop);
    }
    t.scale(this.zoomLevel, this.zoomLevel);
    if (this.rotation) {
        t.rotate(this.rotation, this._viewPortWidth / 2, this._viewPortHeight / 2);
    }
    return (this._transform = t);
},

_getInverseGlobalTransform : function () {
    var inverseTransform = this._inverseTransform,
        transform = this._getGlobalTransform();
    if (inverseTransform == null || transform != this._inverseInverseTransform) {
        inverseTransform = this._inverseTransform = transform.getInverse();
        this._inverseInverseTransform = transform;
    }
    return inverseTransform;
},

redrawBitmapNow : function (skipSetupEventOnlyDrawItems) {
    

    this._redrawPending = false;

    
    this._redrawingDelayedDrawItems = true;
    var delayedDrawItems = this._delayedDrawItems;
    if (delayedDrawItems != null) {
        
        var numDelayedDrawItems = delayedDrawItems.length,
            newDelayedDrawItems = [];
        for (var i = 0; i < numDelayedDrawItems; ++i) {
            var item = delayedDrawItems[i];
            if (this.shouldDeferDrawing(item)) {
                newDelayedDrawItems.add(item);
            }
            item.draw();
        }
        delayedDrawItems = this._delayedDrawItems = newDelayedDrawItems;
    }
    this._redrawingDelayedDrawItems = false;

    if (!this.isDrawn()) return; // clear()d during redraw delay

    var context = this.getBitmapContext();
    if (!context) return; // getBitmapContext has already logged this error
    
    context.clearRect(0, 0, this._viewPortWidth, this._viewPortHeight);

    this._drawBitmapState = {
        // Get the global translation, zoomLevel, and rotation to apply.
        _globalTransform: this._getGlobalTransform(),
        // DrawItems with `exemptFromGlobalTransform: true` need only be translated by
        // `scrollLeft` and `scrollTop`.
        _scrollLeft: this.scrollLeft,
        _scrollTop: this.scrollTop,
        _first: true
    };
    this._drawBitmapDrawItems(context, this.drawItems);
    delete this._drawBitmapState;

    if (!skipSetupEventOnlyDrawItems) this._setupEventOnlyDrawItems();

    
},

// This method is called by DrawPane.redrawBitmapNow() and DrawGroup.drawBitmap().
_drawBitmapDrawItems : function (context, drawItems) {
    var state = this._drawBitmapState;
    
    var numDrawItems = (drawItems ? drawItems.length : 0),
        first = state._first,
        exemptFromGlobalTransform = state._exemptFromGlobalTransform;

    // The default miterLimit value for a 2d context is 10:
    // http://www.w3.org/TR/2dcontext/#dom-context-2d-miterlimit
    // Set the miterLimit to 4 to match SVG: http://www.w3.org/TR/SVG/painting.html#StrokeMiterlimitProperty
    //
    // A miterlimit of 4 means that for angles less than approximately 29 degrees between
    // two line segments, the join style will switch to "bevel".
    context.miterLimit = 4;

    for (var i = 0; i < numDrawItems; ++i) {
        var drawItem = drawItems[i];
        if (state._tmpBitmapContext == null) drawItem.drawingBitmap = true;
        if (!drawItem.hidden) {
            var isDrawGroup = isc.isA.DrawGroup(drawItem);

            if (isDrawGroup) {
                state._first = first;
                state._exemptFromGlobalTransform = exemptFromGlobalTransform;
            } else if (first || exemptFromGlobalTransform != drawItem.exemptFromGlobalTransform) {
                first = false;
                exemptFromGlobalTransform = drawItem.exemptFromGlobalTransform;
                if (exemptFromGlobalTransform) {
                    // Reset the context's transform to identity.
                    context.setTransform(1, 0, 0, 1, 0, 0);
                } else {
                    var t = state._globalTransform;
                    context.setTransform(t.m00, t.m10, t.m01, t.m11, t.m02, t.m12);
                }
            }
            drawItem.drawBitmap(context);
            if (isDrawGroup) {
                first = state._first;
                exemptFromGlobalTransform = state._exemptFromGlobalTransform;
            }

            if (state._tmpBitmapContext == null) {
                drawItem._setupEventParent();
                drawItem._drawn = true;
                drawItem._drawExemptFromGlobalTransformAutoChildren();
            }
        }
    }

    state._first = first;
    state._exemptFromGlobalTransform = exemptFromGlobalTransform;
},


_tempPoint: new Array(2),
_pageToGlobal : function (x, y) {
    var pageOffsets = this.getPageOffsets(),
        point = this._tempPoint;
    point[0] = x - (pageOffsets.left + this.getLeftMargin() + this.getLeftBorderSize() + this.getLeftPadding());
    point[1] = y - (pageOffsets.top + this.getTopMargin() + this.getTopBorderSize() + this.getTopPadding());
    return point;
},

viewbox2global : function (drawRect) {
    return [
        Math.round(drawRect[0] * this.zoomLevel + this._viewBoxLeft - this.scrollLeft + this.getLeftPadding()),
        Math.round(drawRect[1] * this.zoomLevel + this._viewBoxTop - this.scrollTop + this.getTopPadding()),
        Math.round(drawRect[2] * this.zoomLevel),
        Math.round(drawRect[3] * this.zoomLevel)];
},

// Define the region within the drawing space (the "viewbox") that should be scaled to fit in the
// current visible area (the "viewport"), effectively panning and/or zooming the drawPane.
// zoom() and pan() call through this method to get the job done.
_viewBoxUpdated : function (dontNotifyExemptedDrawItems) {
    // This is the only place where the DrawPane's global transform is invalidated.
    this._transform = null;

    this._viewBoxLeft = this.translate == null ? 0 : this.translate[0];
    this._viewBoxTop = this.translate == null ? 0 : this.translate[1];
    this._viewBoxWidth = this._viewPortWidth / this.zoomLevel;
    this._viewBoxHeight = this._viewPortHeight / this.zoomLevel;

    if (!dontNotifyExemptedDrawItems) {
        // Call `_globalTransformChanged()` on all DrawItems with
        // `exemptFromGlobalTransform: true`.
        var exemptList = this._exemptedDrawItems;
        for (var i = 0, len = exemptList.length; i < len; ++i) {
            var drawItem = exemptList[i];
            if (drawItem._globalTransformChanged) {
                drawItem._globalTransformChanged();
            }
        }
    }

    var type = this.drawingType;
    if (isc.Browser.isIE && type === "vml") {
        
        this._vmlRenderer.updatedGlobalTransform();
    }

    if (!this.isDrawn()) return;

    // Update items and item properties that should not be affected by the viewbox/global transformations,
    // or that need to be corrected because of the viewbox transformation.
    this._updateItemsToViewBox();

    // Update the viewbox/global transforms.
    if (type === "vml") {
        var vmlBox = this._vmlBox;
        if (this._vmlBox == null) {
            vmlBox = this._vmlBox = this.getDocument().getElementById(this.getID() + "_vml_box");
        }
        if (vmlBox != null) {
            var pow10 = this._pow10,
                left = (this._viewBoxLeft - this.scrollLeft) + this.getLeftPadding(),
                top = (this._viewBoxTop - this.scrollTop) + this.getTopPadding(),
                vmlBoxStyle = vmlBox.style;

            
            vmlBoxStyle.visibility = "hidden";

            
            var theta = (this.rotation || 0) * Math.PI / 180,
                sin = Math.sin(theta), cos = Math.cos(theta);
            vmlBoxStyle.left = Math.round(pow10 * (cos * left - sin * top)) + this._$px;
            vmlBoxStyle.top = Math.round(pow10 * (sin * left + cos * top)) + this._$px;

            vmlBox.coordsize = (
                (pow10 * this._viewBoxWidth) + "," + (pow10 * this._viewBoxHeight));
            vmlBoxStyle.visibility = "visible";
        }
    } else if (type === "svg") {
        if (this._svgBox != null) {
            var t = this._getGlobalTransform();
            this._svgBox.setAttributeNS(null, "transform", "matrix(" + t.m00 + " " + t.m10 + " " + t.m01 + " " + t.m11 + " " + t.m02 + " " + t.m12 + ")");
        }
    } else {
        
        this.redrawBitmap();
    }
},

_$defaultViewPortEdgeOffsets: {
    top: 0,
    right: 0,
    bottom: 0,
    left: 0
},
_getViewPortEdgeOffsets : function (printForExport) {
    return this._$defaultViewPortEdgeOffsets;
},

_getViewPortWidth : function () {
    var edgeOffsets = this._getViewPortEdgeOffsets();
    return this.getInnerContentWidth() - edgeOffsets.right - edgeOffsets.left;
},

_getViewPortHeight : function () {
    var edgeOffsets = this._getViewPortEdgeOffsets();
    return this.getInnerContentHeight() - edgeOffsets.top - edgeOffsets.bottom;
},

_updateViewPort : function () {
    var width = this._getViewPortWidth(),
        height = this._getViewPortHeight();

    this._viewPortWidth = width;
    this._viewPortHeight = height;
    this.scrollLeft = Math.max(0, Math.min(this.scrollLeft, this.getScrollRight()));
    this.scrollTop = Math.max(0, Math.min(this.scrollTop, this.getScrollBottom()));
    this._viewBoxWidth = width / this.zoomLevel;
    this._viewBoxHeight = height / this.zoomLevel;

    if (!this.isDrawn()) return;

    this._updateItemsToViewBox();

    var type = this.drawingType;
    if (type === "vml") {
        var vmlBox = this._vmlBox;
        if (this.vmlBox == null) {
            vmlBox = this._vmlBox = this.getDocument().getElementById(this.getID() + "_vml_box");
        }
        if (vmlBox != null) {
            var pow10 = this._pow10;
            vmlBox.style.width = (pow10 * width) + this._$px;
            vmlBox.style.height = (pow10 * height) + this._$px;
            vmlBox.coordsize = (
                (this._viewBoxWidth * pow10) + "," + (this._viewBoxHeight * pow10));
        }

    } else if (type === "svg") {
        var svgBody = this._svgBody;
        if (svgBody != null) {
            if (!this._useSvgHelper) {
                svgBody.setAttributeNS(null, "width", width + "px");
                svgBody.setAttributeNS(null, "height", height + "px");
            }
            svgBody.setAttributeNS(null, "viewBox", "0 0 " + width + " " + height);
        }

    } else {
        
        var canvas = this.getDocument().getElementById(this.getID() + "_bitmap");
        if (canvas != null) {
            canvas.width = width;
            canvas.height = height;
        }
        this.redrawBitmap();
    }
},

// Embed a canvas in this DrawPane, so it zooms/pans/clips in synch with shapes in the vector space.
// We assume that the original rect (left/top/width/height properties) of this canvas has been
// set in drawing coordinates, so we apply the current zoom and pan immediately.
// TODO support canvasItems in drawGroups
// TODO patch coordinate setters (setLeft, moveBy, etc) so they work with drawing coords?
//      or implement setDrawingLeft(), etc?
//      Currently we've worked around a single case: In DrawKnobs we supply a custom
//      setCenterPoint() method which can take global OR viewbox coordinates.
//
// NOTE: Any Canvas can be added to a drawPane as a canvasItem (it's not a custom Canvas class, and
// doesn't necessarily have any associated content rendered out as drawItems. 
// By adding a canvas to a drawPane as a canvasItem you're essentially
// just causing the canvas to be notified when the drawPane zooms / pans.

addCanvasItem : function (item) {
    // crosslink
    this.canvasItems.add(item);
    item.drawPane = this;
    // save original position and size settings
    item._drawingLeft = item.left;
    item._drawingTop = item.top;
    item._drawingWidth = item.width;
    item._drawingHeight = item.height;
    // add as child, for local coordinates and clipping
    this.addChild(item);
    // update for current zoom & pan
    this._updateCanvasItemToViewBox(item);
    
    return item;
},

removeCanvasItem : function (item) {
    if (this.canvasItems != null) this.canvasItems.remove(item);
},

_getGradientSvgString : function (id, gradient, conversionContext, drawItem) {
    if (isc.DrawItem._isSimpleGradient(gradient)) {
        return this._getSimpleGradientSvgString(id, gradient, conversionContext, drawItem);
    } else if (isc.DrawItem._isLinearGradient(gradient)) {
        return this._getLinearGradientSvgString(id, gradient, conversionContext, drawItem);
    } else {
        
        return this._getRadialGradientSvgString(id, gradient, conversionContext, drawItem);
    }
},

_getSimpleGradientSvgString : function (id, simpleGradient, conversionContext, drawItem) {
    var colorStops;
    if (simpleGradient.startColor && simpleGradient.endColor) {
        colorStops = [{
            color: simpleGradient.startColor,
            offset: 0.0
        }, {
            color: simpleGradient.endColor,
            offset: 1.0
        }];
    } else {
        colorStops = simpleGradient.colorStops;
    }
    return this._getLinearGradientSvgString(id, {
        id: simpleGradient[conversionContext.gradientIdProperty],
        direction: simpleGradient.direction,
        colorStops: colorStops
    }, conversionContext, drawItem);
},


_getLinearGradientSvgString : function (id, linearGradient, conversionContext, drawItem) {
    var defSvgStrings = conversionContext.defSvgStrings || (conversionContext.defSvgStrings = {}),
        baseGradientID = id,
        baseGradientDefID = this.getID() + "_" + baseGradientID;
    if (!defSvgStrings[baseGradientDefID]) {
        var svgString = "<linearGradient id='" + baseGradientDefID + "'>";
        if (linearGradient.startColor != null && linearGradient.endColor != null) {
            svgString += (
                "<stop stop-color='" + linearGradient.startColor + "' offset='0' stop-opacity='1'></stop>" +
                "<stop stop-color='" + linearGradient.endColor + "' offset='1' stop-opacity='1'></stop>"
                );
        } else if (isc.isAn.Array(linearGradient.colorStops)) {
            for (var i = 0; i < linearGradient.colorStops.length; ++i) {
                var colorStop = linearGradient.colorStops[i],
                opacity = colorStop.opacity || "1";
                svgString += (
                    "<stop stop-color='" + colorStop.color +
                    "' offset='" + colorStop.offset +
                    "' stop-opacity='" + opacity + "'></stop>"
                    );
            }
        }
        svgString += "</linearGradient>";
        defSvgStrings[baseGradientDefID] = svgString;
    }

    id = conversionContext.getNextSvgDefID();
    var useGradientID = conversionContext.useGradientIDs[drawItem.ID] = this.getID() + "_" + id,
        xlinkPrefix = (conversionContext.xlinkPrefix||isc.SVGStringConversionContext._$xlink),
        vector = drawItem._normalizeLinearGradient(linearGradient);
    return (
        "<linearGradient id='" + useGradientID +
        "' " + xlinkPrefix + ":href='#" + this.getID() + "_" + baseGradientID +
        "' x1='" + vector[0] +
        "' y1='" + vector[1] +
        "' x2='" + vector[2] +
        "' y2='" + vector[3] +
        "' gradientUnits='userSpaceOnUse'>" +
        
        "</linearGradient>"
        );
},


_getRadialGradientSvgString : function (id, radialGradient, conversionContext, drawItem) {
    var defSvgStrings = conversionContext.defSvgStrings || (conversionContext.defSvgStrings = {}),
        baseGradientID = id,
        baseGradientDefID = this.getID() + "_" + baseGradientID,
        svgString,
        xlinkPrefix = conversionContext.xlinkPrefix || isc.SVGStringConversionContext._$xlink;
    if (!defSvgStrings[baseGradientDefID]) {
        svgString = "<radialGradient id='" + baseGradientDefID;
        if (radialGradient._baseGradient != null) {
            var baseGradientID2, baseGradient;
            if (isc.isA.String(radialGradient._baseGradient)) {
                baseGradientID2 = radialGradient._baseGradient;
                baseGradient = this._gradientMap[baseGradientID2];
            } else {
                baseGradient = radialGradient._baseGradient;
                baseGradientID2 = baseGradient[conversionContext.gradientIdProperty];
            }
            var baseGradientDefID2 = this.getID() + "_" + baseGradientID2;
            if (!defSvgStrings[baseGradientDefID2]) {
                defSvgStrings[baseGradientDefID2] = this._getRadialGradientSvgString(baseGradientID2, baseGradient, conversionContext);
            }
            svgString += "' " + xlinkPrefix + ":href='#" + baseGradientDefID2;
        }
        svgString += "'>";
        if (radialGradient.colorStops != null) {
            for (var i = 0; i < radialGradient.colorStops.length; ++i) {
                var colorStop = radialGradient.colorStops[i],
                    opacity = colorStop.opacity || "1";
                svgString += "<stop stop-color='" + colorStop.color + "' offset='" + colorStop.offset + "' stop-opacity='" + opacity + "'></stop>";
            }
        }
        svgString += "</radialGradient>";
        defSvgStrings[baseGradientDefID] = svgString;
    }

    // `drawItem' can be null if a _baseGradient is set on some RadialGradient config (see the
    // call to _getRadialGradientSvgString() a few lines up which does not pass a DrawItem reference).
    if (drawItem != null) {
        id = conversionContext.getNextSvgDefID();
        var useGradientID = conversionContext.useGradientIDs[drawItem.ID] = this.getID() + "_" + id,
            vector = drawItem._normalizeRadialGradient(radialGradient);
        svgString = "<radialGradient id='" + useGradientID +
                "' " + xlinkPrefix + ":href='#" + baseGradientDefID;
        if (radialGradient.cx != null) svgString += "' cx='" + vector[0];
        if (radialGradient.cy != null) svgString += "' cy='" + vector[1];
        if (radialGradient.fx != null) svgString += "' fx='" + vector[3];
        if (radialGradient.fy != null) svgString += "' fy='" + vector[4];
        if (radialGradient.r != null) svgString += "' r='" + vector[5];
        svgString += "' gradientUnits='userSpaceOnUse'></radialGradient>";
    }

    return svgString;
},

_getFilterSvgString : function (id) {
    if (id == "isc_ds1") {
        return "<filter id='isc_ds1'><feOffset result='offOut' in='SourceAlpha' dx='2.4' dy='2.4'/><feGaussianBlur result='blurOut' in='offOut' stdDeviation='1.8'/><feColorMatrix result='cmatOut' in='blurOut' type='matrix' values='1 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 0.8 0'/><feBlend in='SourceGraphic' in2='cmatOut' mode='normal'/></filter>";
    }
    return null;
},


// move and scale an associated canvas to match the current drawPane zoom & pan
// called when a canvasItem is added to this drawPane, and when the drawPane is zoomed or panned
// will also need to call this when a canvasItem is moved or resized
_updateCanvasItemToViewBox : function (item) {
    // synchingToPane flag - This is required for DrawKnobs - it essentially notifies the 
    // canvas that the drawItem (the knobShape) doesn't need to be moved in response to the
    // canvas move.
    item.synchingToPane = true;
    item.setRect(this.viewbox2global([
        item._drawingLeft,
        item._drawingTop,
        item._drawingWidth,
        item._drawingHeight
    ]));
    // HACK also scale border width, assuming uniform px borders all around
    // TODO consider how to structure this via shared CSS classes for faster updates
    // Note: we could get the initial border width from parseInt(item.getStyleHandle().borderWidth),
    // but we need some flag anyway to selectively enable border scaling (no point doing
    // this for eg canvasitems that are used as invisible hotspots), so we just use drawBorderSize
    // as a pixel width here
    if (item.drawBorderSize) {
        // floor of 1px prevents borders from disappearing entirely in IE
        item.getStyleHandle().borderWidth = Math.max(1, item.drawBorderSize * this.zoomLevel) + "px";
    }
    delete item.synchingToPane;
},

// Update items and item properties that are not automatically updated by the viewbox
// transformation, or that must be corrected because of the viewbox transformation.
// Currently:
//  - move/resize canvasItems
//  - move/scale text for VML drawpanes
//  - scale lines for VML drawpanes
// TODO/future:
//  - prevent scaling of hairlines (always 1px) in SVG
//  - prevent scaling of fixed-size labels in SVG
//  - enforce minimum arrowhead sizes
// NOTE: must also account for the viewbox when elements are drawn or updated, so currently in:
//      drawPane.addCanvasItem()
//      drawItem._getElementVML()
//      drawItem.setLineWidth()
//      drawLabel._getElementVML()
//      drawLabel.setFontSize()      
// TODO recurse through DrawGroups - this only hits the top level
_updateItemsToViewBox : function () {
    // update canvasItems - see addCanvasItem()
    var canvasItems = this.canvasItems;
    for (var i = 0; i < canvasItems.length; ++i) {
        this._updateCanvasItemToViewBox(canvasItems[i]);
    }

    var prevZoomLevel = this._prevZoomLevel,
        zoomLevel = this._prevZoomLevel = this.zoomLevel,
        zoomLevelChanged = (prevZoomLevel != zoomLevel),
        drawItems = this.drawItems;

    // scalable text and linewidths for VML
    if (this.drawingType === "vml") {
        for (var i = 0; i < drawItems.length; ++i) {
            var di = drawItems[i];
            if (!di.drawingVML) continue;

            if (isc.isA.DrawLabel(di) && !di.useTextpath) {
                // TODO hide these attributes behind DrawLabel setters
                // scale font size
                //  alternate approach - could just change the CSS class, eg:
                //  isc.Element.getStyleDeclaration("drawLabelText").fontSize = 24 * zoomLevel;
                if (zoomLevelChanged) di._getVMLTextHandle().fontSize = (di.fontSize * zoomLevel) + "px";
                if (!di.synchTextMove) {
                    // using higher-performance external html DIVs - but need to move them ourselves
                    var screenCoords = this.viewbox2global([di.left, di.top, 0, 0]); // width/height not used here - labels overflow
                    var vmlHandleStyle = di._getVMLHandle().style;
                    vmlHandleStyle.left = screenCoords[0] + isc.px;
                    vmlHandleStyle.top = screenCoords[1] + isc.px;
                }
            } else if (zoomLevelChanged) {
                // scale linewidths (TODO call this on DrawShapes only, when DrawShape has been factored out)
                di._setLineWidthVML(di.lineWidth * zoomLevel);
            }
        }

    // update the CSS3 transform of "htmlText" DrawLabels
    } else if (this.drawingType === "bitmap") {
        for (var i = 0; i < drawItems.length; ++i) {
            var di = drawItems[i];
            if (isc.isA.DrawLabel(di) && di._htmlText != null) {
                isc.Element._updateTransformStyle(di._htmlText, di._htmlText._getTransformFunctions());
            }
        }
    }
},

getScrollLeft : function () {
    return this.scrollLeft;
},

getScrollRight : function () {
    return Math.max(0, this.drawingWidth * this.zoomLevel - this._viewPortWidth) << 0;
},

getScrollTop : function () {
    return this.scrollTop;
},

getScrollBottom : function () {
    return Math.max(0, this.drawingHeight * this.zoomLevel - this._viewPortHeight) << 0;
},

scrollTo : function (left, top, reason) {
    if (left == null) left = this.scrollLeft;
    if (top == null) top = this.scrollTop;

    left = Math.max(0, Math.min(left, this.getScrollRight()));
    top = Math.max(0, Math.min(top, this.getScrollBottom()));

    if (this.scrollLeft != left || this.scrollTop != top) {
        this.scrollLeft = left;
        this.scrollTop = top;
        this._viewBoxUpdated();
    }
},

// Global Pan
//  anticipated panning UIs:
//      - draggable viewbox outline in a thumbnail view -- drag&drop or realtime
//      - hand tool -- drag outlines or realtime
//      - arrow keys/buttons - press to pan by steps
//  so panRight and panDown are *deltas in screen (pixel) coordinates*
pan : function (panRight, panDown) {
    var translate = this.translate;
    if (translate == null) translate = this.translate = [0, 0];
    translate[0] -= panRight;
    translate[1] -= panDown;
    if (panRight != 0 || panDown != 0) {
        this._viewBoxUpdated();
    }
},

//> @method drawPane.setZoomLevel()
// Sets the zoom on this <code>DrawPane</code> to the specified magnification, maintaining the
// current viewport position.
//
// @param zoomLevel (float) Desired zoom level as a float where <code>1.0</code> is equivalent
// to 100% magnification. Must be greater than 0.
// @visibility drawing
//<

setZoomLevel : function (zoomLevel) {
    if (!(zoomLevel > 0)) { // this way catches NaN, null, undefined
        this.logWarn("The requested zoomLevel:" + zoomLevel + " is not greater than 0.");
        return;
    }

    var prevZoomLevel = this.zoomLevel;
    if (prevZoomLevel == zoomLevel) return;
    this.zoomLevel = zoomLevel;
    var newScrollLeft = Math.max(0, Math.min(this.scrollLeft, this.getScrollRight())),
        newScrollTop = Math.max(0, Math.min(this.scrollTop, this.getScrollBottom()));

    if (zoomLevel != prevZoomLevel ||
        this.scrollLeft != newScrollLeft || this.scrollTop != newScrollTop)
    {
        this.scrollLeft = newScrollLeft;
        this.scrollTop = newScrollTop;

        if (isc.Browser.isIE && this.drawingType == "vml" && this.isDrawn()) {
            var globalSkew = this._getGlobalRotationSkewMatrixAndOffset(),
                vmlShapetype = isc.Element.get(this.getID() + "_global_rotation_zoom");
            
            var vmlSkew = vmlShapetype.firstChild;
            vmlSkew.matrix = globalSkew.zoomMatrix;
            vmlSkew.offset = globalSkew.zoomOffset;
        }

        this._viewBoxUpdated();
    }
},

//> @method drawPane.zoom()
// Synonym of +link{DrawPane.setZoomLevel()}.
// @include DrawPane.setZoomLevel()
//<
zoom : function (zoomLevel) {
    this.setZoomLevel(zoomLevel);
},

//> @method drawPane.setRotation()
// Sets the +link{DrawPane.rotation,rotation} of the <code>DrawPane</code>.
//
// @param degrees (double) the new rotation in degrees. The positive direction corresponds to
// clockwise rotation.
// @visibility drawing
//<
setRotation : function (degrees) {
    if (this.rotation != degrees) {
        this.rotation = degrees;

        if (isc.Browser.isIE && this.drawingType == "vml" && this.isDrawn()) {
            var globalSkew = this._getGlobalRotationSkewMatrixAndOffset(),
                vmlShapetype1 = isc.Element.get(this.getID() + "_global_rotation"),
                vmlShapetype2 = isc.Element.get(this.getID() + "_global_rotation_zoom");
            
            var vmlSkew1 = vmlShapetype1.firstChild,
                vmlSkew2 = vmlShapetype2.firstChild;
            vmlSkew1.matrix = globalSkew.matrix;
            vmlSkew1.offset = globalSkew.offset;
            vmlSkew2.matrix = globalSkew.zoomMatrix;
            vmlSkew2.offset = globalSkew.zoomOffset;
        }
        this._viewBoxUpdated();
    }
},

//> @method drawPane.rotate()
// Synonym of +link{DrawPane.setRotation()}.
// @include DrawPane.setRotation()
//<
rotate : function (degrees) {
    this.setRotation(degrees);
},

//> @method drawPane.addGradient()
// Add a new gradient to the drawPane shared gradient list (+link{drawPane.gradients}). If the
// gradient does not have an ID a new one will be assigned.
//
// @param gradient (Gradient) gradient to add
// @return (identifier) the ID of the gradient (either provided or auto-assigned)
// @visibility drawing
//<
_nextGradientNum: 0,
addGradient : function (gradient) {
    var id = gradient.id;
    if (!id) {
        id = gradient.id = "_gradient" + this._nextGradientNum++;
    }
    if (!gradient._constructor) gradient._constructor = isc.DrawItem._getGradientConstructor(gradient);

    var gradients = this.gradients,
        gradientMap = this._gradientMap,
        existingGradient = gradientMap[gradient.id];
    if (existingGradient === gradient) {
        return id;
    } else if (existingGradient != null) {
        this.logWarn("Duplicate gradient with ID " + gradient.id + " - replacing previous gradient");
        
        gradients.remove(existingGradient);
    }

    
    gradients.add(gradient);
    gradientMap[id] = gradient;
    return id;
},

//> @method drawPane.getGradient()
// Returns gradient for gradientID.
//
// @param gradientID (identifier) ID of gradient to retrieve
// @return (Gradient) the gradient or null if not found
//@visibility drawing
//<
getGradient : function (gradientID) {
    var res = (this.gradients ? this._gradientMap[gradientID] : null);
    if (res) {
        this._normalizeRelativeGradient(res);
    }
    return res;
},

/**
 * In case when a startColor, endColor or colorStop color starts with "-" or "+" and is in the
 * "color offset" format expected by Drawing.mixrgb(), we check whether either startColor,
 * endColor, or any other colorStop in the gradient (starting from the beginning) specifies a
 * color that is not an offset, and use that color as the base color
 * We do this conversion once whenever the gradient is first used, and store the calculated
 * color on the gradient object
 * If the calculated color appears in a color stop, this method don't modifies the color stop,
 * instead it makes a copy of the color stop Object and duplicate() the containing Array or
 * color stops, and stores the modified list of color stops on the gradient
 * @param gradient - gradient to normalize colors
 */
_normalizeRelativeGradient : function (gradient) {
    if (gradient._normalized) return;
    gradient._normalized = true;
    var needToNormalize = false;
    var needToChangeColorStops = false;
    var baseColor = null;
    // check in order: startColor, endColor, colorStops - looking for not-relative color and
    // at least one relative color
    if (gradient.startColor) {
        if (gradient.startColor.startsWith("+") || gradient.startColor.startsWith("-")) {
            needToNormalize = true;
        } else if (!baseColor) {
            baseColor = gradient.startColor;
        }
    }
    if (gradient.endColor) {
        if (gradient.endColor.startsWith("+") || gradient.endColor.startsWith("-")) {
            needToNormalize = true;
        } else if (!baseColor) {
            baseColor = gradient.endColor;
        }
    }
    if (gradient.colorStops) {
        for (var i = 0; i < gradient.colorStops.length; i++) {
            var color = gradient.colorStops[i].color;
            if (color.startsWith("+") || color.startsWith("-")) {
                needToNormalize = true;
                needToChangeColorStops = true;
            } else if (!baseColor) {
                baseColor = color;
            }
        }
    }
    // if no colors that looks like relative colors or if all of them looks like relative do
    // nothing
    if (!needToNormalize || !baseColor) return;
    if (gradient.startColor && (gradient.startColor.startsWith("+") || gradient.startColor.startsWith("-"))) {
        try {
            gradient.startColor = isc.DrawPane.mixrgb(baseColor, gradient.startColor);
        } catch (error) {
            isc.logWarn("Unable to normalize relative startColor " + gradient.startColor, error); 
        }
    }
    if (gradient.endColor && (gradient.endColor.startsWith("+") || gradient.endColor.startsWith("-"))) {
        try {
            gradient.endColor = isc.DrawPane.mixrgb(baseColor, gradient.endColor);
        } catch (error) {
            isc.logWarn("Unable to normalize relative endColor " + gradient.endColor, error); 
        }
    }
    if (needToChangeColorStops) {
        gradient.colorStops = gradient.colorStops.duplicate();
        for (var i = 0; i < gradient.colorStops.length; i++) {
            var color = gradient.colorStops[i].color;
            if (color.startsWith("+") || color.startsWith("-")) {
                try {
                    gradient.colorStops[i].color = isc.DrawPane.mixrgb(baseColor, color);
                } catch (error) {
                    isc.logWarn("Unable to normalize relative color " + color, error); 
                }
            }
        }
    }
},

//> @method drawPane.removeGradient()
// Removes gradient for gradientID.
//
// @param gradientID (identifier) ID of gradient to remove
// @visibility drawing
//<
removeGradient : function (gradientID) {
    var gradientMap = this._gradientMap,
        gradient = gradientMap[gradientID];
    if (gradient != null) {
        delete gradientMap[gradientID];
        this.gradients.remove(gradient);

        if (this.drawingType === "svg") {
            var gradientDefID = this.getID() + "_" + gradientID;
            var svgDocument = this._getSvgDocument();
            if (svgDocument != null) {
                var existingDef = svgDocument.getElementById(gradientDefID);
                if (existingDef != null) {
                    existingDef.parentNode.removeChild(existingDef);
                }
            }
            var batchDrawConversionContext = this._batchDrawConversionContext;
            delete batchDrawConversionContext.defSvgStrings[gradientDefID];
        }
    }
},

//> @method drawPane.createSimpleGradient()
// Creates a simple linear gradient which can be used by any DrawItem of this DrawPane.
// Any DrawItem's +link{DrawItem.fillGradient,fillGradient} can reference the gradient by the
// given ID.
//
// @param id (identifier) the ID of the simple linear gradient
// @param simple (SimpleGradient) the simple linear gradient
// @return (identifier) id
// @deprecated in favor of +link{drawPane.addGradient}
// @visibility drawing
//<
createSimpleGradient : function(id, simple) {
    simple.id = id;
    this.addGradient(simple);
    return id;
},

//> @method drawPane.createLinearGradient()
// Creates a linear gradient which can be used by any DrawItem of this DrawPane.
// Any DrawItem's +link{DrawItem.fillGradient,fillGradient} can reference the gradient by the
// given ID.
//
// @param id (identifier) the ID of the linear gradient
// @param linearGradient (LinearGradient) the linear gradient
// @return (identifier) id
// @deprecated in favor of +link{drawPane.addGradient}
// @visibility drawing
//<
createLinearGradient : function(id, linearGradient) {
    linearGradient.id = id;
    this.addGradient(linearGradient);
    return id;
},

//> @method drawPane.createRadialGradient()
// Creates a radial gradient which can be used by any DrawItem of this DrawPane.
// Any DrawItem's +link{DrawItem.fillGradient,fillGradient} can reference the gradient by the
// given ID.
//
// @param id (identifier) the ID of the radial gradient
// @param radialGradient (RadialGradient) the radial gradient
// @return (identifier) id
// @deprecated in favor of +link{drawPane.addGradient}
// @visibility drawing
//<
createRadialGradient : function(id, radialGradient) {
    radialGradient.id = id;
    this.addGradient(radialGradient);
    return id;
},

// shouldDeferDrawing() - check for the case where the drawPane is not yet ready to render out
// content, and set up a deferred draw operation here
shouldDeferDrawing : function (item) {
    var type = this.drawingType;
    if (this._isBatchDrawing() ||
        (type === "svg" && this._getSvgBody() == null) ||
        (type === "vml" && this.getHandle() == null))
    {
        var delayedDrawItems = this._delayedDrawItems;
        if (delayedDrawItems == null) {
            delayedDrawItems = this._delayedDrawItems = [item];
        } else {
            if (!delayedDrawItems.contains(item)) delayedDrawItems.add(item);
        }
        return true;
    }
    return false;
},

cancelDeferredDraw : function (item) {
    var delayedDrawItems = this._delayedDrawItems;
    if (delayedDrawItems != null) {
        return delayedDrawItems.remove(item);
    }
    return false;
},

// TODO execute deferred draw operations here
// TODO remove these props from DrawItem - can get them via drawItem.drawPane
svgLoaded : function () {
    var svgDocument = this._svgDocument = this.getHandle().firstChild.contentDocument; // svg helper doc in iframe
    this._svgBody = svgDocument.getElementById("isc_svg_body");
    this._svgBox = svgDocument.getElementById("isc_svg_box");
    this._svgDefs = svgDocument.getElementById("isc_svg_defs"); // defs element (container for arrowhead markers, gradients, and filters)

    this._updateViewPort();

    if (this._delayedDrawItems != null) {
        if (!this._isRedrawTEAScheduled()) this._endBatchDrawing(true);
    }
    this._isLoaded = true;
}


}); // end DrawPane.addProperties

isc.DrawPane.addClassProperties({
    defaultDrawingType: isc.Browser.defaultDrawingType,

    // DrawPane class provides default fullscreen DrawPane instances for each drawingType,
    // which are used by DrawItems that are not provided with a DrawPane
    _defaultDrawPanes: [],
    getDefaultDrawPane : function (type) {
        if (!type) type = this.defaultDrawingType;
        if (this._defaultDrawPanes[type]) return this._defaultDrawPanes[type];
        var ddp = this._defaultDrawPanes[type] = this.create({
            drawingType: type,
            // initialize container size to the display rect of the page
            // see (ISSUE: CANVAS element clipping) above
            // this size will be passed through to the CANVAS or SVG element
            width: isc.Page.getScrollWidth(),
            height: isc.Page.getScrollHeight(),
            autoDraw: true
        });
        ddp.sendToBack();
        return ddp;
    },

    
    _spaceCharsRegExp: /[\u0009\u000C\u000D]/g,

    
    _getEnclosingQuote : function (fontFamily) {
        return (isc.isA.String(fontFamily) && fontFamily.contains("\"") ? "'" : "\"");
    },

    _getMeasureCanvas : function () {
        var measureCanvas = this._measureCanvas;
        if (measureCanvas == null) {
            measureCanvas = this._measureCanvas = isc.Canvas.create({
                top: -1000,
                overflow: "visible",
                autoDraw: false,
                height: 1,
                width: 1,
                
                ariaState: {
                    hidden: true
                },
                markForRedraw : isc.Class.NO_OP
            });
            measureCanvas.draw();
        }
        
        return measureCanvas;
    },

    // color helper functions
    addrgb : function (a, b) {
        
        var mask = 0xff,
            ca = parseInt(a.substr(1), 16) >>> 0,
            cb = parseInt(b.substr(1), 16) >>> 0,
            red = Math.min(mask, ((ca >>> 16) & mask) + ((cb >>> 16) & mask)) >>> 0,
            green = Math.min(mask, ((ca >>> 8) & mask) + ((cb >>> 8) & mask)) >>> 0,
            blue = Math.min(mask, (ca & mask) + (cb & mask)) >>> 0,
            sum = (red << 16) | (green << 8) | blue;
        return "#" + isc.NumberUtil._stringify(6, false, sum, 16);
    },
    subtractrgb : function (a, b) {
        
        var mask = 0xff,
            ca = parseInt(a.substr(1), 16) >>> 0,
            cb = parseInt(b.substr(1), 16) >>> 0,
            red = Math.max(0, ((ca >>> 16) & mask) - ((cb >>> 16) & mask)) >>> 0,
            green = Math.max(0, ((ca >>> 8) & mask) - ((cb >>> 8) & mask)) >>> 0,
            blue = Math.max(0, (ca & mask) - (cb & mask)) >>> 0,
            diff = (red << 16) | (green << 8) | blue;
        return "#" + isc.NumberUtil._stringify(6, false, diff, 16);
    },
    mixrgb: function(a, b){
        return b.charAt(0) === '+' ? isc.DrawPane.addrgb(a,b.substring(1)) : b.charAt(0) === '-' ? isc.DrawPane.subtractrgb(a,b.substring(1)) : b;
    },
    // _mutergb() "mutes" colors by shifting them a given percentage toward white (or for
    // negative percentages, toward black).
    _mutergb : function (colorMutePercent, color) {
        

        // Convert from RGB to HSL, adjust the lightness according to colorMutePercent, then convert back to RGB.
        // See:  http://en.wikipedia.org/wiki/HSL_and_HSV

        var mask = 0xff,
            colorValue = parseInt(color.substr(1), 16) >>> 0,
            cr = ((colorValue >>> 16) & mask) / mask,
            cg = ((colorValue >>> 8) & mask) / mask,
            cb = (colorValue & mask) / mask,
            min = Math.min(cr, cg, cb),
            max = Math.max(cr, cg, cb),
            h, s, l = (min + max) / 2;

        if (min == max) {
            h = s = 0;
        } else {
            var c = max - min;
            s = c / (2 * (l < 0.5 ? l : 1 - l));
            if (max == cr) {
                h = (cg - cb) / c;
                if (cg < cb) {
                    h += 6;
                }
            } else if (max == cg) {
                h = (cb - cr) / c + 2;
            } else {
                h = (cr - cg) / c + 4;
            }
            h /= 6;
        }

        

        if (colorMutePercent <= 0) {
            l *= (100 + colorMutePercent) / 100;
        } else {
            l += (colorMutePercent / 100) * (1 - l);
        }

        

        var red, green, blue;
        if (s == 0) {
            red = green = blue = Math.max(0, Math.min(mask, Math.round(mask * l))) >>> 0;
        } else {
            var high = l + s * (l < 0.5 ? l : 1.0 - l),
                low = 2 * l - high,
                h0 = (3 * h - Math.floor(3 * h)) / 3,
                mid = high - 6 * (high - low) * Math.abs(h0 - 1/6),
                r, g, b;
            switch (Math.floor(h * 6)) {
            case 0:
            case 6:  r = high; g = mid;  b = low;  break;
            case 1:  r = mid;  g = high; b = low;  break;
            case 2:  r = low;  g = high; b = mid;  break;
            case 3:  r = low;  g = mid;  b = high; break;
            case 4:  r = mid;  g = low;  b = high; break;
            case 5:  r = high; g = low;  b = mid;  break;
            }

            red = Math.max(0, Math.min(mask, Math.round(mask * r))) >>> 0;
            green = Math.max(0, Math.min(mask, Math.round(mask * g))) >>> 0;
            blue = Math.max(0, Math.min(mask, Math.round(mask * b))) >>> 0;
        }

        var muted = ((red << 16) | (green << 8) | blue);
        return "#" + isc.NumberUtil._stringify(6, false, muted, 16);
    },
    hex2rgb: function(hex,opacity) {
        var hexValue = hex.split('#')[1];
        var hexValueLength = hexValue.length/3;
        var redHex = hexValue.substring(0,hexValueLength);
        var greenHex = hexValue.substring(hexValueLength,hexValueLength*2);
        var blueHex = hexValue.substring(hexValueLength*2,hexValueLength*3);
        var red = parseInt(redHex.toUpperCase(),16);
        var green = parseInt(greenHex.toUpperCase(),16);
        var blue = parseInt(blueHex.toUpperCase(),16);
        var rgb = (typeof(opacity) !== 'undefined') ? 'rgba('+red+','+green+','+blue+','+opacity+')': 'rgb('+red+','+green+','+blue+')';
        return rgb;
    },
    rgb2hex: function(value){
        var hex = "", v, i;
        var regexp = /([0-9]+)[, ]+([0-9]+)[, ]+([0-9]+)/;
        var h = regexp.exec(value);
        for (i = 1; i < 4; i++) {
          v = parseInt(h[i],10).toString(16);
          if (v.length == 1) {
            hex += "0" + v;
          } else {
            hex += v;
          }
        }
        return ("#" + hex);
    },

    //> @classMethod drawPane.scaleAndCenter() 
    // Computes the top-, left-, bottom-, and right-most coordinates in a list
    // of points, then translates and scales all points to fit the entire shape
    // into the given width and height.
    // <smartclient>
    // <p>
    // The example call below scales a set of points into a 100x100 thumbnail:
    //
    // <pre>
    //    var scaledPoints = DrawPane.scaleAndCenter(100, 100, 50, 50,
    //            [[500, 50], [525, 50], [550, 75], [575, 75],
    //             [600, 75], [600, 125], [575, 125], [550, 125],
    //             [525, 150], [500, 150]]);
    // </pre>
    // </smartclient>
    // @param width (int) width of target space
    // @param height (int) height of target space
    // @param xc (int) center point x
    // @param yc (int) center point y
    // @param points (Array of Point) list of points to scale and translate
    // @visibility drawing
    //<
    scaleAndCenter : function (width, height, xc, yc, points) {
        var n = points.length,
            minX = points[0][0],
            maxX = minX,
            minY = points[0][1],
            maxY = minY;

        for (var i = 1; i < n; ++i) {
            var x = points[i][0],
                y = points[i][1];
            minX = Math.min(minX, x);
            maxX = Math.max(maxX, x);
            minY = Math.min(minY, y);
            maxY = Math.max(maxY, y);
        }

        var x0 = (minX + maxX) / 2,
            y0 = (minY + maxY) / 2,
            scaleX = width / (maxX - minX),
            scaleY = height / (maxY - minY);

        for (var i = 0; i < n; ++i) {
            var point = points[i];
            point[0] = isc.DrawItem._makeCoordinate(xc + scaleX * (point[0] - x0));
            point[1] = isc.DrawItem._makeCoordinate(yc + scaleY * (point[1] - y0));
        }
        return points;
    },

    //> @classMethod drawPane.bezier()
    // Computes a cubic Be&#769;zier curve polynomial:
    // <code>
    // B(t) = (1 - t)<sup>3</sup>P<sub>1</sub> + 3(1 - t)<sup>2</sup>tCP<sub>1</sub> + 3(1 - t)t<sup>2</sup>CP<sub>2</sub> + t<sup>3</sup>P<sub>2</sub>
    // </code>
    // @param p1 (double) starting point coordinate
    // @param cp1 (double) first control point coordinate
    // @param cp2 (double) second control point coordinate
    // @param p2 (double) end point coordinate
    // @param t (double) the value of the parameter of the curve, between 0 and 1
    // @return (double) the value of the polynomial <code>B(t)</code> at <code>t</code>
    // @visibility drawing
    //<
    bezier : function (p1, cp1, cp2, p2, t) {
        
        var u = (1 - t),
            ab = u * p1 + t * cp1,
            bc = u * cp1 + t * cp2,
            cd = u * cp2 + t * p2,
            abbc = u * ab + t * bc,
            bccd = u * bc + t * cd;
        return (u * abbc + t * bccd);
    },

    //> @classMethod drawPane.bezierExtrema()
    // Computes the minimum and maximum value of the cubic Be&#769;zier curve polynomial
    // defined in +link{bezier()},
    // for <code>0 &le; t &le; 1</code>.
    // @param p1 (double) starting point coordinate
    // @param cp1 (double) first control point coordinate
    // @param cp2 (double) second control point coordinate
    // @param p2 (double) end point coordinate
    // @return (array of double) the minimum and maximum value of the cubic Be&#769;zier curve
    // polynomial
    // @visibility drawing
    //<
    bezierExtrema : function (p1, cp1, cp2, p2) {
        var epsilon = 1e-6,
            min = Math.min(p1, p2),
            max = Math.max(p1, p2);

        
        var a = (-p1 + 3 * (cp1 - cp2) + p2),
            b = 2 * (p1 - cp1 - cp1 + cp2),
            c = (-p1 + cp1),
            discriminant = (b * b - 4 * a * c),
            aIsZero = (Math.abs(a) < epsilon);

        if (aIsZero) {
            var bIsZero = (Math.abs(b) < epsilon);
            if (!bIsZero) {
                var root = -c / b;
                root = Math.max(0, Math.min(1, root));
                var value = isc.DrawPane.bezier(p1, cp1, cp2, p2, root);
                min = Math.min(min, value);
                max = Math.max(max, value);
            }
        } else if (discriminant >= 0) {
            var sqrtDiscriminant = Math.sqrt(discriminant),
                root1 = (-b + sqrtDiscriminant) / (2 * a),
                root2 = (-b - sqrtDiscriminant) / (2 * a);

            root1 = Math.max(0, Math.min(1, root1));
            root2 = Math.max(0, Math.min(1, root2));
            var value1 = isc.DrawPane.bezier(p1, cp1, cp2, p2, root1),
                value2 = isc.DrawPane.bezier(p1, cp1, cp2, p2, root2);
            min = Math.min(min, value1, value2);
            max = Math.max(max, value1, value2);
        }

        return [min, max];
    },

    //> @classMethod drawPane.getBezierBoundingBox()
    // Calculate the bounding box of the cubic Be&#769;zier curve with endpoints
    // <code>p1</code> and <code>p2</code> and control points <code>cp1</code> and
    // <code>cp2</code>.
    // @param p1 (Point) start point of the curve
    // @param cp1 (Point) first cubic Be&#769;zier control point
    // @param cp2 (Point) second cubic Be&#769;zier control point
    // @param p2 (Point) end point of the curve
    // @return (array of double) the x1, y1, x2, y2 coordinates.  The point
    // <code>(x1, y1)</code> is the top-left point of the bounding box and the point
    // <code>(x2, y2)</code> is the bottom-right point of the bounding box.
    // @visibility drawing
    //<
    getBezierBoundingBox : function (p1, cp1, cp2, p2) {
        var xExtrema = isc.DrawPane.bezierExtrema(
                p1[0], cp1[0], cp2[0], p2[0]),
            yExtrema = isc.DrawPane.bezierExtrema(
                p1[1], cp1[1], cp2[1], p2[1]);

        return [xExtrema[0], yExtrema[0], xExtrema[1], yExtrema[1]];
    },

    //> @classMethod drawPane.scaleAndCenterBezier()
    // Computes the top-, left-, bottom-, and right-most coordinates containing the
    // Be&#769;zier curve defined by <code>startPoint</code>, <code>controlPoint1</code>,
    // <code>controlPoint2</code>, and <code>endPoint</code>, then translates and scales these
    // four points to fit the entire curve into the given width and height.
    // @param width (int) width of target space
    // @param height (int) height of target space
    // @param xc (int) center point x
    // @param yc (int) center point y
    // @param startPoint (Point) start point of the curve
    // @param endPoint (Point) end point of the curve
    // @param controlPoint1 (Point) first cubic Be&#769;zier control point
    // @param controlPoint2 (Point) second cubic Be&#769;zier control point
    // @visibility drawing
    //<
    scaleAndCenterBezier : function (
        width, height, xc, yc, startPoint, endPoint, controlPoint1, controlPoint2)
    {
        var box = isc.DrawPane.getBezierBoundingBox(
                startPoint, controlPoint1, controlPoint2, endPoint),
            maxX = box[2],
            minX = box[0],
            maxY = box[3],
            minY = box[1],
            scaleX = width / (maxX - minX),
            scaleY = height / (maxY - minY),
            x0 = (minX + maxX) / 2,
            y0 = (minY + maxY) / 2;

        startPoint[0] = isc.DrawItem._makeCoordinate(xc + scaleX * (startPoint[0] - x0));
        startPoint[1] = isc.DrawItem._makeCoordinate(yc + scaleY * (startPoint[1] - y0));
        endPoint[0] = isc.DrawItem._makeCoordinate(xc + scaleX * (endPoint[0] - x0));
        endPoint[1] = isc.DrawItem._makeCoordinate(yc + scaleY * (endPoint[1] - y0));
        controlPoint1[0] = isc.DrawItem._makeCoordinate(xc + scaleX * (controlPoint1[0] - x0));
        controlPoint1[1] = isc.DrawItem._makeCoordinate(yc + scaleY * (controlPoint1[1] - y0));
        controlPoint2[0] = isc.DrawItem._makeCoordinate(xc + scaleX * (controlPoint2[0] - x0));
        controlPoint2[1] = isc.DrawItem._makeCoordinate(yc + scaleY * (controlPoint2[1] - y0));
    },

    //> @classMethod drawPane.getRegularPolygonPoints()
    // Calls +link{getPolygonPoints()} with angles spread evenly over the full 360 degrees.
    // @param n (int) the number of vertices the polygon
    // @param width (int) width of target space
    // @param height (int) height of target space
    // @param xc (int) center point x
    // @param yc (int) center point y
    // @param startAngle (double) the angle (in radians) with respect to the center point of
    // the first vertex of the polygon
    // @return (array of Point) list of the vertices of the regular polygon
    // @visibility drawing
    //<
    getRegularPolygonPoints : function (n, width, height, xc, yc, startAngle) {
        var theta = 2 * Math.PI / n,
            angles = new Array(n);
        for (var i = 0; i < n; ++i) {
            angles[i] = startAngle + i * theta;
        }
        return isc.DrawPane.getPolygonPoints(width, height, xc, yc, angles);
    },

    //> @classMethod drawPane.getPolygonPoints()
    // Computes an array of Points for a polygon that has an equal distance from its center to
    // any of its vertices and that fits in the given width and height.
    // @param width (int) width of target space
    // @param height (int) height of target space
    // @param xc (int) center point x
    // @param yc (int) center point y
    // @param angles (array of double) the complete list of angles (in radians) with respect
    // to the center point at which the polygon must have vertices
    // @return (array of Point) list of the vertices of the polygon
    // @visibility drawing
    //<
    getPolygonPoints : function (width, height, xc, yc, angles) {
        var n = angles.length,
            points = new Array(n);
        if (n == 0) {
            return points;
        } else if (n == 1) {
            points[0] = [xc, yc];
            return points;
        }

        var maxSin = -1, minSin = 1, maxCos = -1, minCos = 1;
        for (var i = 0; i < n; ++i) {
            var angle = angles[i],
                sin = Math.sin(angle),
                cos = Math.cos(angle);

            points[i] = [cos, sin];

            maxSin = Math.max(maxSin, sin);
            minSin = Math.min(minSin, sin);
            maxCos = Math.max(maxCos, cos);
            minCos = Math.min(minCos, cos);
        }

        var t = Math.min(
                width / (maxCos - minCos),
                height / (maxSin - minSin)),
            xCenter = xc - width / 2 + (width - t * (maxCos - minCos)) / 2 - t * minCos,
            yCenter = yc - height / 2 + (height - t * (maxSin - minSin)) / 2 + t * maxSin;

        for (var i = 0; i < n; ++i) {
            var point = points[i];
            point[0] = isc.DrawItem._makeCoordinate(xCenter + t * point[0]);
            point[1] = isc.DrawItem._makeCoordinate(yCenter - t * point[1]);
        }

        return points;
    },

    _interpolateRGB : function (startColor, endColor, lambda) {
        

        // Calculate the red, green, and blue components of start and end colors in sRGB.
        var mask = 0xff,
            c0 = parseInt(startColor.substr(1), 16) >>> 0,
            red0 = ((c0 >> 16) & mask) / mask,
            green0 = ((c0 >> 8) & mask) / mask,
            blue0 = (c0 & mask) / mask,
            c1 = parseInt(endColor.substr(1), 16) >>> 0,
            red1 = ((c1 >> 16) & mask) / mask,
            green1 = ((c1 >> 8) & mask) / mask,
            blue1 = (c1 & mask) / mask;

        // Interpolate.
        var red = (1 - lambda) * red0 + lambda * red1,
            green = (1 - lambda) * green0 + lambda * green1,
            blue = (1 - lambda) * blue0 + lambda * blue1;

        // Return the hexadecimal representation.
        var r = Math.max(0, Math.min(mask, Math.floor(mask * red))) >>> 0,
            g = Math.max(0, Math.min(mask, Math.floor(mask * green))) >>> 0,
            b = Math.max(0, Math.min(mask, Math.floor(mask * blue))) >>> 0,
            interpolated = ((r << 16) | (g << 8) | b);
        return "#" + isc.NumberUtil._stringify(6, false, interpolated, 16);
    },

    _colorLightness : function (color) {
        
        var mask = 0xff,
            colorValue = parseInt(color.substr(1), 16),
            cr = ((colorValue >>> 16) & mask) / mask,
            cg = ((colorValue >>> 8) & mask) / mask,
            cb = (colorValue & mask) / mask,
            min = Math.min(cr, cg, cb),
            max = Math.max(cr, cg, cb);
        return (min + max) / 2;
    }
});


//------------------------------------------------------------------------------------------
//> @class DrawItem
//
// Base class for graphical elements drawn in a DrawPane.  All properties and methods
// documented here are available on all DrawItems unless otherwise specified.
// <P>
// Each DrawItem has its own local transform that maps its 
// +link{DrawItem,local coordinate system} to the drawing coordinate system that is shared by
// all DrawItems in the same DrawPane (explained +link{class:DrawPane,here}).  The local
// transform is a combination of rotation, scaling, and other affine transformations.  The
// DrawItem is first +link{drawItem.translate,translated}, then +link{drawItem.scale,scaled},
// then +link{drawItem.xShearFactor,sheared} in the direction of the x-axis, then
// +link{drawItem.yShearFactor,sheared} in the directiton of the y-axis, and then finally
// +link{drawItem.rotation,rotated}.
// <P>
// Note that DrawItems as such should never be created, only concrete subclasses such as
// +link{DrawGroup} and +link{DrawLine}.
// <P>
// See +link{DrawPane} for the different approaches to create DrawItems.
// @inheritsFrom BaseWidget
// @treeLocation Client Reference/Drawing
// @visibility drawing
//<


//------------------------------------------------------------------------------------------

// DrawItem implements the line (aka stroke) and fill attributes that are shared
// by all drawing primitives.



isc.defineClass("DrawItem", "BaseWidget");

isc.DrawItem.addClassProperties({
    _nextZIndex: 200000,
    _nextDrawToBackZIndex: 100000,
    _SMALL_Z_INDEX: 99950,
    _BIG_Z_INDEX: 800000,

    // Compare by zIndex if different, otherwise by _addOrder.
    
    _zIndexCompareFunc : function (drawItemA, drawItemB) {
        
        if (drawItemA.zIndex == drawItemB.zIndex) {
            if (drawItemB._addOrder == null) {
                return 0;
            }
            return drawItemA._addOrder - drawItemB._addOrder;
        }
        return drawItemA.zIndex - drawItemB.zIndex;
    },

    
    _kappa: 4 * (Math.SQRT2 - 1) / 3,

    //> @classMethod drawItem.computeAngle()
    // Computes the angle in degrees from the positive X axis to the difference vector
    // <nobr><b>v</b><sub>2</sub> - <b>v</b><sub>1</sub></nobr> between the two given vectors.
    // @param px1 (double) X coordinate of <b>v</b><sub>1</sub>
    // @param py1 (double) Y coordinate of <b>v</b><sub>1</sub>
    // @param px2 (double) X coordinate of <b>v</b><sub>2</sub>
    // @param py2 (double) Y coordinate of <b>v</b><sub>2</sub>
    // @return (double) the angle in degrees, in the range [0, 360).
    // @visibility drawing
    //<
    _radPerDeg: isc.Math._radPerDeg,
    computeAngle : function (px1, py1, px2, py2) {
        // http://mathworld.wolfram.com/VectorDifference.html
        // http://www.euclideanspace.com/maths/algebra/vectors/angleBetween/issues/index.htm
        var a = Math.atan2(py2 - py1, px2 - px1);
        if (a < 0) a += 2 * Math.PI;
        return a / this._radPerDeg;
    },

    
    _makeCoordinate : function (x) {
        return Math.round(x);
    },
    

    _isSimpleGradient : function (def) {
        return (def.direction != null);
    },
    _isLinearGradient : function (def) {
        return (def.x1 != null);
    },
    _isRadialGradient : function (def) {
        return (!isc.DrawItem._isSimpleGradient(def) && !isc.DrawItem._isLinearGradient(def));
    },
    _getGradientConstructor : function (def) {
        return (isc.DrawItem._isSimpleGradient(def) ? "SimpleGradient" :
            (isc.DrawItem._isLinearGradient(def) ? "LinearGradient" :
            "RadialGradient"));
    },

    // @param element (SVGElement) the reference SVG element
    // @param where (String) one of "beforebegin", "afterbegin", "beforeend", or "afterend"
    // @param svgString (SVGString) SVG source to insert
    // @param singleElement (boolean) true if the svgString is markup for a single element;
    // false otherwise.
    // @param conversionContext (SVGStringConversionContext)
    // @return (SVGElement) if singleElement is true, the SVGElement that was inserted; null
    // otherwise.
    _insertAdjacentSVG : function (element, where, svgString, singleElement, conversionContext) {
        where = where.toLowerCase(); // case-insensitive match

        if (isc.Browser.useInsertAdjacentHTMLForSVG) {
            try {
                element.insertAdjacentHTML(where, svgString);

                if (singleElement) {
                    switch (where) {
                        case "beforebegin":
                            return element.previousSibling;
                        case "afterbegin":
                            return element.firstChild;
                        case "beforeend":
                            return element.lastChild;
                        case "afterend":
                            return element.nextSibling;
                    }
                }
                return;

            } catch (e) {
                // In case of exception, fall through to the alternative implementation.
            }
        }

        var newElement,
            doc = element.ownerDocument;
        if (isc.Browser._supportsSVGCreateContextualFragment) {
            var range = doc.createRange();
            range.setStartBefore(element);
            newElement = range.createContextualFragment(svgString);
            if (singleElement) newElement = newElement.firstChild;
        } else {
            svgString = (
                "<svg xmlns='" + isc._$svgNS + "' xmlns:" + conversionContext.xlinkPrefix + "='" + isc._$xlinkNS + "'>" +
                svgString +
                "</svg>"
                );
            
            var domParser = new DOMParser,
                svgSVGElem;
            try {
                var parsedDoc = domParser.parseFromString(svgString, "image/svg+xml");
                svgSVGElem = parsedDoc.documentElement;
            } catch (e) {
                var newDocument = doc.implementation.createHTMLDocument("Untitled");
                newDocument.body.innerHTML = svgString;
                svgSVGElem = newDocument.body.firstChild;
            }
            if (singleElement || svgSVGElem.firstChild == svgSVGElem.lastChild) newElement = svgSVGElem.firstChild;
            else {
                newElement = doc.createDocumentFragment();
                var child;
                while ((child = svgSVGElem.firstChild) != null) {
                    newElement.appendChild(child);
                }
            }
        }

        switch (where){
        case "beforebegin":
            element.parentNode.insertBefore(newElement, element);
            break;
        case "afterbegin":
            element.insertBefore(newElement, element.firstChild);
            break;
        case "beforeend":
            element.appendChild(newElement);
            break;
        case "afterend":
            element.parentNode.insertBefore(newElement, element.nextSibling);
            break;
        }
        if (singleElement) return newElement;
    },

    
    _$namedColors: {
        "aliceblue": [0.9411764705882353, 0.9725490196078431, 1, 1],
        "antiquewhite": [0.9803921568627451, 0.9215686274509803, 0.8431372549019608, 1],
        "aqua": [0, 1, 1, 1],
        "aquamarine": [0.4980392156862745, 1, 0.8313725490196079, 1],
        "azure": [0.9411764705882353, 1, 1, 1],
        "beige": [0.9607843137254902, 0.9607843137254902, 0.8627450980392157, 1],
        "bisque": [1, 0.8941176470588236, 0.7686274509803922, 1],
        "black": [0, 0, 0, 1],
        "blanchedalmond": [1, 0.9215686274509803, 0.803921568627451, 1],
        "blue": [0, 0, 1, 1],
        "blueviolet": [0.5411764705882353, 0.16862745098039217, 0.8862745098039215, 1],
        "brown": [0.6470588235294118, 0.16470588235294117, 0.16470588235294117, 1],
        "burlywood": [0.8705882352941177, 0.7215686274509804, 0.5294117647058824, 1],
        "cadetblue": [0.37254901960784315, 0.6196078431372549, 0.6274509803921569, 1],
        "chartreuse": [0.4980392156862745, 1, 0, 1],
        "chocolate": [0.8235294117647058, 0.4117647058823529, 0.11764705882352941, 1],
        "coral": [1, 0.4980392156862745, 0.3137254901960784, 1],
        "cornflowerblue": [0.39215686274509803, 0.5843137254901961, 0.9294117647058824, 1],
        "cornsilk": [1, 0.9725490196078431, 0.8627450980392157, 1],
        "crimson": [0.8627450980392157, 0.0784313725490196, 0.23529411764705882, 1],
        "cyan": [0, 1, 1, 1],
        "darkblue": [0, 0, 0.5450980392156862, 1],
        "darkcyan": [0, 0.5450980392156862, 0.5450980392156862, 1],
        "darkgoldenrod": [0.7215686274509804, 0.5254901960784314, 0.043137254901960784, 1],
        "darkgray": [0.6627450980392157, 0.6627450980392157, 0.6627450980392157, 1],
        "darkgreen": [0, 0.39215686274509803, 0, 1],
        "darkgrey": [0.6627450980392157, 0.6627450980392157, 0.6627450980392157, 1],
        "darkkhaki": [0.7411764705882353, 0.7176470588235294, 0.4196078431372549, 1],
        "darkmagenta": [0.5450980392156862, 0, 0.5450980392156862, 1],
        "darkolivegreen": [0.3333333333333333, 0.4196078431372549, 0.1843137254901961, 1],
        "darkorange": [1, 0.5490196078431373, 0, 1],
        "darkorchid": [0.6, 0.19607843137254902, 0.8, 1],
        "darkred": [0.5450980392156862, 0, 0, 1],
        "darksalmon": [0.9137254901960784, 0.5882352941176471, 0.47843137254901963, 1],
        "darkseagreen": [0.5607843137254902, 0.7372549019607844, 0.5607843137254902, 1],
        "darkslateblue": [0.2823529411764706, 0.23921568627450981, 0.5450980392156862, 1],
        "darkslategray": [0.1843137254901961, 0.30980392156862746, 0.30980392156862746, 1],
        "darkslategrey": [0.1843137254901961, 0.30980392156862746, 0.30980392156862746, 1],
        "darkturquoise": [0, 0.807843137254902, 0.8196078431372549, 1],
        "darkviolet": [0.5803921568627451, 0, 0.8274509803921568, 1],
        "deeppink": [1, 0.0784313725490196, 0.5764705882352941, 1],
        "deepskyblue": [0, 0.7490196078431373, 1, 1],
        "dimgray": [0.4117647058823529, 0.4117647058823529, 0.4117647058823529, 1],
        "dimgrey": [0.4117647058823529, 0.4117647058823529, 0.4117647058823529, 1],
        "dodgerblue": [0.11764705882352941, 0.5647058823529412, 1, 1],
        "firebrick": [0.6980392156862745, 0.13333333333333333, 0.13333333333333333, 1],
        "floralwhite": [1, 0.9803921568627451, 0.9411764705882353, 1],
        "forestgreen": [0.13333333333333333, 0.5450980392156862, 0.13333333333333333, 1],
        "fuchsia": [1, 0, 1, 1],
        "gainsboro": [0.8627450980392157, 0.8627450980392157, 0.8627450980392157, 1],
        "ghostwhite": [0.9725490196078431, 0.9725490196078431, 1, 1],
        "gold": [1, 0.8431372549019608, 0, 1],
        "goldenrod": [0.8549019607843137, 0.6470588235294118, 0.12549019607843137, 1],
        "gray": [0.5019607843137255, 0.5019607843137255, 0.5019607843137255, 1],
        "green": [0, 0.5019607843137255, 0, 1],
        "greenyellow": [0.6784313725490196, 1, 0.1843137254901961, 1],
        "grey": [0.5019607843137255, 0.5019607843137255, 0.5019607843137255, 1],
        "honeydew": [0.9411764705882353, 1, 0.9411764705882353, 1],
        "hotpink": [1, 0.4117647058823529, 0.7058823529411765, 1],
        "indianred": [0.803921568627451, 0.3607843137254902, 0.3607843137254902, 1],
        "indigo": [0.29411764705882354, 0, 0.5098039215686274, 1],
        "ivory": [1, 1, 0.9411764705882353, 1],
        "khaki": [0.9411764705882353, 0.9019607843137255, 0.5490196078431373, 1],
        "lavender": [0.9019607843137255, 0.9019607843137255, 0.9803921568627451, 1],
        "lavenderblush": [1, 0.9411764705882353, 0.9607843137254902, 1],
        "lawngreen": [0.48627450980392156, 0.9882352941176471, 0, 1],
        "lemonchiffon": [1, 0.9803921568627451, 0.803921568627451, 1],
        "lightblue": [0.6784313725490196, 0.8470588235294118, 0.9019607843137255, 1],
        "lightcoral": [0.9411764705882353, 0.5019607843137255, 0.5019607843137255, 1],
        "lightcyan": [0.8784313725490196, 1, 1, 1],
        "lightgoldenrodyellow": [0.9803921568627451, 0.9803921568627451, 0.8235294117647058, 1],
        "lightgray": [0.8274509803921568, 0.8274509803921568, 0.8274509803921568, 1],
        "lightgreen": [0.5647058823529412, 0.9333333333333333, 0.5647058823529412, 1],
        "lightgrey": [0.8274509803921568, 0.8274509803921568, 0.8274509803921568, 1],
        "lightpink": [1, 0.7137254901960784, 0.7568627450980392, 1],
        "lightsalmon": [1, 0.6274509803921569, 0.47843137254901963, 1],
        "lightseagreen": [0.12549019607843137, 0.6980392156862745, 0.6666666666666666, 1],
        "lightskyblue": [0.5294117647058824, 0.807843137254902, 0.9803921568627451, 1],
        "lightslategray": [0.4666666666666667, 0.5333333333333333, 0.6, 1],
        "lightslategrey": [0.4666666666666667, 0.5333333333333333, 0.6, 1],
        "lightsteelblue": [0.6901960784313725, 0.7686274509803922, 0.8705882352941177, 1],
        "lightyellow": [1, 1, 0.8784313725490196, 1],
        "lime": [0, 1, 0, 1],
        "limegreen": [0.19607843137254902, 0.803921568627451, 0.19607843137254902, 1],
        "linen": [0.9803921568627451, 0.9411764705882353, 0.9019607843137255, 1],
        "magenta": [1, 0, 1, 1],
        "maroon": [0.5019607843137255, 0, 0, 1],
        "mediumaquamarine": [0.4, 0.803921568627451, 0.6666666666666666, 1],
        "mediumblue": [0, 0, 0.803921568627451, 1],
        "mediumorchid": [0.7294117647058823, 0.3333333333333333, 0.8274509803921568, 1],
        "mediumpurple": [0.5764705882352941, 0.4392156862745098, 0.8588235294117647, 1],
        "mediumseagreen": [0.23529411764705882, 0.7019607843137254, 0.44313725490196076, 1],
        "mediumslateblue": [0.4823529411764706, 0.40784313725490196, 0.9333333333333333, 1],
        "mediumspringgreen": [0, 0.9803921568627451, 0.6039215686274509, 1],
        "mediumturquoise": [0.2823529411764706, 0.8196078431372549, 0.8, 1],
        "mediumvioletred": [0.7803921568627451, 0.08235294117647059, 0.5215686274509804, 1],
        "midnightblue": [0.09803921568627451, 0.09803921568627451, 0.4392156862745098, 1],
        "mintcream": [0.9607843137254902, 1, 0.9803921568627451, 1],
        "mistyrose": [1, 0.8941176470588236, 0.8823529411764706, 1],
        "moccasin": [1, 0.8941176470588236, 0.7098039215686275, 1],
        "navajowhite": [1, 0.8705882352941177, 0.6784313725490196, 1],
        "navy": [0, 0, 0.5019607843137255, 1],
        "oldlace": [0.9921568627450981, 0.9607843137254902, 0.9019607843137255, 1],
        "olive": [0.5019607843137255, 0.5019607843137255, 0, 1],
        "olivedrab": [0.4196078431372549, 0.5568627450980392, 0.13725490196078433, 1],
        "orange": [1, 0.6470588235294118, 0, 1],
        "orangered": [1, 0.27058823529411763, 0, 1],
        "orchid": [0.8549019607843137, 0.4392156862745098, 0.8392156862745098, 1],
        "palegoldenrod": [0.9333333333333333, 0.9098039215686274, 0.6666666666666666, 1],
        "palegreen": [0.596078431372549, 0.984313725490196, 0.596078431372549, 1],
        "paleturquoise": [0.6862745098039216, 0.9333333333333333, 0.9333333333333333, 1],
        "palevioletred": [0.8588235294117647, 0.4392156862745098, 0.5764705882352941, 1],
        "papayawhip": [1, 0.9372549019607843, 0.8352941176470589, 1],
        "peachpuff": [1, 0.8549019607843137, 0.7254901960784313, 1],
        "peru": [0.803921568627451, 0.5215686274509804, 0.24705882352941178, 1],
        "pink": [1, 0.7529411764705882, 0.796078431372549, 1],
        "plum": [0.8666666666666667, 0.6274509803921569, 0.8666666666666667, 1],
        "powderblue": [0.6901960784313725, 0.8784313725490196, 0.9019607843137255, 1],
        "purple": [0.5019607843137255, 0, 0.5019607843137255, 1],
        "rebeccapurple": [0.4, 0.2, 0.6, 1],
        "red": [1, 0, 0, 1],
        "rosybrown": [0.7372549019607844, 0.5607843137254902, 0.5607843137254902, 1],
        "royalblue": [0.2549019607843137, 0.4117647058823529, 0.8823529411764706, 1],
        "saddlebrown": [0.5450980392156862, 0.27058823529411763, 0.07450980392156863, 1],
        "salmon": [0.9803921568627451, 0.5019607843137255, 0.4470588235294118, 1],
        "sandybrown": [0.9568627450980393, 0.6431372549019608, 0.3764705882352941, 1],
        "seagreen": [0.1803921568627451, 0.5450980392156862, 0.3411764705882353, 1],
        "seashell": [1, 0.9607843137254902, 0.9333333333333333, 1],
        "sienna": [0.6274509803921569, 0.3215686274509804, 0.17647058823529413, 1],
        "silver": [0.7529411764705882, 0.7529411764705882, 0.7529411764705882, 1],
        "skyblue": [0.5294117647058824, 0.807843137254902, 0.9215686274509803, 1],
        "slateblue": [0.41568627450980394, 0.35294117647058826, 0.803921568627451, 1],
        "slategray": [0.4392156862745098, 0.5019607843137255, 0.5647058823529412, 1],
        "slategrey": [0.4392156862745098, 0.5019607843137255, 0.5647058823529412, 1],
        "snow": [1, 0.9803921568627451, 0.9803921568627451, 1],
        "springgreen": [0, 1, 0.4980392156862745, 1],
        "steelblue": [0.27450980392156865, 0.5098039215686274, 0.7058823529411765, 1],
        "tan": [0.8235294117647058, 0.7058823529411765, 0.5490196078431373, 1],
        "teal": [0, 0.5019607843137255, 0.5019607843137255, 1],
        "thistle": [0.8470588235294118, 0.7490196078431373, 0.8470588235294118, 1],
        "tomato": [1, 0.38823529411764707, 0.2784313725490196, 1],
        "turquoise": [0.25098039215686274, 0.8784313725490196, 0.8156862745098039, 1],
        "violet": [0.9333333333333333, 0.5098039215686274, 0.9333333333333333, 1],
        "wheat": [0.9607843137254902, 0.8705882352941177, 0.7019607843137254, 1],
        "white": [1, 1, 1, 1],
        "whitesmoke": [0.9607843137254902, 0.9607843137254902, 0.9607843137254902, 1],
        "yellow": [1, 1, 0, 1],
        "yellowgreen": [0.6039215686274509, 0.803921568627451, 0.19607843137254902, 1]
    },
    _lookUpRGBA : function (cssColorString) {
        if (!cssColorString) cssColorString = "black";
        else cssColorString = String(cssColorString).trim().toLowerCase();

        var namedColors = isc.DrawItem._$namedColors;
        if (namedColors.hasOwnProperty(cssColorString)) return namedColors[cssColorString];

        var result;

        result = cssColorString.match(/#([0-9a-f])([0-9a-f])([0-9a-f])$/);
        if (result) {
            var rr = parseInt(result[1] + result[1], 16),
                gg = parseInt(result[2] + result[2], 16),
                bb = parseInt(result[3] + result[3], 16);
            return [rr / 255, gg / 255, bb / 255, 1];
        }

        result = cssColorString.match(/#([0-9a-f][0-9a-f])?([0-9a-f][0-9a-f])([0-9a-f][0-9a-f])([0-9a-f][0-9a-f])$/);
        if (result) {
            var aa = result[1] ? parseInt(result[1], 16) : 255,
                rr = parseInt(result[2], 16),
                gg = parseInt(result[3], 16),
                bb = parseInt(result[4], 16);
            return [rr / 255, gg / 255, bb / 255, 1];
        }

        result = cssColorString.match(/rgb\(([^,]+),([^,]+),([^,]+)\)$/);
        if (result) {
            var r = parseFloat(result[1]),
                g = parseFloat(result[2]),
                b = parseFloat(result[3]);

            if (isNaN(r)) r = 0;
            else if (result[1].endsWith("%")) r /= 100;
            else r /= 255;

            if (isNaN(g)) g = 0;
            else if (result[2].endsWith("%")) g /= 100;
            else g /= 255;

            if (isNaN(b)) b = 0;
            else if (result[3].endsWith("%")) b /= 100;
            else b /= 255;

            return [r, g, b, 1];
        }

        result = cssColorString.match(/rgba\(([^,]+),([^,]+),([^,]+),([^,]+)\)$/);
        if (result) {
            var r = parseFloat(result[1]),
                g = parseFloat(result[2]),
                b = parseFloat(result[3]),
                a = parseFloat(result[4]);

            if (isNaN(r)) r = 0;
            else if (result[1].endsWith("%")) r /= 100;
            else r /= 255;

            if (isNaN(g)) g = 0;
            else if (result[2].endsWith("%")) g /= 100;
            else g /= 255;

            if (isNaN(b)) b = 0;
            else if (result[3].endsWith("%")) b /= 100;
            else b /= 255;

            if (isNaN(a)) a = 0;
            else if (result[4].endsWith("%")) a /= 100;

            return [r, g, b, a];
        }

        return [0, 0, 0, 0];
    },

    
    _fitBestRectOutput: { success: false, left: 0, top: 0, width: 0, height: 0 },
    _fitBestRect : function (transform, cx, cy, left, top, width, height, halfLineWidthAndHitTolerance) {
        
        var output = isc.DrawItem._fitBestRectOutput;
        output.success = false;

        
        var epsilon = 1e-9;
        if (width != 0 && height != 0 &&
            Math.abs(transform.getDeterminant()) > epsilon)
        {
            var keepAsInts = (
                    left == Math.floor(left) &&
                    top == Math.floor(top) &&
                    width == Math.floor(width) &&
                    height == Math.floor(height)),
                sx = 0, sy = 0, dx = 0, dy = 0;

            
            var decomp = isc.AffineTransform._decomposeTransform(transform, cx, cy),
                h00 = decomp.h00, h01 = decomp.h01, h02 = decomp.h02,
                h10 = decomp.h10, h11 = decomp.h11, h12 = decomp.h12,
                detH = (h00 * h11 - h01 * h10);

            sx = h00;
            sy = h11;
            dx = sx * (h11 * h02 - h01 * h12) / detH;
            dy = sy * (h00 * h12 - h10 * h02) / detH;

            
            if (keepAsInts) {
                dx = Math.round(dx);
                dy = Math.round(dy);
            }

            if (sx != 0 && sy != 0) {
                var t = isc.AffineTransform.create(sx, 0, dx, 0, sy, dy);
                // Apply `t` to (left, top), (left + width, top + height)
                var v = t.transform(left, top),
                    w = t.transform(left + width, top + height),
                    newLeft = 0, newTop = 0, newWidth = 0, newHeight = 0;
                if (keepAsInts) {
                    var newRight = Math.round(w[0]),
                        newBottom = Math.round(w[1]);
                    newLeft = Math.round(v[0]);
                    newTop = Math.round(v[1]);
                    newWidth = newRight - newLeft;
                    newHeight = newBottom - newTop;
                } else {
                    newLeft = v[0];
                    newTop = v[1];
                    newWidth = w[0] - newLeft;
                    newHeight = w[1] - newTop;
                }

                // Avoid collapsing the shape to a point or line.
                if (newWidth != 0 && newHeight != 0) {

                    var radius = (halfLineWidthAndHitTolerance || 0);
                    if (radius != 0) {
                        
                        // If `sx == 1` then the formulas would not change `sx` or `dx`.
                        if (sx != 1) {
                            var sxNumer = (newWidth + 2 * radius),
                                sxDenom = (newWidth + 2 * sx * radius);
                            dx = (radius * (sx - 1) * (newWidth + 2 * newLeft) + sxNumer * dx) / sxDenom;
                            sx *= sxNumer / sxDenom;
                        }
                        if (sy != 1) {
                            var syNumer = (newHeight + 2 * radius),
                                syDenom = (newHeight + 2 * sy * radius);
                            dy = (radius * (sy - 1) * (newHeight + 2 * newTop) + syNumer * dy) / syDenom;
                            sy *= syNumer / syDenom;
                        }
                        t.m00 = sx;
                        t.m02 = dx;
                        t.m11 = sy;
                        t.m12 = dy;
                    }

                    // Save the remaining transform as the new local transform.
                    transform.rightMultiply(t.getInverse());

                    output.success = true;
                    output.left = newLeft;
                    output.top = newTop;
                    output.width = newWidth;
                    output.height = newHeight;
                }
            }
        }
        return output;
    }

    
});

isc.DrawItem.addProperties({
    _radPerDeg: isc.Math._radPerDeg,

    //> @attr drawItem.zIndex (Integer : null : IR)
    // Relative stacking order of this draw item with respect to other items in the same +link{DrawPane}
    // or +link{DrawGroup}.
    // <p>
    // null means that the zIndex has not been resolved. Upon adding this draw item to a
    // <code>DrawPane</code> or <code>DrawGroup</code>, this item's zIndex will be resolved to
    // the next higher auto-assigned zIndex. Note that this may still be less than another item's
    // zIndex if +link{DrawItem.bringToFront(),bringToFront()} was called on that item.
    // <p>
    // If two items within the same <code>DrawPane</code> or <code>DrawGroup</code> have the
    // same zIndex, then they are stacked in the order in which they were added to the
    // <code>DrawPane</code> or <code>DrawGroup</code>.
    // <p>
    // When the <code>DrawPane</code>'s +link{DrawPane.drawingType,drawingType} is
    // <smartclient>"bitmap",</smartclient>
    // <smartgwt>{@link com.smartgwt.client.types.DrawingType#BITMAP},</smartgwt>
    // zIndex, +link{DrawItem.bringToFront()}, and +link{DrawItem.sendToBack()} are not supported
    // for +link{DrawLabel}s on iOS due to platform limitations.
    // @group zIndex
    // @visibility drawing
    //<
    //zIndex: null,

    //> @attr drawItem.cursor (Cursor : null : IRWA)
    // If set, specifies the cursor to display when the mouse pointer is over this DrawItem.
    // @visibility drawing
    //<
    //cursor: null,

    //> @attr drawItem.dragRepositionCursor (Cursor : "move" : IRWA)
    //<
    dragRepositionCursor: "move",

    //> @attr drawItem.canHover (boolean : null : IRW)
    // Will this DrawItem fire hover events when the user hovers over it?
    // @group hovers
    // @visibility drawing
    // @see showHover
    //<
    //canHover: null,

    //> @attr drawItem.showHover (boolean : true : IRW)
    // If +link{canHover,canHover} is true, should we show the global hover canvas by default
    // when the user hovers over this DrawItem?
    // @group hovers
    // @visibility drawing
    // @see getHoverHTML()
    //<
    showHover: true,

    //> @attr drawItem.prompt (HTMLString : null : IRW)
    // Default +link{getHoverHTML(),hover HTML} that is displayed in the global hover canvas if
    // the user hovers over this DrawItem and +link{showHover,showHover} is true.
    // @group hovers
    // @visibility drawing
    //<
    //prompt: null,

    // Pane / Group membership
    // ---------------------------------------------------------------------------------------

    //> @attr drawItem.drawPane   (DrawPane : null : IRW)
    // +link{DrawPane} this drawItem should draw in.
    // <P>
    // If this item has a +link{drawGroup}, the drawGroup's drawPane is automatically used.
    // @visibility drawing
    //<

    //> @attr drawItem.drawGroup  (DrawGroup : null : IR)
    // +link{DrawGroup} this drawItem is a member of.
    // @visibility drawing
    //<

    _getDrawPane : function () {
        var drawPane = null;
        for (var obj = this; ; ) {
            if (obj.drawPane != null) {
                drawPane = obj.drawPane;
                break;
            } else if (obj.drawGroup != null) {
                obj = obj.drawGroup;
            } else {
                break;
            }
        }
        
        return drawPane;
    },

    // Line Styling
    // ---------------------------------------------------------------------------------------

    //> @attr drawItem.lineWidth  (int : 3 : IRW)
    // Pixel width for lines.
    // @group line
    // @visibility drawing
    //<
    // XXX setLineWidth(0) will not cause a VML line to disappear, but will cause an SVG line
    // to disappear
    lineWidth: 3,

    //> @attr drawItem.lineColor (CSSColor : "#808080" : IRW)
    // Line color
    // @group line
    // @visibility drawing
    //<
    lineColor: "#808080",

    //> @attr drawItem.lineOpacity (float : 1.0 : IRW)
    // Opacity for lines, as a number between 0 (transparent) and 1 (opaque).
    // @group line
    // @visibility drawing
    //<
    lineOpacity: 1.0,

    //> @attr drawItem.linePattern (LinePattern : "solid" : IRW)
    // Pattern for lines, eg "solid" or "dash"
    // @group line
    // @visibility drawing
    //<
    linePattern: "solid",

    //> @type LinePattern
    // Supported styles of drawing lines.
    // @value "solid"     Solid line
    // @value "dot"       Dotted line
    // @value "dash"      Dashed line
    // @value "shortdot"  Dotted line, with more tightly spaced dots
    // @value "shortdash" Dashed line, with shorter, more tightly spaced dashes
    // @value "longdash"  Dashed line, with longer, more widely spaced dashes
    // @group line
    // @visibility drawing
    //<
    // see setLinePattern() for notes on expanding this set

    //> @attr drawItem.lineCap     (LineCap : "round" : IRW)
    // Style of drawing the endpoints of a line.
    // <P>
    // Note that for dashed and dotted lines, the lineCap style affects each dash or dot.
    //
    // @group line
    // @visibility drawing
    //<
    lineCap: "round",

    //> @type LineCap
    // Supported styles of drawing the endpoints of a line
    //
    // @value "round"    Semicircular rounding
    // @value "square"   Squared-off endpoint
    // @value "butt"     Square endpoint, stops exactly at the line's end coordinates instead
    //                   of extending 1/2 lineWidth further as "round" and "square" do
    // 
    // @group line
    // @visibility drawing
    //<

    // Fill Styling
    // ---------------------------------------------------------------------------------------

    //> @attr drawItem.fillColor     (CSSColor : null : IRW)
    // Fill color to use for shapes.  The default of 'null' is transparent.
    // @group fill
    // @visibility drawing
    //<
    //fillColor: null, // transparent

    //> @attr drawItem.fillGradient     (Gradient | string: null : IRW)
    // Fill gradient to use for shapes.  If a string it uses the gradient identifier parameter provided in 
    // +link{drawPane.addGradient}. Otherwise it expects one of +link{SimpleGradient,SimpleGradient},
    // +link{LinearGradient,LinearGradient} or +link{RadialGradient,RadialGradient}. 
    // @see Gradient
    // @group fill
    // @visibility drawing
    //<
    fillGradient: null,

    //> @attr drawItem.fillOpacity   (float : 1.0 : IRW)
    // Opacity of the fillColor, as a number between 0 (transparent) and 1 (opaque).
    // @group fill
    // @visibility drawing
    //<
    fillOpacity: 1.0,

    //> @attr drawItem.shadow  (Shadow: null : IRW)
    // Shadow used for all DrawItem subtypes. 
    // @example drawingShadows
    // @visibility drawing
    //<
    shadow: null,

    //> @attr drawItem.rotation (float : 0.0 : IR)
    // Rotation in degrees about the +link{getCenter(),center point}.
    // The positive direction is clockwise.
    // @visibility drawing
    //<
    rotation: 0,

    //> @attr drawItem.xShearFactor (float : 0.0 : IRA)
    // The slope of an x-shearing transformation applied to this DrawItem.  Each point in the
    // shape is moved along the x-axis a distance that is proportional to the initial
    // y-coordinate of the point.
    // @visibility drawing
    //<
    xShearFactor: 0,

    //> @attr drawItem.yShearFactor (float : 0.0 : IRA)
    // The slope of a y-shearing transformation applied to this DrawItem.  Each point in the
    // shape is moved along the y-axis a distance that is proportional to the initial
    // x-coordinate of the point.
    // @visibility drawing
    //<
    yShearFactor: 0,

    //> @attr drawItem.scale (Array[] of float : null : IRA)
    // Array holds 2 values representing scaling along x and y dimensions.
    // @visibility drawing
    //<
    scale: null,

    //> @attr drawItem.translate (Array[] of float : null : IRA)
    // Array holds two values representing translation along the x and y dimensions.
    // @visibility drawing
    //<
    translate: null,

    
    exemptFromGlobalTransform: false,

    //> @attr drawItem.svgFilter (String : null : IRA)
    // ID of an SVG <code>&lt;filter&gt;</code> definition to use.
    //<
    svgFilter: null,

    // Arrowheads
    // ---------------------------------------------------------------------------------------

    //> @attr drawItem.startArrow    (ArrowStyle : null : IRW)
    // Style of arrow head to draw at the beginning of the line or path.
    // @example LinesAndArrowheads
    // @visibility drawing
    //<

    //> @attr drawItem.endArrow (ArrowStyle : null : IRW)
    // Style of arrow head to draw at the end of the line or path.
    // @example LinesAndArrowheads
    // @visibility drawing
    //<

    //> @type ArrowStyle
    // Supported styles for arrowheads.
    // @value "block"     Filled triangle.
    // @value "open"      Two lines are drawn on each side of the path.
    // @visibility drawing
    //<

    

    // Control Knobs
    // -----------------------------------------------------------------------------------------

    //> @attr drawItem.knobs (Array of KnobType : null : IR)
    // Array of control knobs to display for this item. Each +link{knobType} specified in this array
    // will turn on UI element(s) allowing the user to manipulate this drawItem.  To update the
    // set of knobs at runtime use +link{drawItem.showKnobs()} and +link{drawItem.hideKnobs()}.
    // <p>
    // <b>NOTE:</b> Unless otherwise documented, DrawItem types only support
    // <smartclient>"resize" and "move"</smartclient>
    // <smartgwt>{@link com.smartgwt.client.types.KnobType#RESIZE} and {@link com.smartgwt.client.types.KnobType#MOVE}</smartgwt>
    // knobs.
    //
    // @example editDrawPane
    // @example drawKnobs
    // @visibility drawing
    //<

    //> @type KnobType
    // Entries for the +link{drawItem.knobs} array. Each specified knobType will enable some UI 
    // allowing the user to manipulate the DrawItem directly.
    // <p>
    // <b>NOTE:</b> Not all knob types are supported by each DrawItem type. Refer to the DrawItem
    // type's +link{DrawItem.knobs,knobs} attribute documentation for a list of the supported knob types.
    //
    // @value "resize"
    //  Display up to 8 control knobs at the corners specified by +link{DrawItem.resizeKnobPoints},
    //  allowing the user to drag-resize the item.
    //  See also +link{drawItem.cornerResizeKnob} and +link{drawItem.sideResizeKnob}.
    //
    // @value "move"
    //  Display a control knob for moving the item around. See also +link{drawItem.moveKnobPoint}
    //  and +link{drawItem.moveKnobOffset}
    //
    // @value "startPoint"
    //  Control knob  to manipulate +link{drawLine.startPoint}.
    //
    // @value "endPoint"
    //  Control knob to manipulate +link{drawLine.endPoint}.
    //
    // @value "controlPoint1"
    //  Display a draggable control knob along with a DrawLine indicating the angle between controlPoint1
    //  and the startPoint. Dragging the knob will adjust controlPoint1.
    //
    // @value "controlPoint2" 
    //  Display a draggable control knob along with a DrawLine indicating the angle between controlPoint2
    //  and the endPoint. Dragging the knob will adjust controlPoint2.
    // @visibility drawing
    //<

    // Title
    // ---------------------------------------------------------------------------------------

    //> @attr drawItem.title (String : null : IRWA)
    // A string to show at the +link{getCenter(),center point} of this <code>DrawItem</code>.
    // <p>
    // When set to a non-null value (including an empty string), the +link{DrawItem.titleLabel,titleLabel}
    // +link{DrawLabel} AutoChild will be created automatically and positioned at the center of
    // this <code>DrawItem</code>.
    // @see attr:titleRotationMode
    // @visibility drawing
    //<
    //title: null,

    //> @attr drawItem.titleLabel (DrawLabel AutoChild : null : RA)
    // When a non-null +link{DrawItem.title,title} is set, this AutoChild is created automatically
    // and positioned at the +link{getCenter(),center} of this <code>DrawItem</code>. The
    // <code>titleLabel</code> moves with this <code>DrawItem</code> and, depending on
    // +link{titleRotationMode,titleRotationMode}, can rotate with this <code>DrawItem</code> as
    // well.
    // <p>
    // The following +link{group:autoChildUsage,passthrough} applies:<br>
    // +link{DrawItem.title,title} for +link{DrawLabel.contents}.
    // <p>
    // Related to the <code>titleLabel</code> is the +link{DrawItem.titleLabelBackground,titleLabelBackground}
    // which allows a border to be placed around the <code>titleLabel</code> and/or a background
    // added. By default, shapes that are commonly filled such as +link{DrawTriangle}s, with
    // the exception of +link{DrawSector}s, do not show the <code>titleLabelBackground</code>
    // (see +link{DrawItem.showTitleLabelBackground,showTitleLabelBackground}).
    // @see DrawItem.showTitleLabelBackground
    // @visibility drawing
    //<
    titleLabelDefaults: {
        _constructor: "DrawLabel",
        _isTitleLabel: true,
        cursor: isc.Canvas.DEFAULT,
        alignment: "center",
        // Make the default lineColor a tiny bit darker than the default DrawItem lineColor
        // (#808080) because the default "normal" weight of the font makes the text color look
        // lighter, so this compensates for the lighter appearance.
        lineColor: "#707070",
        fontSize: 11,
        fontFamily: "Arial",
        fontWeight: "normal",

        setContents : function (contents, fromUpdateTitleLabelAndBackground) {
            var returnVal = this.getClass()._instancePrototype.setContents.apply(this, arguments);
            if (!fromUpdateTitleLabelAndBackground) {
                this.creator._updateTitleLabelAndBackground();
            }
            return returnVal;
        }
    },

    //> @type TitleRotationMode
    // The different ways in which the +link{DrawItem.titleLabel,titleLabel} of a +link{DrawItem}
    // can be rotated with the item.
    // <p>
    // <strong>NOTE:</strong> The effect of the
    // <smartclient>"withItemAlwaysUp" and "withLineAlwaysUp"</smartclient>
    // <smartgwt>{@link com.smartgwt.client.types.TitleRotationMode#WITH_ITEM_ALWAYS_UP} and
    // {@link com.smartgwt.client.types.TitleRotationMode#WITH_LINE_ALWAYS_UP}</smartgwt>
    // settings is not affected by the global rotation, if any (see +link{DrawPane.rotation}).
    //
    // @value "neverRotate" the <code>titleLabel</code> is never rotated with the item.
    // @value "withItem" the <code>titleLabel</code> is rotated exactly to match the item's
    // rotation (see +link{DrawItem.rotation}).
    // @value "withItemAlwaysUp" the <code>titleLabel</code> is rotated exactly to match the
    // item's rotation, except that at certain rotations, the <code>titleLabel</code> is flipped
    // by 180&deg; so that the title text is never upside down.
    // @value "withLine" (applies only to +link{DrawLine} and +link{DrawLinePath}) the
    // <code>titleLabel</code> is rotated to match the line or center segment.
    // <p>
    // If used on a <code>DrawItem</code> that is not a <code>DrawLine</code> or <code>DrawLinePath</code>,
    // then the effect is the same as
    // <smartclient>"withItem".</smartclient>
    // <smartgwt>{@link com.smartgwt.client.types.TitleRotationMode#WITH_ITEM}.</smartgwt>
    // @value "withLineAlwaysUp" (applies only to +link{DrawLine} and +link{DrawLinePath}) the
    // <code>titleLabel</code> is rotated to match the line or center segment, except that
    // at certain rotations, the <code>titleLabel</code> is flipped by 180&deg; so that the
    // title text is never upside down.
    // <p>
    // If used on a <code>DrawItem</code> that is not a <code>DrawLine</code> or <code>DrawLinePath</code>,
    // then the effect is the same as
    // <smartclient>"withItemAlwaysUp".</smartclient>
    // <smartgwt>{@link com.smartgwt.client.types.TitleRotationMode#WITH_ITEM_ALWAYS_UP}.</smartgwt>
    // @visibility drawing
    //<

    //> @attr drawItem.titleRotationMode (TitleRotationMode : "neverRotate" : IRA)
    // The mode in which the +link{DrawItem.titleLabel,titleLabel} (if shown) is rotated with
    // this draw item.
    // @see DrawItem.title
    // @example TitleRotationMode
    // @visibility drawing
    //<
    titleRotationMode: "neverRotate",

    //> @attr drawItem.showTitleLabelBackground (boolean : false : IRA)
    // If the +link{DrawItem.titleLabel,titleLabel} is showing, should the +link{drawItem.titleLabelBackground,titleLabelBackground}
    // be created and placed behind the <code>titleLabel</code>?
    // <p>
    // This defaults to true for +link{DrawSector}s and shapes that are not commonly filled
    // (e.g. +link{DrawLine}s).
    // @visibility drawing
    //<
    showTitleLabelBackground: false,

    //> @attr drawItem.titleLabelBackground (DrawRect AutoChild : null : RA)
    // When the +link{DrawItem.titleLabel,titleLabel} is showing and +link{showTitleLabelBackground,showTitleLabelBackground}
    // is <code>true</code>, this +link{DrawRect} AutoChild is created and placed behind the
    // <code>titleLabel</code>.
    // @see DrawItem.titleLabelPadding
    // @visibility drawing
    //<
    titleLabelBackgroundDefaults: {
        _constructor: "DrawRect",
        lineColor: null,
        lineWidth: 1,
        fillColor: "#ffffff",
        fillOpacity: 0.9
    },

    //> @attr drawItem.titleLabelPadding (int : 2 : IRA)
    // If the +link{titleLabelBackground,titleLabelBackground} is visible, how much padding
    // should be left between the bounds of the +link{DrawItem.titleLabel,titleLabel} and the edges of
    // the <code>titleLabelBackground</code>?
    // @visibility drawing
    //<
    titleLabelPadding: 2,


    //> @attr drawItem.keepInParentRect (Boolean | Array of Float : null : IRWA)
    // Constrains drag-resizing and drag-repositioning of this draw item to either the current
    // visible area of the +link{drawPane,draw pane} or an arbitrary bounding box (if set to
    // an array of the form <code>[left, top, left + width, top + height]</code>).  When using
    // a bounding box-type argument the left/top values can be negative, or the width/height
    // values can be greater than the dimensions of the viewable area, to allow positioning
    // or resizing the draw item beyond the confines of the draw pane.
    // <P>
    // Note:  keepInParentRect affects only user drag interactions, not programmatic moves or
    // resizes.
    //
    // @visibility drawing
    //<

    //> @attr drawItem.resizeViaLocalTransformOnly (boolean : false : IR)
    // If this DrawItem is showing <smartclient>"resize"</smartclient>
    // <smartgwt>{@link com.smartgwt.client.types.KnobType#RESIZE}</smartgwt>
    // +link{drawItem.knobs,control knobs}, should resizing the shape solely update the local
    // transform (for example, the DrawItem's +link{drawItem.scale,scale} or
    // +link{drawItem.translate,translation})?.
    // <p>
    // The default is <code>false</code>, which means that the DrawItem is allowed to modify
    // its shape properties in order to fit within a given width and height.  Some examples:
    // <ul>
    // <li>A +link{class:DrawOval,DrawOval} might decrease its +link{drawOval.radius,radius}
    // when resized to a smaller size.</li>
    // <li>A +link{class:DrawPath,DrawPath} might change its +link{drawPath.points,points}
    // to lengthen all line segments in the path by some proportion so that it fits into a
    // larger size.</li>
    // </ul>
    // This approach allows a DrawItem to maintain the same
    // +link{drawItem.lineWidth,line width} even as it is being resized.
    // <p>
    // If this property is set to <code>true</code> then all visual aspects of the DrawItem,
    // including the line width and the fill, will be magnified or reduced during resizes
    // as if the DrawItem were placed under a lens.
    // @visibility drawing
    //<
    resizeViaLocalTransformOnly: false,

    //> @attr drawItem.proportionalResizing (ProportionalResizeMode : "modifier" : IR)
    // This property specifies the conditions for when proportional resizing is used.
    // <p>
    // By default the DrawItem is forced to only resize proportionally while any modifier key
    // specified in +link{proportionalResizeModifiers} is pressed.  For example, the
    // DrawItem will change its width and height by the same percentage as long as the "Shift"
    // key is held down.
    // <p>
    // Note that this property only has an effect if the DrawItem is showing
    // <smartclient>"resize"</smartclient>
    // <smartgwt>{@link com.smartgwt.client.types.KnobType#RESIZE}</smartgwt>
    // +link{drawItem.knobs,control knobs}.
    // @visibility drawing
    //<
    proportionalResizing: "modifier",

    //> @attr drawItem.proportionalResizeModifiers (Array of KeyName : ["Shift"] : IR)
    // If +link{proportionalResizing} is set to <smartclient>"modifier"</smartclient>
    // <smartgwt>{@link com.smartgwt.client.types.ProportionalResizeMode#MODIFIER}</smartgwt>
    // or <smartclient>"modifierOff"</smartclient>
    // <smartgwt>{@link com.smartgwt.client.types.ProportionalResizeMode#MODIFIER_OFF}</smartgwt>
    // then proportional resizing of the DrawItem is activated or deactivated, respectively,
    // whenever at least one key in this set of modifier keys is pressed.
    // <p>
    // The keys allowed in this set are:  "Alt", "Ctrl", and "Shift".  If this set
    // of keys is empty then proportional resizing is always used if
    // <code>proportionalResizing</code> is <smartclient>"modifier"</smartclient>
    // <smartgwt><code>MODIFIER</code></smartgwt> and is never used if
    // <code>proportionalResizing</code> is <smartclient>"modifierOff"</smartclient>
    // <smartgwt><code>MODIFIER_OFF</code></smartgwt>.
    // @visibility drawing
    //<
    proportionalResizeModifiers: ["Shift"],

    _getProportionalResizing : function () {
        
        var proportionalResizing = this.proportionalResizing,
            always = proportionalResizing == "always",
            modifier = !always && proportionalResizing == "modifier",
            modifierOff = !(always || modifier) && proportionalResizing == "modifierOff";
        if (!(always || modifier || modifierOff)) { // including "none"
            return "none";
        }
        var positions = this.resizeKnobPoints,
            supportsProportionalResizing = (
                // If the DrawItem is being destroyed then the `proportionalResizing` mode
                // should be "none" so that the DrawItem will stop listening for the modifier
                // keys.
                !this.destroying &&
                !this.destroyed &&
                // Proportional resizing is only applicable when there are corner resize knobs.
                this.knobs != null &&
                this.knobs.contains("resize") &&
                positions != null &&
                positions.length > 0 &&
                (positions.contains("TL") || positions.contains("TR") ||
                    positions.contains("BL") || positions.contains("BR")) &&
                
                this.dragResizeMove == isc.DrawItem.getInstanceProperty("dragResizeMove"));
        if (!supportsProportionalResizing) {
            return "none";
        } else {
            // If the set of keys is empty then proportional resizing is always used if
            // `modifier` (and never used if `modifierOff`).
            var emptyModifierSet = (
                    isc.Page._getModifierKeysFlags(this.proportionalResizeModifiers) == 0);
            if (emptyModifierSet) {
                if (modifier) {
                    return "always";
                } else if (modifierOff) {
                    return "none";
                }
            }

            // Otherwise simply return the property value.
            return proportionalResizing;
        }
    },

    //drawingType: null, // vml, bitmap, svg - only respected for implicit DrawPane
    
    
    autoOffset:true,
    canDrag: false,

    //> @attr drawItem.hitTolerance (double : 0 : IRA)
    // A tolerance applied in checks for whether a mouse event is within this <code>DrawItem</code>.
    // Increasing the tolerance makes this <code>DrawItem</code> easier to click, drag, etc.
    // <p>
    // The tolerance is applied in the item's local coordinate system. If there is no scaling
    // in effect, either globally on the <code>DrawPane</code> or locally via a nontrivial
    // +link{DrawItem.scale,scale}, then this tolerance can be thought of as a distance in pixels.
    // <p>
    // This setting is only supported in Firefox 19+, Safari 6.1+, and Chrome 27+.
    //<
    
    hitTolerance: 0,


addPeer : function (newPeer, name, autoDraw) {
    if (!newPeer) return null;

    var oldMasterElement = newPeer.masterElement;
    if (oldMasterElement === this) return null;
    else if (oldMasterElement != null) newPeer.depeer(name);

    newPeer.masterElement = this;
    if (name != null) this[name] = newPeer;

    var peers = this.peers;
    if (peers == null) {
        peers = this.peers = [newPeer];
    } else {
        
        if (!peers.contains(newPeer)) {
            peers.add(newPeer);
        }
    }

    if (this.drawGroup != null) {
        this.drawGroup.addDrawItem(newPeer);
    } else if (this.drawPane != null) {
        this.drawPane.addDrawItem(newPeer, false);
        
        if (!this.drawPane.drawItems.contains(newPeer)) {
            this.drawPane.drawItems.add(newPeer);
        }
    }

    if (autoDraw === false) return newPeer;

    if (this._drawn && !newPeer._drawn) {
        newPeer.draw();
    }

    return newPeer;
},

removePeer : function (peer, name) {
    if (!peer) return;

    var peers = this.peers;
    if (peers == null) {
    //>DEBUG
        this.logWarn("Attempt to remove peer: " + peer + " from DrawItem that does not have peers");
    //<DEBUG
        return;
    }
    var index = peers.indexOf(peer);
    if (index < 0) {
    //>DEBUG
        this.logWarn("Attempt to remove peer: " + peer + " from DrawItem that is not its master");
    //<DEBUG
        return;
    }

    if (this.drawGroup) {
        this.drawGroup.removeDrawItem(peer);
    } else if (this.drawPane) {
        this.drawPane.removeDrawItem(peer);
    }

    // remove our links to the peer
    peers.removeAt(index);
    if (this[name] === peer) this[name] = null;
    // remove peer's link to us
    peer.masterElement = null;
    // fire notifications
    if (peer.depeered) peer.depeered(this, name);
    if (this.peerRemoved) this.peerRemoved(peer, name);
},

depeer : function (name) {
    var masterElement = this.masterElement;
    if (masterElement == null) return;
    masterElement.removePeer(this, name);
},

addPeer : function (newPeer, name, autoDraw) {
    if (!newPeer) return null;

    var oldMasterElement = newPeer.masterElement;
    if (oldMasterElement === this) return null;
    else if (oldMasterElement != null) newPeer.depeer(name);

    newPeer.masterElement = this;
    if (name != null) this[name] = newPeer;

    var peers = this.peers;
    if (peers == null) {
        peers = this.peers = [newPeer];
    } else {
        
        if (!peers.contains(newPeer)) {
            peers.add(newPeer);
        }
    }

    if (this.drawGroup != null) {
        this.drawGroup.addDrawItem(newPeer);
    } else if (this.drawPane != null) {
        this.drawPane.addDrawItem(newPeer, false);
        
        if (!this.drawPane.drawItems.contains(newPeer)) {
            this.drawPane.drawItems.add(newPeer);
        }
    }

    if (autoDraw === false) return newPeer;

    if (this.isDrawn() && !newPeer.isDrawn()) {
        newPeer.draw();
    }

    return newPeer;
},

removePeer : function (peer, name) {
    if (!peer) return;

    var peers = this.peers;
    if (peers == null) {
    //>DEBUG
        this.logWarn("Attempt to remove peer: " + peer + " from DrawItem that does not have peers");
    //<DEBUG
        return;
    }
    var index = peers.indexOf(peer);
    if (index < 0) {
    //>DEBUG
        this.logWarn("Attempt to remove peer: " + peer + " from DrawItem that is not its master");
    //<DEBUG
        return;
    }

    if (this.drawGroup) {
        this.drawGroup.removeDrawItem(peer);
    } else if (this.drawPane) {
        this.drawPane.removeDrawItem(peer);
    }

    // remove our links to the peer
    peers.removeAt(index);
    if (this[name] === peer) this[name] = null;
    // remove peer's link to us
    peer.masterElement = null;
    // fire notifications
    if (peer.depeered) peer.depeered(this, name);
    if (this.peerRemoved) this.peerRemoved(peer, name);
},

depeer : function (name) {
    var masterElement = this.masterElement;
    if (masterElement == null) return;
    masterElement.removePeer(this, name);
},

//> @method drawItem.setPropertyValue()
// Sets a property on this DrawItem, calling the appropriate setter method if one is found and
// is +link{class.isMethodSupported(),supported}.
// @param propertyName (String) name of the property to set
// @param newValue (any) new value for the property
// @see method:class.setProperty()
// @visibility drawing
//<

setPropertyValue : function (propertyName, newValue) {
    var setter = this._getSetter(propertyName),
        props;
    if (isc.isA.StringMethod(newValue)) {
        newValue = newValue.getValue();
    }
    if (setter != null) {
        // Check isMethodSupported() to avoid warnings for attempts to set unsupported
        // properties.
        if (!this.getClass().isMethodSupported(setter)) {
            return;
        }
        this[setter](newValue);
    } else {
        props = {};
        props[propertyName] = newValue;
        this.addProperties(props);
    }

    // Fire the notification function.
    if (this.propertyChanged) {
        this.propertyChanged(propertyName, newValue);
    }

    // Fire any "doneSettingProperties()" - allows the instance to respond to multiple
    // related properties being set without having to respond to each one.
    if (this.doneSettingProperties) {
        if (setter != null) {
            props = {};
            props[propertyName] = newValue;
        }
        this.doneSettingProperties(props);
    }
},

_isClosed : function () {
    return false;
},


//> @method drawItem.getZIndex()
// Returns the +link{DrawItem.zIndex,zIndex} of this draw item.
// @param [resolveToNumber] (Boolean) if true and this item's <code>zIndex</code> is null,
// resolve to the next higher zIndex.
// @return (Integer) this draw item's zIndex, or null if not resolved yet. If the <code>resolveToNumber</code>
// parameter is true, then the returned integer is guaranteed to be non-null.
// @group zIndex
// @visibility drawing
//<
getZIndex : function (resolveToNumber) {
    var zIndex = this.zIndex;
    if (resolveToNumber && zIndex == null) {
        if (this.drawToBack) {
            zIndex = this.zIndex = (isc.DrawItem._nextDrawToBackZIndex += 9);
        } else {
            zIndex = this.zIndex = (isc.DrawItem._nextZIndex += 18);
        }
    }
    return zIndex;
},

//> @method drawItem.setZIndex() (A)
// Setter for +link{DrawItem.zIndex}.
// @param newZIndex (Integer) new <code>zIndex</code>. If null and this draw item is already
// in a <code>DrawPane</code> or <code>DrawGroup</code>, then this item's zIndex will be set
// to the next higher auto-assigned zIndex.
// <p>
// Note that when setting draw items' zIndexes via this advanced API, the application should
// take over management of all draw items' zIndexes, and +link{DrawItem.bringToFront(),bringToFront()}
// / +link{DrawItem.sendToBack(),sendToBack()} should not be used, as those APIs assume
// automatic management of zIndexes.
// @group zIndex
// @visibility drawing
//<

setZIndex : function (newZIndex) {
    if (this.zIndex == newZIndex) return;

    this.zIndex = newZIndex;

    var parent,
        siblingItems;
    if (this.drawGroup != null) {
        parent = this.drawGroup;
        siblingItems = parent.drawItems;
    } else if (this.drawPane != null) {
        parent = this.drawPane;
        siblingItems = parent.drawItems;
    }
    if (!isc.isAn.Array(siblingItems) || siblingItems.length == 1) return;
    if (newZIndex == null) newZIndex = this.getZIndex(true);

    // First remove this draw item and figure out where to re-insert.
    var oldI = siblingItems.indexOf(this);
    var wasFound;
    if (oldI < 0) wasFound = false;
    else {
        wasFound = true;
        siblingItems.removeAt(oldI);
    }
    
    var i = isc.Array._binarySearch(siblingItems, this, isc.DrawItem._zIndexCompareFunc);
    if (i < 0) i = -i - 1;
    siblingItems.addAt(this, i);

    var peers = this.peers;
    if (peers != null) {
        for (var peerIndex = 0, numPeers = peers.length; peerIndex < numPeers; ++peerIndex) {
            var peer = peers[peerIndex];
            if (peer._sameZIndexAsMaster) {
                peer.zIndex = newZIndex;
                if (siblingItems.remove(peer)) {
                    siblingItems.addAt(peer, ++i);
                }
            }
        }
    }

    // If this item is being inserted at the exact same spot, then return early - no need to
    // do anything to update the display.
    if (oldI == i) return;

    if (this.drawingSVG) {
        // We need to loop through the siblings at indices i+1, i+2, etc.
        // It may be that some of these items do not have an SVG handle yet (batch drawing),
        // so we'll need to find one that does in order to insert this item's SVG handle before.
        // If there are none, then simply append this item's SVG handle.

        // Find the first drawn sibling after this item.
        var siblingHandle = null;
        for (i = i + 1; i < siblingItems.length; ++i) {
            var sibling = siblingItems[i];
            if (sibling.drawingSVG) {
                if (isc.isA.DrawGroup(sibling) && !sibling.renderGroupElement) {
                    
                }
                siblingHandle = sibling._svgHandle;
                break;
            }
        }
        

        this._svgHandle.parentNode.insertBefore(this._svgHandle, siblingHandle);
        

        if (peers != null) {
            for (var peerIndex = 0, numPeers = peers.length; peerIndex < numPeers; ++peerIndex) {
                var peer = peers[peerIndex];
                if (peer._sameZIndexAsMaster && peer.isDrawn()) {
                    peer._svgHandle.parentNode.insertBefore(peer._svgHandle, siblingHandle);
                    
                }
            }
        }

    } else if (this.drawingVML) {
        if (isc.isA.DrawGroup(this) && !this.renderGroupElement) {
            
        } else {
            this._vmlRendererHandle.setProperty("zIndex", newZIndex).flush();

            if (peers != null) {
                for (var peerIndex = 0, numPeers = peers.length; peerIndex < numPeers; ++peerIndex) {
                    var peer = peers[peerIndex];
                    if (peer._sameZIndexAsMaster && peer.isDrawn()) {
                        peer._vmlRendererHandle.setProperty("zIndex", newZIndex).flush();
                    }
                }
            }
        }

    // If bitmap drawing, then this item has been inserted into the correct place, so we just
    // need to schedule a redraw.
    } else if (this.drawingBitmap) {
        this.drawPane.redrawBitmap();
    }
},

//> @method drawItem.bringToFront()
// Places this draw item at the top of the stacking order so that it appears in front of other
// draw items in the same +link{DrawPane} or +link{DrawGroup}.
// <p>
// When the <code>DrawPane</code>'s +link{DrawPane.drawingType,drawingType} is
// <smartclient>"bitmap",</smartclient>
// <smartgwt>{@link com.smartgwt.client.types.DrawingType#BITMAP},</smartgwt>
// +link{DrawItem.zIndex}, bringToFront(), and +link{DrawItem.sendToBack()} are not supported
// for +link{DrawLabel}s on iOS due to platform limitations.
// @see DrawItem.sendToBack()
// @see DrawItem.setZIndex()
// @group zIndex
// @visibility drawing
//<
bringToFront : function () {
    this.setZIndex(isc.DrawItem._BIG_Z_INDEX += 18);
    if (this.titleLabel) this.titleLabel.bringToFront();
},

//> @method drawItem.sendToBack()
// Places this draw item at the bottom of the stacking order so that it appears behind other
// draw items in the same +link{DrawPane} or +link{DrawGroup}.
// <p>
// When the <code>DrawPane</code>'s +link{DrawPane.drawingType,drawingType} is
// <smartclient>"bitmap",</smartclient>
// <smartgwt>{@link com.smartgwt.client.types.DrawingType#BITMAP},</smartgwt>
// +link{DrawItem.zIndex}, +link{DrawItem.bringToFront()}, and sendToBack() are not supported
// for +link{DrawLabel}s on iOS due to platform limitations.
// @see DrawItem.bringToFront()
// @see DrawItem.setZIndex()
// @group zIndex
// @visibility drawing
//<
sendToBack : function () {
    if (this.titleLabel) this.titleLabel.sendToBack();
    this.setZIndex(isc.DrawItem._SMALL_Z_INDEX -= 18);
},

//> @method drawItem.setCursor()
//
// @param newCursor (Cursor) new cursor.
// @visibility drawing
//<
setCursor : function (newCursor) {
    this.cursor = newCursor;
},

//> @method drawItem.setDragRepositionCursor()
// Setter for +link{DrawItem.dragRepositionCursor,dragRepositionCursor}.
//
// @param dragRepositionCursor (Cursor) new value for <code>this.dragRepositionCursor</code>.
//<
setDragRepositionCursor : function (dragRepositionCursor) {
    this.dragRepositionCursor = dragRepositionCursor;
},

getCurrentCursor : function () {
    if (this.canDrag) return this.dragRepositionCursor;
    return this.cursor;
},

_updateQuadTreeItem : function () {
    var item = this.item;
    if (item != null) {
        var bboxPrime = this._getTransformedBoundingBox(true, true, true, this._tempBoundingBox);
        this.drawPane.quadTree.update(
            item,
            bboxPrime[0],
            bboxPrime[1],
            (bboxPrime[2] - bboxPrime[0]),
            (bboxPrime[3] - bboxPrime[1]));
    }
},


_resolve : function (
    outputCoordinateSystem, position, boxCoordinateSystem, includeStroke, includeHitTolerance,
    round, output)
{
    
    if (boxCoordinateSystem == null) {
        boxCoordinateSystem = outputCoordinateSystem;
    }

    var isLeft = position.contains("L"),
        isRight = !isLeft && position.contains("R"),
        isTop = position.contains("T"),
        isBottom = !isTop && position.contains("B"),
        isCenter = !(isLeft || isRight || isTop || isBottom) && position.contains("C"),
        isRotationCenter = (
            !(isLeft || isRight || isTop || isBottom || isCenter) &&
            position.contains("X"));
    if (!(isLeft || isRight || isTop || isBottom || isCenter || isRotationCenter)) {
        return null;
    }
    if (isRotationCenter) {
        // The DrawItem's center of rotation is only defined with respect to the local
        // coordinate system.
        boxCoordinateSystem = "local";
    }
    if (this.drawPane == null) {
        if (boxCoordinateSystem == "global") {
            boxCoordinateSystem = "drawing";
        }
        if (outputCoordinateSystem == "global") {
            outputCoordinateSystem = "drawing";
        }
    }

    var x = 0, y = 0;
    if (isRotationCenter) {
        var center = this._getRotationCenter();
        if (center.cx != null && center.cy != null) {
            x = center.cx;
            y = center.cy;
        } else {
            return null;
        }
    } else {
        var bbox = null;
        if (boxCoordinateSystem == "local") {
            bbox = this.getBoundingBox(includeStroke, this._tempBoundingBox);
            if (includeHitTolerance) {
                bbox = this._adjustBoundingBox(false, true, bbox);
            }
        } else if (boxCoordinateSystem == "drawing") {
            bbox = this._getTransformedBoundingBox(includeStroke, true, includeHitTolerance, bbox);
        } else { // boxCoordinateSystem == "global"
            bbox = this._getTransformedBoundingBox(includeStroke, false, includeHitTolerance, bbox);
        }

        if (isLeft) {
            x = bbox[0];
        } else if (isRight) {
            x = bbox[2];
        } else {
            x = (bbox[0] + bbox[2]) / 2;
        }
        if (isTop) {
            y = bbox[1];
        } else if (isBottom) {
            y = bbox[3];
        } else {
            y = (bbox[1] + bbox[3]) / 2;
        }
    }

    // Transform the point into the specified output coordinate system.
    if (boxCoordinateSystem != outputCoordinateSystem) {
        var transform = this._getNormalizeTransform(
                boxCoordinateSystem, outputCoordinateSystem),
            newX = transform.m00 * x + transform.m01 * y + transform.m02,
            newY = transform.m10 * x + transform.m11 * y + transform.m12;
        x = newX;
        y = newY;
    }

    if (round) {
        x = Math.round(x);
        y = Math.round(y);
    }

    output = output || new Array(2);
    output[0] = x;
    output[1] = y;
    return output;
},

//> @method drawItem.getCenter()
// Returns the center point of this draw item in local coordinates. Generally this is the
// center of the +link{DrawItem.getBoundingBox(),bounding box}, but some item types may use
// a different point. For example, +link{DrawTriangle} uses the
// +externalLink{http://en.wikipedia.org/wiki/Incenter#Cartesian_coordinates,incenter}
// of the triangle.
// @return (Point) the center point in local coordinates
// @visibility drawing
//<
getCenter : function () {
    var bbox = this.getBoundingBox(false, this._tempBoundingBox);
    return [isc.DrawItem._makeCoordinate((bbox[0] + bbox[2]) / 2), isc.DrawItem._makeCoordinate((bbox[1] + bbox[3]) / 2)];
},

//> @method drawItem.getBoundingBox()
// Calculates the bounding box of the shape in the +link{DrawPane,local coordinate system}.
// <p>
// Note that the bounding box of the shape when transformed into the global coordinate system
// is available from the method +link{getResizeBoundingBox()}.
//
// @return (Array[] of double) the x1, y1, x2, y2 coordinates. When the width and height are both positive,
// point (x1, y1) is the top-left point of the bounding box and point (x2, y2) is the bottom-right
// point of the bounding box.
//
// @see DrawPane
// @visibility drawing
//<

_boundingBox: [0, 0, 0, 0],
getBoundingBox : function (includeStroke, outputBox) {
    
    var box = (outputBox || new Array(4));
    for (var i = 4; i--; ) {
        box[i] = this._boundingBox[i];
    }
    return box;
},


_tempBoundingBox: new Array(4),

_adjustBoundingBox : function (forStroke, forHitTolerance, bbox) {
    var halfLineWidth = (forStroke && this._hasStroke() && !isc.isA.DrawLabel(this) ? (this.lineWidth / 2) : 0),
        hitTolerance = (forHitTolerance ? this.hitTolerance : 0),
        offset = halfLineWidth + hitTolerance;

    if (offset != 0) {
        if (bbox[0] <= bbox[2]) {
            bbox[0] -= offset;
            bbox[2] += offset;
        } else {
            bbox[2] -= offset;
            bbox[0] += offset;
        }

        if (bbox[1] <= bbox[3]) {
            bbox[1] -= offset;
            bbox[3] += offset;
        } else {
            bbox[3] -= offset;
            bbox[1] += offset;
        }
    }
    return bbox;
},

//> @method drawItem.getResizeBoundingBox()
// Calculates the bounding box of the shape reflected by the
// +link{showResizeOutline,resize outline} shown when dragging the
// +link{resizeKnobPoints,resize knobs}.  This method is similar to +link{getBoundingBox()}
// except that the coordinates returned by this method are in the global coordinate system
// (described +link{class:DrawPane,here}) rather than the local coordinate system.
// @return (array) the x1, y1, x2, y2 coordinates. When the width and height are both positive,
// point (x1, y1) is the top-left point of the bounding box and point (x2, y2) is the bottom-right
// point of the bounding box.
// @see drawItem.getBoundingBox()
// @visibility drawing
//<
getResizeBoundingBox : function (outputBox) {
    
    return this._getTransformedBoundingBox(true, false, false, outputBox);
},

// Calculate a bounding box of the DrawItem in the global coordinate system. This might not be
// a tight bounding box.
//
// The default implementation takes DrawItem's bounding box (in the local coordinate system),
// transforms it by the local and global transforms, and then returns the bounding box of the
// transformed box.  This is not the same as the bounding box of the shape in the global
// coordinate system, but it is inclusive.
// @return (Array of double) the x1', y1', x2', y2' coordinates
_getTransformedBoundingBox : function (
    includeStroke, excludeGlobalTransform, includeHitTolerance, outputBox)
{
    

    var t;
    if (excludeGlobalTransform || this.drawPane == null) {
        t = this._getLocalTransform();
    } else {
        t = this.drawPane._getGlobalTransform().duplicate().rightMultiply(this._getLocalTransform());
    }

    return this._getBoundingBoxOfTransformedShape(
        t, includeStroke, includeHitTolerance, outputBox);
},
_getBoundingBoxOfTransformedShape : function (
    transform, includeStroke, includeHitTolerance, outputBox)
{
    

    // http://stackoverflow.com/questions/622140/calculate-bounding-box-coordinates-from-a-rotated-rectangle-picture-inside

    var bbox = this.getBoundingBox(includeStroke, outputBox);
    // Since the hit tolerance is interpreted in the item's local coordinate system, the hit tolerance
    // is applied to the bounding box before being transformed.
    this._adjustBoundingBox(false, includeHitTolerance, bbox);

    var v1 = transform.transform(bbox[0], bbox[1]),
        v2 = transform.transform(bbox[2], bbox[1]),
        v3 = transform.transform(bbox[2], bbox[3]),
        v4 = transform.transform(bbox[0], bbox[3]);

    bbox[0] = Math.min(v1[0], v2[0], v3[0], v4[0]);
    bbox[1] = Math.min(v1[1], v2[1], v3[1], v4[1]);
    bbox[2] = Math.max(v1[0], v2[0], v3[0], v4[0]);
    bbox[3] = Math.max(v1[1], v2[1], v3[1], v4[1]);
    return bbox;
},

// isInBounds / isPointInPath used for event handling.



//> @method drawItem.isInBounds(x,y)
// Returns true if the given point in the drawing coordinate system, when converted to
// coordinates in this DrawItem's local coordinate system, is within the
// +link{getBoundingBox(),bounding box} of this DrawItem's shape.
// <p>
// This method can be used to quickly check whether the given point is definitely <em>not</em> within
// the DrawItem shape. To check whether the point is within the DrawItem shape, use the slower
// but exact +link{isPointInPath()} method.
// @param x (int) X coordinate of the point in the drawing coordinate system.
// @param y (int) Y coordinate of the point in the drawing coordinate system.
// @return (boolean)
// @visibility drawing
//<
isInBounds : function (x, y) {
    var normalized = this._normalize(x, y),
        b = this.getBoundingBox(true, this._tempBoundingBox),
        hitTolerance = this.hitTolerance;

    
    return (
        ((b[0] - hitTolerance <= normalized[0] && normalized[0] <= b[2] + hitTolerance) ||
         (b[2] - hitTolerance <= normalized[0] && normalized[0] <= b[0] + hitTolerance)) &&
        ((b[1] - hitTolerance <= normalized[1] && normalized[1] <= b[3] + hitTolerance) ||
         (b[3] - hitTolerance <= normalized[1] && normalized[1] <= b[1] + hitTolerance)));
},

_useExemptHack : function () {
    
    return (
        this.exemptFromGlobalTransform &&
        this.drawPane != null &&
        this.drawPane.drawingType != "bitmap");
},

//> @attr drawItem.transform (AffineTransform : null : IA)
// The transform to apply to the DrawItem to map it from local coordinates to drawing
// coordinates during rendering.  The coordinate systems used in a drawing are described
// +link{class:DrawPane,here}.
// <p>
// Note that the local transform may be equivalently specified via the DrawItem's
// +link{drawItem.translate,translate}, +link{drawItem.scale,scale},
// +link{drawItem.xShearFactor,xShearFactor}, +link{drawItem.yShearFactor,yShearFactor}, and
// +link{drawItem.rotation,rotation} properties.  If the <code>transform</code> property is
// not null then it takes precedence over the settings of these other properties.
// @visibility customTransform
//<



//> @attr drawItem.shapeData (object : null : I)
// An opaque object specifying the local transformation that should be applied to this
// <code>DrawItem</code>, obtained through a call to +link{getShapeData()}.<p>
// <b>Note:</b> if this property is specified, you should avoid also specifying a
// +link{translate}, +link{scale}, +link{xShearFactor}, +link{yShearFactor}, or +link{rotation}.
// @visibility drawing
//<

//> @method drawItem.getShapeData()
// Returns an opaque JavaScript object representing the current local transformation applied to
// the DrawItem's local coordinates, as defined by +link{translate}, +link{scale}, 
// +link{xShearFactor}, +link{yShearFactor}, and +link{rotation}.  The object may be serialized
// and deserialized as JSON, and passed into the constructor block as +link{shapeData} to
// restore the local transformation.<p>
// <b>Note:</b> this doesn't include any sepatate configuration, such as for a +link{DrawRect}
// the current values of +link{DrawRect.left,left}, +link{DrawRect.top,top},
// +link{DrawRect.width,width}, or +link{DrawRect.height,height}.
//
// @return (object) opaque tranformation data
// @see JSON.encode()
// @visibility drawing
//<
getShapeData : function () {
    var transform = this.getTransform();
    // change AffineTransform instance to a plain JSO to simplify serialization
    return {
        m00: transform.m00, m01: transform.m01, m02: transform.m02,
        m10: transform.m10, m11: transform.m11, m12: transform.m12
    };
},

//> @method drawItem.getTransform() [A]
// Returns a copy of the current local transform being applied to this DrawItem.  This
// transform takes coordinates in the DrawItem's local coordinate system into the shared
// drawing coordinate system (described +link{class:DrawPane,here}).
// @return (AffineTransform)
// @visibility customTransform
//<
getTransform : function () {
    return this._getLocalTransform(true).duplicate();
},

//> @method drawItem.setTransform() [A]
// Setter for +link{drawItem.transform}.
// @see drawItem.scaleBy()
// @see drawItem.rotateBy()
// @param transform (AffineTransform) the new local transform
// @visibility customTransform
//<
setTransform : function (transform) {
    if (isc.isAn.AffineTransform(transform)) {
        var center0 = this._getRotationCenter();
        this._updateLocalTransform(transform, center0.cx, center0.cy, null, true, true);
    }
},



_getLocalTransform : function (withoutExemptHack) {
    var useExemptHack = (!withoutExemptHack && this._useExemptHack()),
        t = this._transform;
    if (t != null && !useExemptHack) return t;

    if (useExemptHack) {
        // Start with a transform that undoes DrawPane.translate, zoomLevel, and rotation.
        t = this.drawPane._getInverseGlobalTransform().duplicate();
        t.translate(this.drawPane.scrollLeft, this.drawPane.scrollTop);
    } else {
        // Start with the identity transform.
        t = isc.AffineTransform.create();
    }

    if (this.translate) {
        t.translate(this.translate[0], this.translate[1]);
    }
    if (this.scale && this.scale.length === 2) {
        t.scale(this.scale[0], this.scale[1]);
    }
    if (this.xShearFactor) {
        t.xShear(this.xShearFactor);
    }
    if (this.yShearFactor) {
        t.yShear(this.yShearFactor);
    }
    var rotation = this.rotation,
        center = this._getRotationCenter();
    if (center.cx != null && center.cy != null && rotation) {
        t.rotate(rotation, center.cx, center.cy);
    }
    if (!useExemptHack) {
        this._transform = t;
    }
    return t;
},

_getInverseLocalTransform : function () {
    var inverseTransform = this._inverseTransform,
        transform = this._getLocalTransform();
    if (inverseTransform == null || transform != this._inverseInverseTransform) {
        inverseTransform = this._inverseTransform = transform.getInverse();
        this._inverseInverseTransform = transform;
    }
    return inverseTransform;
},

_rotationCenter: { cx: 0, cy: 0 },
_getRotationCenter : function () {
    var output = this._rotationCenter;
    if (isc.isA.DrawLabel(this) && !this._isTitleLabel) {
        output.cx = this.left;
        output.cy = this.top + (this.drawPane != null ? this._calculateAlignMiddleCorrection() : 0);
    } else {
        var center = this.getCenter && this.getCenter();
        if (center && center.length === 2) {
            output.cx = center[0];
            output.cy = center[1];
        } else {
            output.cx = output.cy = null;
        }
    }
    return output;
},


_normalize : function (x, y, inputCoordinateSystem, outputCoordinateSystem) {
    if (inputCoordinateSystem == null) {
        inputCoordinateSystem = "drawing"
    }
    if (outputCoordinateSystem == null) {
        outputCoordinateSystem = "local";
    }
    var transform = this._getNormalizeTransform(inputCoordinateSystem, outputCoordinateSystem);
    return transform.transform(x, y);
},

_getNormalizeTransform : function (inputCoordinateSystem, outputCoordinateSystem) {
    
    if (this.drawPane == null) {
        if (inputCoordinateSystem == "global") {
            inputCoordinateSystem = "drawing";
        }
        if (outputCoordinateSystem == "global") {
            outputCoordinateSystem = "drawing";
        }
    }

    if (inputCoordinateSystem == outputCoordinateSystem) {
        return isc.AffineTransform.getIdentityTransform();
    } else {
        if (this.exemptFromGlobalTransform && this.drawPane != null) {
            
            var scrollLeft = this.drawPane.scrollLeft,
                scrollTop = this.drawPane.scrollTop;
            if (inputCoordinateSystem == "local") {
                if (outputCoordinateSystem == "drawing") {
                    return (
                        this.drawPane._getInverseGlobalTransform().duplicate()
                            .translate(scrollLeft, scrollTop)
                            .leftMultiply(this._getLocalTransform(true)));
                } else { // outputCoordinateSystem == "global"
                    return (
                        this._getLocalTransform(true).duplicate()
                            .preTranslate(scrollLeft, scrollTop));
                }
            } else if (inputCoordinateSystem == "drawing") {
                if (outputCoordinateSystem == "local") {
                    return (
                        this._getLocalTransform(true).getInverse()
                            .translate(-scrollLeft, -scrollTop)
                            .rightMultiply(this.drawPane.getGlobalTransform()));
                } else { // outputCoordinateSystem == "global"

                    // Same as below.
                }
            } else { // inputCoordinateSystem == "global"
                if (outputCoordinateSystem == "local") {
                    return (
                        this._getLocalTransform(true).getInverse()
                            .translate(-scrollLeft, -scrollTop));
                } else { // outputCoordinateSystem == "drawing"

                    // Same as below.
                }
            }
        }

        if (inputCoordinateSystem == "local") {
            if (outputCoordinateSystem == "drawing") {
                return this._getLocalTransform();
            } else { // outputCoordinateSystem == "global"
                var transform = this._getLocalTransform();
                return this.drawPane._getGlobalTransform().duplicate().rightMultiply(transform);
            }
        } else if (inputCoordinateSystem == "drawing") {
            if (outputCoordinateSystem == "local") {
                return this._getInverseLocalTransform();
            } else { // outputCoordinateSystem == "global"
                return this.drawPane._getGlobalTransform();
            }
        } else { // inputCoordinateSystem == "global"
            if (outputCoordinateSystem == "local") {
                var transform = this.drawPane._getGlobalTransform().duplicate().rightMultiply(this._getLocalTransform());
                return transform.getInverse();
            } else { // outputCoordinateSystem == "drawing"
                return this.drawPane._getInverseGlobalTransform();
            }
        }
    }
},

//> @method drawItem.isPointInPath()
// Returns true if the given point in the drawing coordinate system is within this DrawItem's shape,
// taking into account local transforms.
// @param x (int) X coordinate of the test point.
// @param y (int) Y coordinate of the test point.
// @return (boolean)
// @visibility drawing
//<
isPointInPath : function (x, y, pageX, pageY) {
    var hitTolerance = this.hitTolerance,
        lineWidth = (
            (this.lineWidth == null ? 1 : Math.max(1, this.lineWidth)) +
            2 * hitTolerance);

    // Temporarily add half of the lineWidth to the hitTolerance when determining whether the
    // point in in bounds of the DrawItem.
    this.hitTolerance = lineWidth / 2; // note: `lineWidth / 2' includes the hitTolerance
    var isInBounds = this.isInBounds(x, y);
    this.hitTolerance = hitTolerance;
    if (!isInBounds) {
        return false;
    }

    if (this.drawingBitmap) {
        var context = this.drawPane.getBitmapContext(),
            isDrawCurve = isc.isA.DrawCurve(this),
            origLinePattern = this.linePattern;
        context.save();
        this.linePattern = "solid";
        try {
            context.beginPath();
            if (isDrawCurve) {
                // The second argument is set to false to avoid filling or stroking the start
                // and end arrows.
                this.drawBitmapPath(context, false);
            } else {
                this.drawBitmapPath(context);
            }
            var normalized = this._normalize(x, y);
            // For compatibility with "svg" and "vml" drawing modes, don't consider a point
            // in the path (the fill area) to be part of this draw item unless this item is
            // actually filled or the browser does not support the native isPointInStroke()
            // method (because it we didn't use the native isPointInPath() method in the case
            // of an unfilled shape and the browser not supporting isPointInStroke(), then the
            // item would be effectively invisible for the purposes of event handling).
            
            if ((this._hasFill() || !isc.Browser._supportsCanvasIsPointInStroke) &&
                context.isPointInPath(normalized[0], normalized[1]))
            {
                return true;
            }
            if (isDrawCurve && this._isPointInPathOfStartOrEndArrow(
                    context, lineWidth, normalized[0], normalized[1]))
            {
                return true;
            } else if (isc.Browser._supportsCanvasIsPointInStroke) {
                context.lineWidth = lineWidth;
                context.lineCap = this.lineCap;
                context.lineJoin = "round";
                context.strokeStyle = "#ff00ff";
                
                return context.isPointInStroke(normalized[0], normalized[1]);
            } else if (isDrawCurve) {
                return this._isPointInStroke(context, normalized[0], normalized[1]);
            } else {
                return false;
            }
        } finally {
            this.linePattern = origLinePattern;
            context.restore();
        }

    } else if (this.drawingVML) {
        var vmlHandle = this._getVMLHandle();
        return vmlHandle.ownerDocument.elementFromPoint(pageX, pageY) === vmlHandle;

    } else if (this.drawingSVG && isc.Browser._supportsSVGGetIntersectionList) {
        var svgBody = this.drawPane._getSvgBody();
        

        
        var ctm = svgBody.getScreenCTM();
        var svgPoint = svgBody.createSVGPoint();
        svgPoint.x = pageX - lineWidth / 2;
        svgPoint.y = pageY - lineWidth / 2;
        svgPoint = svgPoint.matrixTransform(ctm.inverse());

        var svgRect = svgBody.createSVGRect();
        svgRect.x = svgPoint.x;
        svgRect.y = svgPoint.y;
        svgRect.width = lineWidth;
        svgRect.height = lineWidth;
        var intersectionList = svgBody.getIntersectionList(svgRect, null);
        return Array.prototype.indexOf.call(intersectionList, this._svgHandle) >= 0;
    }

    // If the point is within the bounding box, but there isn't a way to determine whether the
    // point is in the path, return false here because it isn't a good user experience to return
    // true. Returning true can make large areas of whitespace qualify as part of a draw item.
    return false;
},

_isPointInPathOfStartOrEndArrow : function (context, lineWidth, x, y) {
    if (this.startArrow == "open" || this.startArrow == "block") {
        context.save();
        this._drawBitmapStartArrow(context, lineWidth, false);
        var success = (
                context.isPointInPath(x, y) ||
                (isc.Browser._supportsCanvasIsPointInStroke && context.isPointInStroke(x, y)));
        context.restore();
        if (success) {
            return true;
        }
    }
    if (this.endArrow == "open" || this.endArrow == "block") {
        context.save();
        this._drawBitmapEndArrow(context, lineWidth, false);
        var success = (
                context.isPointInPath(x, y) ||
                (isc.Browser._supportsCanvasIsPointInStroke && context.isPointInStroke(x, y)));
        context.restore();
        if (success) {
            return true;
        }
    }
    return false;
},

//> @method drawItem.computeAngle()
// Computes the angle in degrees from the positive X axis to the difference vector
// <nobr><b>v</b><sub>2</sub> - <b>v</b><sub>1</sub></nobr> between the two given vectors.
// @param px1 (double) X coordinate of <b>v</b><sub>1</sub>
// @param py1 (double) Y coordinate of <b>v</b><sub>1</sub>
// @param px2 (double) X coordinate of <b>v</b><sub>2</sub>
// @param py2 (double) Y coordinate of <b>v</b><sub>2</sub>
// @return (double) the angle in degrees, in the range [0, 360).
// @visibility drawing
//<
computeAngle : isc.DrawItem.computeAngle,

// DrawItem events
getHoverTarget : function (event, eventInfo) {
    return (this.drawPane ? this.drawPane.getHoverTarget(event, eventInfo) : null);
},

// Called by EventHandler and required to allow DrawItem event functionality
// since DrawItem does not extend Canvas
_allowNativeTextSelection : function (event) {
    return false;
},
_updateCursor : function () {
    if (this.drawPane) this.drawPane._updateCursor(this);
},

dragStartDistance: 5,
prepareForDragging : function () {
    var EH = this.ns.EH;
    if (!EH.dragTarget && !EH.rightButtonDown()) {
        if (this.canDrag) {
            EH.dragTarget = this;
            EH.dragOperation = "drag";
            return true;
        }
    }
},

// Called by EventHandler and required to allow DrawItem event functionality
// since DrawItem does not extend Canvas
focus : function (reason) {
    
    if (this.drawPane != null) this.drawPane.focus(reason);
},
// Called by EventHandler and required to allow DrawItem event functionality
// since DrawItem does not extend Canvas
visibleAtPoint : function (x,y) {
    return true;
},
// Called by EventHandler.doHandleMouseDown() and required to allow DrawItem event
// functionality since DrawItem does not extend Canvas.
_getUseNativeDrag : function () {
    return false;
},

// Called by EventHandler and required to allow DrawItem event functionality
// since DrawItem does not extend Canvas

getDragAppearance : function (dragOperation) {
    return "none";
},
// Called by EventHandler and required to allow DrawItem event functionality
// since DrawItem does not extend Canvas
moveToEvent: function (x,y) {
    this.moveTo(x,y);
},

//> @attr drawItem.dragStartDistance        (number : 5 : IRWA)
// @include canvas.dragStartDistance
//<
dragStartDistance: 1,

getRect : function () {
    var bbox = this.getBoundingBox(false, this._tempBoundingBox);
    return [bbox[0],bbox[1],bbox[2]-bbox[0],bbox[3]-bbox[1]];
},

getPageOffsets : function () {
    var bboxPrime = this._getTransformedBoundingBox(true, false, false, this._tempBoundingBox);

    var offsets = {
        left: bboxPrime[0] << 0,
        top: bboxPrime[1] << 0
    };

    if (this.drawPane != null) {
        var dpOffsets = this.drawPane.getPageOffsets(),
            dpMargins = this.drawPane._calculateMargins();
        offsets.left += dpOffsets.left + dpMargins.left;
        offsets.top += dpOffsets.top + dpMargins.top;
    }

    return offsets;
},

//> @method drawItem.getPageLeft() (A)
// @include canvas.getPageLeft()
//<
getPageLeft : function () {
    return this.getPageOffsets().left;
},

//> @method drawItem.getPageTop() (A)
// @include canvas.getPageTop()
//<
getPageTop : function () {
    return this.getPageOffsets().top;
},

getVisibleDimensions : function () {
    var bboxPrime = this._getTransformedBoundingBox(true, false, false, this._tempBoundingBox);
    return {
        width: Math.ceil(bboxPrime[2] - bboxPrime[0]),
        height: Math.ceil(bboxPrime[3] - bboxPrime[1])
    };
},

getVisibleWidth : function () {
    return this.getVisibleDimensions().width;
},

getVisibleHeight: function () {
    return this.getVisibleDimensions().height;
},

//> @attr drawItem.canDrag (Boolean : false : IRWA)
// Is this DrawItem draggable? If true, then the DrawItem can be drag-repositioned by the user.
// @visibility drawing
//<

//> @method drawItem.setCanDrag()
// Setter for +link{DrawItem.canDrag,canDrag}.
//
// @param canDrag (Boolean) new value for <code>this.canDrag</code>.
// @visibility drawing
//<
setCanDrag : function (canDrag) {
    this.canDrag = canDrag;
},

//> @method drawItem.dragStart()
// Notification fired when the user starts to drag this DrawItem. Will only fire if +link{canDrag,canDrag}
// is true.
// <p>
// Note that if this item is part of a +link{DrawGroup}, then the group's +link{DrawGroup.useGroupRect,useGroupRect}
// setting affects whether this item receives the notification. If useGroupRect is true, then
// this item will <em>not</em> receive the notification. Otherwise, the item receives the
// notification and notification bubbles up to the group.
// @include canvas.dragStart()
// @visibility drawing
//<
dragStart : function (event, info) {
    
    if (this.logIsInfoEnabled("drawEvents")) {
        this.logInfo("DragStart on item:" + this.getID(), "drawEvents");
    }

    // Convert page coordinates to coordinates in the global coordinate system.
    var drawPane = this.drawPane;
    if (drawPane == null) return false;
    var globalPoint = drawPane._pageToGlobal(event.x, event.y),
        x = globalPoint[0],
        y = globalPoint[1];

    // For SGWT we want a separate event which can be easily customized without the
    // need for a call to super.
    if (this.onDragStart(x, y) == false) {
        return false;
    }

    if (this.keepInParentRect) {
        var mouseDownEvent = isc.EH.mouseDownEvent || event;
        globalPoint = drawPane._pageToGlobal(mouseDownEvent.x, mouseDownEvent.y);
        var mouseDownX = globalPoint[0],
            mouseDownY = globalPoint[1],
            bounds = this._getTransformedBoundingBox(true, false, false, this._tempBoundingBox),
            offsetX = bounds[0],
            offsetY = bounds[1];
        this.dragOffsetX = Math.max(0, Math.min(mouseDownX - offsetX, bounds[2] - offsetX));
        this.dragOffsetY = Math.max(0, Math.min(mouseDownY - offsetY, bounds[3] - offsetY));
    }

    this.accumulatedDragX = 0;
    this.accumulatedDragY = 0;

    this.bringToFront();

    return isc.EH.STOP_BUBBLING;
},

//> @method drawItem.onDragStart()
// If +link{DrawItem.canDrag} is true, this notification method will be fired when the user
// starts to drag the draw item.
// @param x (int) x-coordinate within the drawPane
// @param y (int) y-coordinate within the drawPane
// @return (boolean) return false to cancel the default behavior of allowing the shape to be drag-moved
// @visibility sgwt
//<
onDragStart : function (x, y) {
    
},

//> @method drawItem.dragMove()
// Notification fired for every mouseMove event triggered while the user is dragging this
// DrawItem.  Will only fire if +link{canDrag,canDrag} is true.
// <p>
// Note that if this item is part of a +link{DrawGroup}, then the group's +link{DrawGroup.useGroupRect,useGroupRect}
// setting affects whether this item receives the notification. If useGroupRect is true, then
// this item will <em>not</em> receive the notification. Otherwise, the item receives the
// notification and notification bubbles up to the group.
// @include canvas.dragMove()
// @visibility drawing
//<
dragMove : function (event, info) {
    // Convert page coordinates to coordinates in the global coordinate system.
    var drawPane = this.drawPane;
    if (drawPane == null) return false;
    var globalPoint = drawPane._pageToGlobal(event.x, event.y),
        x = globalPoint[0],
        y = globalPoint[1];

    if (this.keepInParentRect) {
        var box = this._getParentRect(),
            boundingBox = this._getTransformedBoundingBox(true, false, false, this._tempBoundingBox),
            width = boundingBox[2] - boundingBox[0],
            height = boundingBox[3] - boundingBox[1];

        x = Math.max(box[0] + this.dragOffsetX, Math.min(box[2] - (width - this.dragOffsetX), x));
        y = Math.max(box[1] + this.dragOffsetY, Math.min(box[3] - (height - this.dragOffsetY), y));

        x = isc.DrawItem._makeCoordinate(x);
        y = isc.DrawItem._makeCoordinate(y);
    }

    if (this.onDragMove(x, y) == false) return false;

    var mouseDownEvent = isc.EH.mouseDownEvent || event;
    globalPoint = drawPane._pageToGlobal(mouseDownEvent.x, mouseDownEvent.y);
    var mouseDownX = globalPoint[0],
        mouseDownY = globalPoint[1];

    var expectedDragX = x - mouseDownX,
        expectedDragY = y - mouseDownY,
        dx = expectedDragX - this.accumulatedDragX,
        dy = expectedDragY - this.accumulatedDragY;

    this.moveBy(dx, dy);
    this.accumulatedDragX += dx;
    this.accumulatedDragY += dy;

    return isc.EH.STOP_BUBBLING;
},

//> @method drawItem.onDragMove()
// If +link{DrawItem.canDrag} is true, this notification method will be fired when the user
// drags the draw item.
// @param x (int) x-coordinate within the drawPane
// @param y (int) y-coordinate within the drawPane
// @return (boolean) return false to cancel the default behavior of allowing the shape to be drag-moved
// @visibility sgwt
//<
onDragMove : function (x, y) {
    
},

//> @method drawItem.dragStop()
// Notification fired when the user stops dragging this DrawItem. Will only fire if +link{canDrag,canDrag}
// is true.
// <p>
// Note that if this item is part of a +link{DrawGroup}, then the group's +link{DrawGroup.useGroupRect,useGroupRect}
// setting affects whether this item receives the notification. If useGroupRect is true, then
// this item will <em>not</em> receive the notification. Otherwise, the item receives the
// notification and notification bubbles up to the group.
// @include canvas.dragStop()
// @visibility drawing
//<
dragStop : function (event, info) {
    // Convert page coordinates to coordinates in the global coordinate system.
    var drawPane = this.drawPane;
    if (drawPane == null) return false;
    var globalPoint = drawPane._pageToGlobal(event.x, event.y),
        x = globalPoint[0],
        y = globalPoint[1];

    // notification for SGWT
    this.onDragStop(x, y);
    // no need to move to x,y - this will have happened in the dragMove event(s).

    return isc.EH.STOP_BUBBLING;
},

//> @method drawItem.onDragStop()
// If +link{DrawItem.canDrag} is true, this notification method will be fired when the user
// completes a drag on the draw item.
// @param x (int) x-coordinate within the drawPane
// @param y (int) y-coordinate within the drawPane
// @visibility sgwt
//<
onDragStop : function (x, y) {
    
},

dropMove : function () {
},

//> @method drawItem.mouseDown()
// Notification fired when the user presses the left mouse button on this DrawItem.
// <p>
// Note that if this item is part of a +link{DrawGroup}, then the group's +link{DrawGroup.useGroupRect,useGroupRect}
// setting affects whether this item receives the notification. If useGroupRect is true, then
// this item will <em>not</em> receive the notification. Otherwise, the item receives the
// notification and notification bubbles up to the group.
// @include canvas.mouseDown()
// @visibility drawing
//<
mouseDown : function () {
    
    return true;
},

//> @method drawItem.mouseUp()
// Notification fired when the user releases the left mouse button on this DrawItem.
// <p>
// Note that if this item is part of a +link{DrawGroup}, then the group's +link{DrawGroup.useGroupRect,useGroupRect}
// setting affects whether this item receives the notification. If useGroupRect is true, then
// this item will <em>not</em> receive the notification. Otherwise, the item receives the
// notification and notification bubbles up to the group.
// @include canvas.mouseUp()
// @visibility drawing
//<
mouseUp : function () {
    
    return true;
},

//> @method drawItem.click()
// Notification fired when the user clicks on this DrawItem.
// <p>
// Note that if this item is part of a +link{DrawGroup}, then the group's +link{DrawGroup.useGroupRect,useGroupRect}
// setting affects whether this item receives the notification. If useGroupRect is true, then
// this item will <em>not</em> receive the notification. Otherwise, the item receives the
// notification and notification bubbles up to the group.
// @include canvas.click()
// @visibility drawing
//<
click : function() {
    
    return true;
},

//> @method drawItem.mouseOver()
// Notification fired when the mouse enters this DrawItem.
// <p>
// Note that if this item is part of a +link{DrawGroup}, then the group's +link{DrawGroup.useGroupRect,useGroupRect}
// setting affects whether this item receives the notification. If useGroupRect is true, then
// this item will <em>not</em> receive the notification. Otherwise, the item receives the
// notification and notification bubbles up to the group.
// @include canvas.mouseOver()
// @visibility drawing
//<
mouseOver : function () {
    
    return true;
},

//> @method drawItem.mouseMove()
// Notification fired when the user moves the mouse over this DrawItem.
// <p>
// Note that if this item is part of a +link{DrawGroup}, then the group's +link{DrawGroup.useGroupRect,useGroupRect}
// setting affects whether this item receives the notification. If useGroupRect is true, then
// this item will <em>not</em> receive the notification. Otherwise, the item receives the
// notification and notification bubbles up to the group.
// @include canvas.mouseMove()
// @visibility drawing
//<
mouseMove : function () {
     
    return true;
},

//> @method drawItem.mouseOut()
// Notification fired when the mouse leaves this DrawItem.
// <p>
// Note that if this item is part of a +link{DrawGroup}, then the group's +link{DrawGroup.useGroupRect,useGroupRect}
// setting affects whether this item receives the notification. If useGroupRect is true, then
// this item will <em>not</em> receive the notification. Otherwise, the item receives the
// notification and notification bubbles up to the group.
// @include canvas.mouseOut()
// @visibility drawing
//<
mouseOut : function () {
    
    return true;
},

//> @attr drawItem.contextMenu (Menu : null : IRW)
// @include canvas.contextMenu
//<

//> @method drawItem.showContextMenu() (A)
// @include canvas.showContextMenu()
//<
showContextMenu : function () {
    
    
    // Support context menus on DrawItems as we do on Canvii    
    var menu = this.contextMenu;
    if (menu) {
        menu.target = this;
        if (!isc.isA.Canvas(menu)) {            
            menu.autoDraw = false;
            this.contextMenu = menu = this.getMenuConstructor().create(menu);            
        }
        menu.showContextMenu();
    }
    return (menu == null);
},

// Support for DrawItem.menuConstructor

getMenuConstructor : function () { 
    var menuClass = isc.ClassFactory.getClass(this.menuConstructor); 
    if (!menuClass) {
        isc.logWarn("Class not found for menuConstructor:" + this.menuConstructor + 
            ". Defaulting to isc.Menu class");    
        menuClass = isc.ClassFactory.getClass("Menu", true);
    } 
    return menuClass;
},

startHover : isc.Canvas.getInstanceProperty("startHover"),

stopHover : isc.Canvas.getInstanceProperty("stopHover"),

_handleHover : function () {
    var EH = isc.EH,
        lastMoveTarget = EH.lastMoveTarget,
        lastEvent = EH.lastEvent;
    if (!lastMoveTarget || lastMoveTarget.getHoverTarget(lastEvent) != this ||
        this.drawPane == null ||
        this.drawPane.getDrawItem(lastEvent.x, lastEvent.y, false, lastEvent) != this)
    {
        return;
    }
    this.handleHover();
},

_getHoverProperties : function () {
    return this.drawPane._getHoverProperties();
},

handleHover : function () {
    if (this.hover && this.hover() == false) return;
    if (this.showHover) {
        var hoverHTML = this.getHoverHTML();
        if (hoverHTML != null && !isc.isAn.emptyString(hoverHTML)) {
            isc.Hover.show(hoverHTML, this._getHoverProperties(), null, this);
        }
    }
},

//> @method drawItem.hover()
// If +link{canHover,canHover} is true for this DrawItem, the hover() string method will
// be fired when the user hovers over this DrawItem. If this method returns false, it will
// suppress the default behavior of showing a hover canvas if +link{showHover,showHover}
// is true.
// @return (boolean) false to cancel the hover event.
// @group hovers
// @visibility drawing
//<

//> @method drawItem.getHoverHTML()
// If +link{showHover,showHover} is true, when the user holds the mouse over this DrawItem for
// long enough to trigger a hover event, a hover canvas is shown by default. This method returns
// the contents of that hover canvas. Default implementation returns +link{prompt,prompt} -
// override for custom hover HTML. Note that returning <code>null</code> or an empty string will
// suppress the hover canvas altogether.
// @return (HTMLString) the HTML to show in the hover
// @group hovers
// @visibility drawing
//<
getHoverHTML : function () {
    return this.prompt;
},

_hoverHidden : isc.Class.NO_OP,

// end of shape events

// drawItem.init()
init : function () {
    this.Super("init", arguments);
    if (this.ID !== false && (this.ID == null || window[this.ID] != this)) {
        isc.ClassFactory.addGlobalID(this); 
    }
    this.drawItemID = isc.DrawItem._IDCounter++;

    
    if (this.shapeData) {
        var shapeData = this.shapeData,
            elements = [shapeData.m00, shapeData.m01, shapeData.m02,
                        shapeData.m10, shapeData.m11, shapeData.m12].map(parseFloat);
        this.setTransform(isc.AffineTransform.create.apply(isc.AffineTransform, elements));
    }

    
    if (this.exemptFromGlobalTransform) {
        this.excludeFromQuadTree = true;
    }

    // Sets the local transform on the DrawItem.
    if (isc.isAn.AffineTransform(this.transform)) {
        var center0 = this._getRotationCenter();
        this._updateLocalTransform(this.transform, center0.cx, center0.cy, null, false, true);
    }

    this._checkProportionalResizing();

    var drawGroup = this.drawGroup,
        drawPane;
    if (drawGroup != null) {
        drawGroup.addDrawItem(this);
    } else if ((drawPane = this.drawPane) != null) {
        drawPane.addDrawItem(this, this.autoDraw, true);
    }
},

// Drawing
// ---------------------------------------------------------------------------------------

//> @method drawItem.draw()
// Draws this item into its current +link{DrawItem.drawPane,drawPane}.
// <p>
// NOTE: For performance reasons, the <code>DrawPane</code> may draw this item on a
// delay to allow multiple items to be added and drawn at one time. The +link{DrawPane.refreshNow()}
// API will force this item to be drawn immediately.
// @visibility drawing
//<
// TODO exception handling for invalid generated VML/SVG markup
draw : function () {

    // be sure to set _drawn before returning
    if (this._drawn) {
        this.logDebug("DrawItem already drawn - exiting draw(). Note that this DrawItem may have been auto-drawn " +
                      "if setDrawPane() was called on it, or if DrawPane.addDrawItem() was called to add this DrawItem to a DrawPane.",
                      "drawing");
        return;
    }
    if (this.drawGroup != null) {
        this.drawPane = this.drawGroup.drawPane;
    } else { // drawing directly into a drawPane
        if (!this.drawPane) this.drawPane = isc.DrawPane.getDefaultDrawPane(this.drawingType);
    }
    var dp = this.drawPane;

    if (this.exemptFromGlobalTransform && !dp._exemptedDrawItems.contains(this)) {
        dp._exemptedDrawItems.push(this);
    }
    if (isc.isA.DrawGroup(this) && this.drawItems != null) {
        var drawItems = this.drawItems,
            warnedAboutEmptyEntries = false;
        for (var i = 0, numDrawItems = drawItems.length; i < numDrawItems; ++i) {
            var drawItem = drawItems[i];
            if (drawItem == null) {
                if (!warnedAboutEmptyEntries) {
                    this.logInfo(
                        "drawGroup included empty entry in drawItems:" + this.echo(drawItems));
                    warnedAboutEmptyEntries = true;
                }
            } else {
                if (drawItem.exemptFromGlobalTransform) {
                    var oldDrawPane = drawItem._getDrawPane();
                    if (oldDrawPane != dp) {
                        if (oldDrawPane != null) {
                            
                            oldDrawPane._exemptedDrawItems.remove(drawItem);
                        }
                        
                        dp._exemptedDrawItems.push(drawItem);
                    }
                }
                drawItem.drawGroup = this;
                drawItem.drawPane = dp;
            }
        }
        if (warnedAboutEmptyEntries) {
            drawItems.removeEmpty();
        }
    }

    // add to pane.drawItems - this is essentially the set of drawItems that should draw/clear with
    // the pane.
    if (this.drawGroup) {
        this.getZIndex(true);
        this._addOrder = this.drawGroup._addOrder;

        if (!this.drawGroup.drawItems.contains(this)) {
            if (this.drawGroup.drawItems == null) {
                this.drawGroup.drawItems = [this];
            } else {
                var i = isc.Array._binarySearch(this.drawGroup.drawItems, this, isc.DrawItem._zIndexCompareFunc);
                if (i < 0) i = -i - 1;
                for (; i < this.drawGroup.drawItems.length && isc.DrawItem._zIndexCompareFunc(this.drawGroup.drawItems[i], this) == 0; ++i) {
                    //empty
                }
                this.drawGroup.drawItems.addAt(this, i);
            }
            this._updateTitleLabelAndBackground();
            this._drawExemptFromGlobalTransformAutoChildren();
        }

        // If this item is within a group, return early - the item will be drawn with that group.
        return;

    } else {
        this.getZIndex(true);
        if (this._addOrder == null) {
            this._addOrder = dp._addCounter++;
        }

        if (!dp.drawItems.contains(this)) {
            var i = isc.Array._binarySearch(dp.drawItems, this, isc.DrawItem._zIndexCompareFunc);
            if (i < 0) i = -i - 1;
            for (; i < dp.drawItems.length && isc.DrawItem._zIndexCompareFunc(dp.drawItems[i], this) == 0; ++i) {
                //empty
            }
            dp.drawItems.addAt(this, i);

            this._updateTitleLabelAndBackground();
            this._drawExemptFromGlobalTransformAutoChildren();
        }
    }

    //this.logWarn("DP:" + dp + ", dg:" + this.drawGroup);

    // shouldDeferDrawing: for VML/SVG graphics There's a window of time between draw
    // completeing before the iframe is loaded.  If draw() is called in this window, defer the
    // draw (shouldDeferDrawing handles this if it returns true).
    if (dp.shouldDeferDrawing(this)) {
        return;
    }

    // If the DP is undrawn, we just wait for it to draw us as it draws.  The handle_drawn
    // states indicates we're being drawn as part of DrawPane's drawChildren.
    if (!dp.isDrawn() && dp.getDrawnState() != isc.Canvas.HANDLE_DRAWN) {
        return;
    }

    
    this.drawHandle();
},

// drawItem._completeDraw()
// This is like Canvas._completeHTMLInit().
_completeDraw : function () {
    var drawPane = this.drawPane;
    

    var type = drawPane.drawingType;
    if (type === "vml") {
        this.drawingVML = true;

    } else if (type === "svg") {
        this.drawingSVG = true;
        
        this._svgDocument = drawPane._getSvgDocument();
        this._svgContainer = drawPane._getSvgBox();
        // If this DrawItem was individually drawn, then the svgHandle will have been set to
        // the return value of the _insertAdjacentSVG() call.
        // The svgHandle will not be set in the case where this DrawItem is in a group.
        if (this._svgHandle == null) {
            this._svgHandle = this._getSvgHandle();
        }
        

    } else {
        
        this.drawingBitmap = true;
    }

    this._setupEventParent();

    

    this._drawn = true;
},

_getQuadTreeItem : function () {
    var bboxPrime = this._getTransformedBoundingBox(true, true, true, this._tempBoundingBox);
    
    return {
        x: bboxPrime[0],
        y: bboxPrime[1],
        width: (bboxPrime[2] - bboxPrime[0]),
        height: (bboxPrime[3] - bboxPrime[1]),
        shape: this,
        addOrder: this._addOrder
    };
},

_setupEventParent : function () {
    if (this.eventParent != null) return;

    // setup eventParent - this ensures that EH events bubble properly
    
    this.eventParent = (this.drawGroup || this.drawPane);

    
    var addToQuadTree = (
            this.item == null &&
            !this.excludeFromQuadTree &&
            (!isc.isA.DrawGroup(this) || this.useGroupRect));
    for (var dg = this.drawGroup; addToQuadTree && dg != null; dg = dg.drawGroup) {
        
        if (dg.useGroupRect) {
            addToQuadTree = false;
        }
    }
    if (addToQuadTree) {
        var item = this.item = this._getQuadTreeItem();
        this.drawPane.quadTree.insert(item);
    }
},

_clearEventParent : function () {
    delete this.eventParent;
    if (this.item != null) {
        this.drawPane.quadTree.remove(this.item);
        delete this.item;
    }
},

_drawExemptFromGlobalTransformAutoChildren : function () {
    this._drawKnobs();
},

_drawKnobs : function () {
    
    // show any specified controlKnobs
    var knobs = this.knobs;
    if (knobs &&
        // The DrawPane must be drawn or this call must be within DrawPane.drawChildren() to
        // be able to draw the DrawKnobs.
        (this.drawPane.isDrawn() ||
            this.drawPane.getDrawnState() == isc.Canvas.HANDLE_DRAWN))
    {
        for (var i = 0; i < knobs.length; ++i) {
            var knobType = knobs[i],
                shown = this._showKnobs(knobType);
            if (shown === false) {
                knobs.removeAt(i);
                --i;
            } else if (knobType == "resize") {
                this._checkProportionalResizing();
            }
        }

        this._groupAssociatedDrawItems();
    }
},

// Place the resizeOutline and knobs' knobShapes immediately after the DrawItem in the drawing
// order.
_groupAssociatedDrawItems : function () {
    var parent = this.drawPane || this.drawGroup;
    
    var knobs = this.knobs,
        drawItems = parent.drawItems,
        // Maintain `k0` as the index into drawItems of this DrawItem.  `k0` is
        // decremented whenever a DrawItem earlier in the draw order is moved after this
        // DrawItem.
        k0 = drawItems.lastIndexOf(this),
        // Maintain `k - k0` as the number of resizeOutline or knobShapes associated with this
        // DrawItem that have been verified to immediately follow this DrawItem in the
        // draw order.
        k = k0;
    
    for (var i = 0; i < knobs.length; ++i) {
        var knobType = knobs[i];
        if (knobType == "resize") {
            if (this.resizeOutline != null) {
                var j = drawItems.lastIndexOf(this.resizeOutline);
                
                if (j != -1) {
                    if (j != k + 1) {
                        // Move the resizeOutline to immediately after this DrawItem.
                        isc.Array._moveAfter(drawItems, j, k, 1);
                    }
                    if (j < k) {
                        --k0;
                    } else {
                        ++k;
                    }
                }
            }
            if (this._resizeKnobs != null) {
                for (var l = 0, numKnobs = this._resizeKnobs.length; l < numKnobs; ++l) {
                    var j = drawItems.lastIndexOf(this._resizeKnobs[l].knobShape);
                    
                    if (j != -1) {
                        if (j != k + 1) {
                            isc.Array._moveAfter(drawItems, j, k, 1);
                        }
                        if (j < k) {
                            --k0;
                        } else {
                            ++k;
                        }
                    }
                }
            }
        } else {
            var knob = null;
            if (knobType == "move") {
                knob = this._moveKnob;
            } else if (knobType == "startPoint") {
                knob = this._startKnob;
            } else if (knobType == "endPoint") {
                knob = this._endKnob;
            } else if (knobType == "controlPoint1") {
                knob = this._c1Knob;
            } else if (knobType == "controlPoint2") {
                knob = this._c2Knob;
            }

            if (knob != null) {
                var j = drawItems.lastIndexOf(knob.knobShape);
                
                if (j != -1) {
                    if (j != k + 1) {
                        isc.Array._moveAfter(drawItems, j, k, 1);
                    }
                    if (j < k) {
                        --k0;
                    } else {
                        ++k;
                    }
                }
            }
        }
    }
},

drawHandle : function () {
    

    var dp = this.drawPane,
        type = this.drawingType = dp.drawingType; // drawingType of drawPane wins

    // map string drawingType to faster booleans
    if (type === "vml") {
        this.drawingVML = true;
    } else if (type === "svg") {
        this.drawingSVG = true;
    } else {
        
        this.drawingBitmap = true;
    }

    if (this.drawingVML) {
        dp._batchDraw(this);

    // Because SVG does not support zIndex, we need to draw the SVG handle in the correct place.
    } else if (this.drawingSVG) {
        var svgDocument = dp._getSvgDocument(),
            conversionContext = dp._batchDrawConversionContext,
            origDefSvgStrings = conversionContext.defSvgStrings;

        // We need to be able to distinguish the def svgStrings added for this item's SVG markup
        // representation from def svgStrings added for other items' SVG markup representations.
        // One way of doing this is to create a new object whose prototype is the previous conversion
        // context defSvgStrings object. After getSvgString() is called to generate this item's
        // SVG markup, we iterate through the has-own properties of the defSvgStrings object
        // (these are the new def svgStrings).
        var defSvgStrings = conversionContext.defSvgStrings = Object.create(origDefSvgStrings);

        var svgString = this.getSvgString(conversionContext);

        var defsSvgString = "",
            newDefIDs = [];
        for (var defID in defSvgStrings) {
            if (!defSvgStrings.hasOwnProperty(defID)) continue;
            var existingDef = svgDocument.getElementById(defID);
            if (existingDef != null) {
                existingDef.parentNode.removeChild(existingDef);
            }

            defsSvgString += defSvgStrings[defID];
            newDefIDs.add(defID);
        }
        
        for (var i = 0, numNewDefIDs = newDefIDs.length; i < numNewDefIDs; ++i) {
            var defID = newDefIDs[i];
            origDefSvgStrings[defID] = defSvgStrings[defID];
        }
        if (defsSvgString != "") dp._insertDefsSvgString(defsSvgString);

        conversionContext.defSvgStrings = origDefSvgStrings;


        // Figure out where to insert the svgString and call DrawItem._insertAdjacentSVG().
        var parent = this.drawGroup != null ? this.drawGroup : this.drawPane;
        var siblingItems = parent.drawItems;
        var i = siblingItems.indexOf(this);
        
        // Find the first drawn sibling after this item.
        var siblingHandle = null;
        for (i = i + 1; i < siblingItems.length; ++i) {
            var sibling = siblingItems[i];
            if (sibling.drawingSVG) {
                if (isc.isA.DrawGroup(sibling) && !sibling.renderGroupElement) {
                    
                }
                siblingHandle = sibling._svgHandle;
                break;
            }
        }
        if (siblingHandle != null) {
            this._svgHandle = isc.DrawItem._insertAdjacentSVG(siblingHandle, "beforebegin", svgString, true, conversionContext);

        // If there isn't a drawn sibling after this item, then we need to append the svgString
        // to the parent's SVG handle element.
        } else {
            var parentElem;
            if (isc.isA.DrawGroup(parent)) {
                if (!parent.renderGroupElement) {
                    
                } else {
                    parentElem = parent._svgHandle;
                    
                }
            } else {
                
                parentElem = dp._getSvgBox();
            }

            this._svgHandle = isc.DrawItem._insertAdjacentSVG(parentElem, "beforeend", svgString, true, conversionContext);
        }


        this._completeDraw();

    // bitmap - redraw the drawPane (NB: redraw is deferred, so item will be added to drawItems first)
    } else {
        
        dp.redrawBitmap(); // drawPane will call back to this.drawBitmap()
    }
},

// drawItem.isDrawn()
isDrawn : function () { return !!this._drawn },

// In SVG drawing mode, we have a gradient def element for each DrawItem so that we can use the
// more intuitive userSpaceOnUse gradient units. When this DrawItem is moved, resized, or generally
// reshaped, we need to update the gradient def element.
_maybeUpdateSvgFillGradientDef : function () {
    if (!this.drawingSVG) return;

    var drawPane = this.drawPane,
        batchDrawConversionContext = drawPane._batchDrawConversionContext;

    var gradientDefID = batchDrawConversionContext.useGradientIDs[this.ID];
    if (gradientDefID != null) {
        var existingGradientElem = this._svgDocument.getElementById(gradientDefID);
        

        var gradient = this.fillGradient;
        gradient = (isc.isA.String(gradient)
                    ? gradient = drawPane.getGradient(gradient)
                    : gradient);
        

        if (isc.DrawItem._isSimpleGradient(gradient) ||
            isc.DrawItem._isLinearGradient(gradient))
        {
            var vector = this._normalizeLinearGradient(gradient);
            existingGradientElem.setAttributeNS(null, "x1", vector[0]);
            existingGradientElem.setAttributeNS(null, "y1", vector[1]);
            existingGradientElem.setAttributeNS(null, "x2", vector[2]);
            existingGradientElem.setAttributeNS(null, "y2", vector[3]);

        } else {
            
            var vector = this._normalizeRadialGradient(gradient);
            if (gradient.cx != null) existingGradientElem.setAttributeNS(null, "cx", vector[0]);
            if (gradient.cy != null) existingGradientElem.setAttributeNS(null, "cy", vector[1]);
            if (gradient.fx != null) existingGradientElem.setAttributeNS(null, "fx", vector[3]);
            if (gradient.fy != null) existingGradientElem.setAttributeNS(null, "fy", vector[4]);
            // Note: we need to update the 'r' attribute because the radial gradient config's
            // `r' property could be a percentage string.
            if (gradient.r != null) existingGradientElem.setAttributeNS(null, "r", vector[5]);
        }

        // Delete the SVG string for this item's "use def" because it's not current. The SVG
        // string will be regenerated if needed.
        delete batchDrawConversionContext.defSvgStrings[gradientDefID];
    }
},

_reshaped : function () {
    this._transform = null;
    this.updateControlKnobs();
    this._updateTitleLabelAndBackground();
    this._updateQuadTreeItem();
    this._maybeUpdateSvgFillGradientDef();
},

// resized / moved notifications
// This is an opportunity to update our control knobs
//> @method drawItem.moved()
// Notification method fired when this component is explicitly moved.
// Note that a component's position on the screen may also be changed due to an ancestor being
// moved. The +link{Canvas.parentMoved(),parentMoved()} method provides a notification entry point to catch
// that case as well.
//
// @param deltaX (int) horizontal difference between current and previous position
// @param deltaY (int) vertical difference between current and previous position
// @visibility drawing
//<
moved : function (deltaX, deltaY) {},

_moved : function (deltaX, deltaY) {
    

    // Rotation is about the center of the DrawItem. So, when the DrawItem is moved, we need
    // to clear the cached local transform.
    this._transform = null;

    this.updateControlKnobs();
    this._updateTitleLabelAndBackground();
    this._updateQuadTreeItem();
    this._maybeUpdateSvgFillGradientDef();
    this.saveCoordinates();
    this.moveSelection(deltaX, deltaY);
    this.moved(deltaX, deltaY);
},

saveCoordinates : function () {
    if (!this.editContext) return;

    var component = this.editContext.getEditNodeArray().find("liveObject", this);

    //  can happen if we get a resized or moved notification while a component is being
    //  added or removed
    if (!component) return; 

    var bb = this.getBoundingBox(false, this._tempBoundingBox);
    this.editContext.setNodeProperties(component, {
        left: bb[0],
        top: bb[1],
        width: bb[2] - bb[0],
        height: bb[3] - bb[1]
    }, true);
},



moveSelection : function (deltaX, deltaY) {
    //>EditMode
    if (!this.editingOn || !this.editContext) return;

    // Prevent infinite recursion and mutual-exclusion on selection
    // movement with a flag on the drawPane
    if (!this.drawPane._movingSelection) {
        // if this component is part of a selection, move the rest of the selected
        // components by the same amount
        var selection = this.editContext.getSelectedComponents();
        if (selection.length > 1 && selection.contains(this)) {
            this.drawPane._movingSelection = true;
            for (var i = 0; i < selection.length; i++) {
                if (selection[i] != this) {
                    selection[i].moveBy(deltaX, deltaY);
                }
            }
            this.drawPane._movingSelection = false;
        }
    }
    //<EditMode
},

//> @method drawItem.resized()
// Observable method called whenever a DrawItem changes size.
// @visibility drawing
//<
resized : function () {},

_resized : function () {
    // Rotation is about the center of the DrawItem. So, when the DrawItem is resized, we need
    // to clear the cached local transform.
    this._transform = null;

    this.updateControlKnobs();
    this._updateTitleLabelAndBackground();
    this._updateQuadTreeItem();
    this._maybeUpdateSvgFillGradientDef();
    this.saveCoordinates();
    this.resized();
},


// Knobs
// ---------------------------------------------------------------------------------------

// _showKnobs / _hideKnobs actually create and render the DrawKnobs for each specified knobType
// Implementation assumes the existance of a show<KnobType>Knobs() method for each specified 
// knobType.

_showKnobs : function (knobType) {
    
    var functionName = isc.DrawItem._getShowKnobsFunctionName(knobType) 
    if (!this[functionName]) {
        this.logWarn("DrawItem specfied with knobType:"+ knobType + 
                    " but no " +  functionName + " function exists to show the knobs. Ignoring");
        return false;
    } else {
        this[functionName]();
    }
},

_hideKnobs : function (knobType) {
    
    var functionName = isc.DrawItem._getShowKnobsFunctionName(knobType, true) 
    if (!this[functionName]) {
        this.logWarn("DrawItem specfied with knobType:"+ knobType + 
                    " but no " +  functionName + " function exists to hide the knobs.");
        return false;
    } else {
        this[functionName]();
    }
},

//> @method drawItem.showKnobs()
// Shows a set of control knobs for this drawItem. Updates +link{drawItem.knobs} to include the
// specified knobType, and if necessary draws out the appropriate control knobs.
// @param knobType (KnobType or Array of KnobType) knobs to show
// @visibility drawing
//<
showKnobs : function (knobType) {
    // allow the developer to show multiple knobs with a single function call
    if (isc.isAn.Array(knobType)) {
        for (var i = 0; i < knobType.length; i++) {
            this.showKnobs(knobType[i]);
        }
        return;
    }
    if (!this.knobs) this.knobs = [];
    if (this.knobs.contains(knobType)) return;
    if (!(isc.isA.DrawPane(this.drawPane) && this.drawPane.isDrawn())) {
        this.knobs.add(knobType);
        return;
    }

    // Skip adding `knobType' to the knobs array if _showKnobs() did not show the knob(s) for
    // the knob type.
    if (this._showKnobs(knobType) !== false) {
        var oldMoveKnobOffset = this._getMoveKnobOffset();

        this.knobs.add(knobType);

        if (knobType == "resize") {
            this._checkProportionalResizing();
        }

        if (this.showResizeKnobs && !this.moveKnobOffset && this.knobs.length > 1 &&
            (knobType == "resize" || knobType == "move") && 
            (this._moveKnob != null && !this._moveKnob.destroyed) && this.knobs.contains("resize"))
        {
            var moveKnobOffset = this._getMoveKnobOffset();
            if (!oldMoveKnobOffset) oldMoveKnobOffset = [0,0];
            this._moveKnob.moveBy(moveKnobOffset[0] - oldMoveKnobOffset[0],
                                  moveKnobOffset[1] - oldMoveKnobOffset[1]);
        }
    }
},

//> @method drawItem.showAllKnobs()
// Shows all supported control knobs for this drawItem. Updates +link{drawItem.knobs} to include the
// supported knobTypes and if necessary draws out the appropriate control knobs.
// @visibility drawing
//<
showAllKnobs : function () {
    var knobs = this.getSupportedKnobs();
    this.showKnobs(knobs);
},

//> @method drawItem.hideKnobs()
// Hides a set of control knobs for this drawItem. Updates +link{drawItem.knobs} to remove the
// specified knobType, and clears any drawn knobs for this knobType. 
// @param knobType (KnobType | Array of KnobType) knobs to hide
// @visibility drawing
//<
hideKnobs : function (knobType) {
    if (!this.knobs) return;
    if (!knobType) {
        knobType = this.knobs.duplicate();
    }
    if (isc.isAn.Array(knobType)) {
        for (var i = 0; i < knobType.length; i++) {
            this.hideKnobs(knobType[i]);
        }
        return;
    }
    var oldMoveKnobOffset = this._getMoveKnobOffset();
    if (this.knobs.contains(knobType)) {
        this.knobs.remove(knobType);
        if (knobType == "resize") {
            this._checkProportionalResizing();
        }
    }

    if (isc.isA.DrawPane(this.drawPane) && this.drawPane.isDrawn()) {
        this._hideKnobs(knobType);
        if (this._moveKnob != null && !this._moveKnob.destroyed && !this.moveKnobOffset) {
            if (knobType == "resize") {
                var moveKnobOffset = this._getMoveKnobOffset();
                if (!moveKnobOffset) moveKnobOffset = [0,0];
                this._moveKnob.moveBy(moveKnobOffset[0] - oldMoveKnobOffset[0],
                                      moveKnobOffset[1] - oldMoveKnobOffset[1]);
            }
        }
    }
},

//> @method drawItem.hideAllKnobs()
// Hides all control knobs for this drawItem. Updates +link{drawItem.knobs} to remove
// all knobTypes and clears any drawn knobs. 
// @visibility drawing
//<
hideAllKnobs : function () {
    if (!this.knobs) return;
    var knobTypes = this.knobs.duplicate();
    this.hideKnobs(knobTypes);
},


// -----------------------------------------
// move control knob shared by all drawItems


//> @type MoveKnobPoint
// Specifies the starting point of a move knob with respect to its draw item. The move knob is
// positioned relative to the move knob point at the +link{DrawItem.moveKnobOffset}.
// @value "TL" Top Left corner
// @value "TR" Top Right corner
// @value "BL" Bottom Left corner
// @value "BR" Bottom Right corner
// @visibility drawing
//<


//> @attr drawItem.moveKnobPoint (MoveKnobPoint : "TL" : IR)
// If this item is showing a
// <smartclient>"move"</smartclient>
// <smartgwt>{@link com.smartgwt.client.types.KnobType#MOVE}</smartgwt>
// +link{drawItem.knobs,control knob}, this attribute specifies where the knob should appear
// with respect to the draw item.
// <p>
// The resize and move knobs show at the same position by default. However, when
// both knobs are shown the move knob is offset slightly to allow access to both.
// This position can be adjusted manually with +link{DrawItem.moveKnobOffset}.
// @see DrawItem.moveKnobOffset
// @visibility drawing
//<
moveKnobPoint:"TL",

//> @attr drawItem.moveKnobOffset (Array[] of int : null : IRWA)
// If this item is showing a <code>"move"</code> +link{drawItem.knobs,control knob}, this attribute
// allows you to specify an offset in pixels from the +link{drawItem.moveKnobPoint} for the
// move knob. Offset should be specified as a 2-element array of [left offset, top offset].
// <p>
// This offset overrides the built-in offset used when showing both resize and move knobs.
//
// @see DrawItem.moveKnobPoint
// @visibility drawing
//<

//> @method drawItem.setMoveKnobOffset() (A)
// Setter for +link{moveKnobOffset}.
// @param [newMoveKnobOffset] (Array[] of int) the new move knob offset. This is a 2-element array
// of [left offset, top offset]. If null, then <smartclient><code>[0,0]</code></smartclient>
// <smartgwt><code>new int[] {0, 0}</code></smartgwt> is assumed.
// @example drawKnobs
// @visibility drawing
//<
setMoveKnobOffset : function (newMoveKnobOffset) {
    var oldMoveKnobOffset = this._getMoveKnobOffset();
    this.moveKnobOffset = newMoveKnobOffset;
    if (this._moveKnob != null && !this._moveKnob.destroyed) {
        var moveKnobOffset = this._getMoveKnobOffset();
        if (moveKnobOffset == null) moveKnobOffset = [0, 0];
        if (oldMoveKnobOffset == null) oldMoveKnobOffset = [0, 0];
        this._moveKnob.moveBy(moveKnobOffset[0] - oldMoveKnobOffset[0],
                              moveKnobOffset[1] - oldMoveKnobOffset[1]);
    }
},


_moveKnobPointOffsets: {
    "TL": [-8,10],
    "TR": [8,10],
    "BL": [-8,-10],
    "BR": [8,-10]
    // Remaining points need no auto-offset
},
_getMoveKnobOffset : function () {
    if (this.moveKnobOffset) {
        return this.moveKnobOffset;
    }
    if (this.knobs == null || this.knobs.length == 0 || 
        !(this.knobs.contains("resize") && this.knobs.contains("move")))
    {
        return null;
    }
    return this._moveKnobPointOffsets[this.moveKnobPoint];
},

//> @attr drawItem.moveKnob (AutoChild DrawKnob : null : IR)
// If this item is showing <smartclient>"move"</smartclient>
// <smartgwt>{@link com.smartgwt.client.types.KnobType#MOVE}</smartgwt>
// +link{drawItem.knobs,control knobs}, this attribute specifies the AutoChild for the
// +link{DrawKnob} that allows a user to move the DrawItem with help of a knob located at
// +link{drawItem.moveKnobPoint}. Default move knob shape is green circle.
//
// @visibility drawing
//<
moveKnobDefaults: {
    cursor: "move",
    knobShapeProperties: {
        _constructor: "DrawOval",
        radius: 4.5,
        lineWidth: 1,
        lineOpacity: 1,
        fillOpacity: 1,
        lineColor: "#333333",
        fillGradient: {
            id: "isc_defaultMoveKnobGradient",
            x1: "0%", y1: "0%",
            x2: "0%", y2: "100%",
            colorStops: [
                {color: "#ffffff", offset: 0.15},
                {color: "#b8ecae", offset: 0.5},
                {color: "#ffffff", offset: 0.85}
            ]
        }
    }
},

moveKnobConstructor: "DrawKnob",

 
getSupportedKnobs : function () {
    var knobTypes = isc.DrawItem._allKnobTypes,
        knobs = []
    ;
    for (var i = 0; i < knobTypes.length; i++) {
        var knobType = knobTypes[i];
        var functionName = isc.DrawItem._getShowKnobsFunctionName(knobType);
        if (this[functionName]) knobs.add(knobType);
    }
    return knobs;
},

// Helper: given a control knob position ("T", "BR", etc) return the x/y coordinates for the knob
_getKnobPosition : function (position, bbox) {
    if (bbox == null) {
        bbox = this.getResizeBoundingBox(this._tempBoundingBox);
    }

    var x, y;
    x = position.contains("L") ? isc.DrawItem._makeCoordinate(bbox[0]) :
            (position.contains("R") ? isc.DrawItem._makeCoordinate(bbox[2]) :
                (isc.DrawItem._makeCoordinate(bbox[0] + (bbox[2] - bbox[0]) / 2)));
    y = position.contains("T") ? isc.DrawItem._makeCoordinate(bbox[1]) :
            (position.contains("B") ? isc.DrawItem._makeCoordinate(bbox[3]) :
                (isc.DrawItem._makeCoordinate(bbox[1] + (bbox[3] - bbox[1]) / 2)));
    return [x, y];
},

// drawItem.showMoveKnobs()
showMoveKnobs : function () {
    if (this._moveKnob != null && !this._moveKnob.destroyed) return;

    // support customization of moveKnob via autoChild mechanism so the user can specify
    // moveKnobDefaults etc. Not currently exposed
    var position = this._getKnobPosition(this.moveKnobPoint);
    if (position == null) return;

    var x = position[0], y = position[1];
    if (window.isNaN(x) || window.isNaN(y)) return;
    

    var moveKnobOffset = this._getMoveKnobOffset();
    if (moveKnobOffset) {
        x += moveKnobOffset[0];
        y += moveKnobOffset[1];
        if (x < 0) x = 0;
        if (y < 0) y = 0;
    }
    this._moveKnob = this.createAutoChild("moveKnob", {
        x: x, y: y,
        drawPane: this.drawPane,

        resetKnobPosition : function () {
            // This is the same calculation as above for the initial `x` and `y`.
            var drawItem = this.creator,
                position = drawItem._getKnobPosition(drawItem.moveKnobPoint);
            if (position != null) {
                var x = position[0], y = position[1];
                if (!(window.isNaN(x) || window.isNaN(y))) {
                    var moveKnobOffset = drawItem._getMoveKnobOffset();
                    if (moveKnobOffset) {
                        x += moveKnobOffset[0];
                        y += moveKnobOffset[1];
                    }
                    this.setCenterPoint(x, y, false);
                }
            }
        },

        updatePoints : function (x, y, dx, dy, state) {
            var drawItem = this.creator,
                startState = (state == "start"),
                moveState = (state == "move"),
                stopState = (state == "stop");

            if (startState) {
                var box = drawItem.getResizeBoundingBox(this._tempBoundingBox),
                    x0 = x - dx,
                    y0 = y - dy;
                drawItem._dragMoveLeftOffset = x0 - box[0];
                drawItem._dragMoveRightOffset = box[2] - x0;
                drawItem._dragMoveTopOffset = y0 - box[1];
                drawItem._dragMoveBottomOffset = box[3] - y0;
            }

            if (drawItem.keepInParentRect) {
                var box = drawItem._getParentRect(),
                    x0 = x - dx,
                    y0 = y - dy;

                x = Math.max(
                    box[0] + drawItem._dragMoveLeftOffset,
                    Math.min(box[2] - drawItem._dragMoveRightOffset, x));
                y = Math.max(
                    box[1] + drawItem._dragMoveTopOffset,
                    Math.min(box[3] - drawItem._dragMoveBottomOffset, y));
                dx = isc.DrawItem._makeCoordinate(x - x0);
                dy = isc.DrawItem._makeCoordinate(y - y0);
            }

            if (stopState) {
                delete drawItem._dragMoveLeftOffset;
                delete drawItem._dragMoveRightOffset;
                delete drawItem._dragMoveTopOffset;
                delete drawItem._dragMoveBottomOffset;
            }

            drawItem.moveBy(dx, dy);
        }
    });
},

// drawItem.hideMoveKnobs()
hideMoveKnobs : function () {
    if (this._moveKnob) {
        this._moveKnob.destroy();
        delete this._moveKnob;
    }
},


// -----------------------------------------
// resize control knob
// Implemented at the drawItem level - not exposed / supported for all drawItems  

//> @type ResizeKnobPoint
// Specifies the position of a resize knob with respect to its draw item.
// @value "TL" Top Left corner
// @value "TR" Top Right corner
// @value "BL" Bottom Left corner
// @value "BR" Bottom Right corner
// @value "T" Centered on the top edge
// @value "B" Centered on the bottom edge
// @value "R" Centered on the left edge
// @value "L" Centered on thie right edge
// @visibility drawing
//<

//> @attr drawItem.resizeKnobPoints (Array of ResizeKnobPoint : ["TL","TR","BL","BR","T","R","B","L"] : IR)
// If this item is showing <smartclient>"resize"</smartclient>
// <smartgwt>{@link com.smartgwt.client.types.KnobType#RESIZE}</smartgwt>
// +link{drawItem.knobs,control knobs}, this attribute
// specifies the points with respect to the draw item where resize knobs should appear.
// @visibility drawing
//<
resizeKnobPoints:["TL","TR","BL","BR","T","R","B","L"],

//> @attr drawItem.cornerResizeKnob (MultiAutoChild DrawKnob : null : IR)
// If this item is showing <smartclient>"resize"</smartclient>
// <smartgwt>{@link com.smartgwt.client.types.KnobType#RESIZE}</smartgwt>
// +link{drawItem.knobs,control knobs}, this attribute specifies the MultiAutoChild for the
// +link{DrawKnob} that allows a user to resize the DrawItem with help of knobs located at
// corners of a bounding rectangle of current DrawItem. The default shape is a light teal
// circle.
//
// @visibility drawing
//<
cornerResizeKnobDefaults: {
    knobShapeProperties: {
        _constructor: "DrawOval",
        radius: 4.5,
        lineWidth: 1,
        lineOpacity: 1,
        fillOpacity: 1,
        lineColor: "#333333",
        fillGradient: {
            id: "isc_defaultCornerResizeKnobGradient",
            x1: "0%", y1: "0%",
            x2: "0%", y2: "100%",
            colorStops: [
                {color: "#ffffff", offset: 0.15},
                {color: "#CAE9ED", offset: 0.5},
                {color: "#ffffff", offset: 0.85}
            ]
        }
    }
},

cornerResizeKnobConstructor: "DrawKnob",

//> @attr drawItem.sideResizeKnob (MultiAutoChild DrawKnob : null : IR)
// If this item is showing <smartclient>"resize"</smartclient>
// <smartgwt>{@link com.smartgwt.client.types.KnobType#RESIZE}</smartgwt>
// +link{drawItem.knobs,control knobs}, this attribute specifies the MultiAutoChild for the
// +link{DrawKnob} that allows a user to resize the DrawItem with help of knobs located at
// centers of edges of a bounding rectangle of current DrawItem. The default shape is a light
// teal square.
//
// @visibility drawing
//<
sideResizeKnobDefaults: {
    knobShapeProperties: {
        _constructor: "DrawRect",
        width:7,
        height: 7,
        lineWidth: 1,
        fillOpacity: 1,
        lineColor: "#333333",
        fillGradient: {
            id: "isc_defaultSideResizeKnobGradient",
            x1: "0%", y1: "0%",
            x2: "0%", y2: "100%",
            colorStops: [
                {color: "#ffffff", offset: 0.2},
                {color: "#CAE9ED", offset: 0.5},
                {color: "#ffffff",   offset: 0.95}
            ] 
        }
    }
},

sideResizeKnobConstructor: "DrawKnob",

//> @attr drawItem.resizeOutline (AutoChild DrawRect : null : R)
// If this item is showing <smartclient>"resize"</smartclient>
// <smartgwt>{@link com.smartgwt.client.types.KnobType#RESIZE}</smartgwt>
// +link{drawItem.knobs,control knobs}, this attribute specifies the AutoChild for the
// +link{DrawRect} that draws a rectangle frame which connects all resize knobs of current
// DrawItem.
// @visibility drawing
//<
resizeOutlineDefaults: {
    lineWidth: 1,
    lineColor: "#007FFF",
    linePattern: "shortdot",
    autoDraw: true,
    _internal:true
},

resizeOutlineConstructor: "DrawRect",

//> @attr drawItem.showResizeOutline (boolean : true : IRW)
// If this item is showing <smartclient>"resize"</smartclient>
// <smartgwt>{@link com.smartgwt.client.types.KnobType#RESIZE}</smartgwt>
// +link{drawItem.knobs,control knobs} will the resize outline be shown or not.
//
// @visibility drawing
// @see resizeOutline
//<
showResizeOutline: true,


getResizeKnobProperties : function (side) {
},

showResizeKnobs : function () {
    var resizeKnobs = this._resizeKnobs;
    if (resizeKnobs != null && !resizeKnobs.isEmpty()) return;
    
    var positions = this.resizeKnobPoints;

    // Precalculate the resize bounding box.
    var bbox = this._tempBoundingBox,
        bbox0 = 0, bbox1 = 0, bbox2 = 0, bbox3 = 0;
    if (this.showResizeOutline || positions.length > 0) {
        bbox = this.getResizeBoundingBox(bbox);
        bbox0 = bbox[0];
        bbox1 = bbox[1];
        bbox2 = bbox[2];
        bbox3 = bbox[3];
    }

    if (this.showResizeOutline) {
        var resizeOutline = this.resizeOutline = this.createAutoChild("resizeOutline", {
            left: bbox[0], 
            top: bbox[1],
            width: bbox[2] - bbox[0],
            height: bbox[3] - bbox[1],
            drawPane: this.drawPane,
            excludeFromQuadTree: true,
            // A resizeOutline cannot have its own knobs.
            knobs: null,
            title: null,
            zIndex: this.getZIndex(true),
            _sameZIndexAsMaster: true,
            // The local transform must be the identity transform.
            rotation: 0,
            xShearFactor: 0,
            yShearFactor: 0,
            scale: null,
            translate: null,

            exemptFromGlobalTransform: true,
            _globalTransformChanged : function () {
                // `resizeOutlineConstructor` is "DrawRect".  However, check that this
                // `resizeOutline` is actually a DrawRect so that the `setRect()` method can be
                // safely called.
                if (isc.isA.DrawRect(this)) {
                    var bbox = this.creator.getResizeBoundingBox(this._tempBoundingBox);
                    this.setRect(isc.DrawItem._makeCoordinate(bbox[0]),
                                 isc.DrawItem._makeCoordinate(bbox[1]),
                                 isc.DrawItem._makeCoordinate(bbox[2] - bbox[0]),
                                 isc.DrawItem._makeCoordinate(bbox[3] - bbox[1]));
                }
            }
        });
        this.addPeer(resizeOutline);
    }

    resizeKnobs = this._resizeKnobs = [];

    for (var i = 0; i < positions.length; i++) {
        var position = positions[i];

        // Reset `_tempBoundingBox` to the resize bounding box.
        bbox[0] = bbox0;
        bbox[1] = bbox1;
        bbox[2] = bbox2;
        bbox[3] = bbox3;

        var coord = this._getKnobPosition(position, bbox);
        if (coord == null) continue;

        var x = coord[0], y = coord[1];
        if (window.isNaN(x) || window.isNaN(y)) continue;

        // support per-side customization via a method
        var props = isc.addProperties({}, this.getResizeKnobProperties(position), {
            _constructor: "DrawKnob",
            point: position,
            x: x, y: y,
            drawPane: this.drawPane,

            resetKnobPosition : function () {
                var drawItem = this.creator,
                    coord = drawItem._getKnobPosition(this.point);
                if (coord != null) {
                    var x = coord[0], y = coord[1];
                    if (!(window.isNaN(x) || window.isNaN(y))) {
                        this.setCenterPoint(x, y, false);
                    }
                }
            },

            // Override rather than observing `updatePoints()`.  This allows us to cancel the
            // drag if we'd shrink below 1x1 px in size.
            updatePoints : function (left, top, dX, dY, state) {
                return this.creator.dragResizeMove(this.point, left, top, dX, dY, state);
            }
        });

        // Also use auto-child mechanism to pick up defaults / properties.
        var name = (position.length == 1 ? "sideResizeKnob": "cornerResizeKnob"),
            knob = this.createAutoChild(name, props);
        resizeKnobs.push(knob);
    }
},


//> @method drawItem.setTitle() (A)
// Setter for the +link{DrawItem.title,title} of this <code>DrawItem</code>.
// @param [newTitle] (String) new <code>title</code>.
// @visibility drawing
//<
setTitle : function (newTitle) {
    this.title = newTitle;
    this._updateTitleLabelAndBackground();
},

// Creates and adds the titleLabel to the drawPane if needed, or removes the titleLabel if it
// should no longer be shown, or re-adds the titleLabel and resets its contents if it should
// be shown again.
_updateTitleLabelAndBackground : function () {
    var title = this.title,
        titleLabel = this.titleLabel,
        showTitleLabelBackground = this.showTitleLabelBackground,
        titleLabelBackground = this.titleLabelBackground,
        drawPane = this.drawPane;

    // If the title is null or this DrawItem is not in a DrawPane, then the titleLabel should
    // be hidden.
    if (title == null || drawPane == null) {
        if (titleLabelBackground != null && !titleLabelBackground.destroyed && titleLabelBackground.drawPane != null) {
            titleLabelBackground.drawPane.removeDrawItem(titleLabelBackground);
        }
        if (titleLabel != null && !titleLabel.destroyed && titleLabel.drawPane != null) {
            titleLabel.drawPane.removeDrawItem(titleLabel);
            titleLabel.setContents(null); // clear the contents to potentially free up some memory
        }

    // Otherwise, the titleLabel should be shown.
    } else {
        // Create/add the titleLabelBackground first so that it is below the titleLabel in
        // stacking order.
        if (showTitleLabelBackground) {
            if (titleLabelBackground == null || titleLabelBackground.destroyed) {
                titleLabelBackground = this.addPeer(this.createAutoChild("titleLabelBackground", {
                    autoDraw: false,
                    eventProxy: this,
                    knobs: null,
                    title: null,
                    rotation: initialRotation
                }), "titleLabelBackground");
            } else if (titleLabelBackground.drawPane !== drawPane) {
                drawPane.addDrawItem(titleLabelBackground);
            }
        } else {
            if (titleLabelBackground != null && !titleLabelBackground.destroyed && titleLabelBackground.drawPane != null) {
                titleLabelBackground.drawPane.removeDrawItem(titleLabelBackground);
            }
        }

        var titleRotationMode = this.titleRotationMode || isc.DrawItem._instancePrototype.titleRotationMode,
            initialRotation = null,
            finalRotation = null,
            isADrawLine = isc.isA.DrawLine(this),
            isADrawLinePath = isc.isA.DrawLinePath(this),
            flipped = false;
        if (titleRotationMode !== "neverRotate") {
            initialRotation = this.rotation;
            if (initialRotation == null) {
                initialRotation = 0;
            }

            if ((isADrawLine || isADrawLinePath) &&
                (titleRotationMode === "withLine" || titleRotationMode === "withLineAlwaysUp"))
            {
                var startLeft,
                    startTop,
                    endLeft,
                    endTop;
                if (isADrawLinePath) {
                    var segmentPoints = this._segmentPoints;
                    
                    var centerSegmentStartPoint = segmentPoints[1],
                        centerSegmentEndPoint = segmentPoints[2];
                    startLeft = centerSegmentStartPoint[0];
                    startTop = centerSegmentStartPoint[1];
                    endLeft = centerSegmentEndPoint[0];
                    endTop = centerSegmentEndPoint[1];
                } else {
                    
                    startLeft = this.startLeft;
                    startTop = this.startTop;
                    endLeft = this.endLeft;
                    endTop = this.endTop;
                }

                // finalRotation is the angle that the titleLabel should be rotated (in degrees) such
                // that it matches the line direction.
                //
                // To get the direction of the line, use atan2().
                // In drawing coordinates, the positive Y direction points downward, whereas atan2()
                // works in a coordinate system where the positive Y direction points upward.
                // Also, atan2() returns the angle where positive is counter-clockwise (drawing
                // treats the clockwise direction as positive).
                finalRotation = -Math.atan2(-(endTop - startTop), endLeft - startLeft) / this._radPerDeg + initialRotation;
                finalRotation = finalRotation % 360;

            } else {
                finalRotation = initialRotation;
            }

            if (finalRotation < -180) finalRotation += 360;
            else if (finalRotation > 180) finalRotation -= 360;
            

            if (titleRotationMode === "withItemAlwaysUp" || titleRotationMode === "withLineAlwaysUp") {
                if (finalRotation > 90) {
                    finalRotation -= 180;
                    flipped = true;
                } else if (finalRotation < -90) {
                    finalRotation += 180;
                    flipped = true;
                }
                
            }
        }

        var titleLabelPadding = this.titleLabelPadding,
            twiceTitleLabelPadding = 2 * titleLabelPadding;

        // If the titleLabel hasn't been created yet or was destroyed, (re-)create it now.
        if (titleLabel == null || titleLabel.destroyed) {
            if (!isc.isA.DrawPane(this.drawPane)) {
                return;
            }

            var titleLabelDynamicProps = {
                drawPane: drawPane,
                autoDraw: false,
                eventProxy: this,
                contents: title,
                knobs: null,
                title: null,
                left: null,
                top: null,
                rotation: finalRotation || initialRotation
            };

            var pi = this._calculateTitleLabelPositionInfo(title,
                                                           isc.addProperties({},
                                                                             this.titleLabelDefaults,
                                                                             this.titleLabelProperties,
                                                                             titleLabelDynamicProps),
                                                           drawPane);
            
            titleLabelDynamicProps.left = pi.topLeftPoint[0] + (this.drawingBitmap ? 1 : 0);
            titleLabelDynamicProps.top = pi.topLeftPoint[1];
            titleLabel = this.addPeer(this.createAutoChild("titleLabel", titleLabelDynamicProps), "titleLabel");

            if (showTitleLabelBackground) {
                

                var dims = pi.dims;
                titleLabelBackground.setRect(isc.DrawItem._makeCoordinate(pi.topLeftPoint[0]) - titleLabelPadding,
                                             isc.DrawItem._makeCoordinate(pi.topLeftPoint[1]) - titleLabelPadding,
                                             dims.width + twiceTitleLabelPadding,
                                             dims.height + twiceTitleLabelPadding);
                if (finalRotation != null) {
                    titleLabelBackground.rotateTo(finalRotation);
                }
            }

        // Otherwise, the titleLabel has been created.
        } else {
            // If the titleLabel is in a different DrawPane, then move it to this DrawItem's
            // DrawPane.
            if (titleLabel.drawPane !== drawPane) {
                drawPane.addDrawItem(titleLabel);

            // If the titleLabel is stacked below the titleLabelBackground, then re-add the titleLabel.
            } else if (showTitleLabelBackground && titleLabel._addOrder < titleLabelBackground._addOrder) {
                drawPane.addDrawItem(titleLabel);
            }

            // Update the position of the titleLabel and titleLabelBackground.
            var pi = this._calculateTitleLabelPositionInfo(title, titleLabel, drawPane),
                p;
            if (initialRotation != null) {
                titleLabel.rotateTo(initialRotation);
                p = titleLabel._normalize(pi.topLeftPoint[0], pi.topLeftPoint[1], "drawing", "local");
            } else {
                p = pi.topLeftPoint;
            }
            
            titleLabel._moveTo(isc.DrawItem._makeCoordinate(p[0]) + (this.drawingBitmap ? 1 : 0),
                               isc.DrawItem._makeCoordinate(p[1]));
            titleLabel.setContents(title, true);
            if (finalRotation != null) {
                titleLabel.rotateTo(finalRotation);
            }
            if (this.showTitleLabelBackground) {
                var titleLabelBackground = this.titleLabelBackground,
                    dims = pi.dims;
                
                if (initialRotation != null) {
                    titleLabelBackground.rotateTo(initialRotation);
                    p = titleLabelBackground._normalize(pi.topLeftPoint[0], pi.topLeftPoint[1], "drawing", "local");
                } else {
                    p = pi.topLeftPoint;
                }
                titleLabelBackground.setRect(isc.DrawItem._makeCoordinate(p[0]) - titleLabelPadding,
                                             isc.DrawItem._makeCoordinate(p[1]) - titleLabelPadding,
                                             dims.width + twiceTitleLabelPadding,
                                             dims.height + twiceTitleLabelPadding);
                if (finalRotation != null) {
                    titleLabelBackground.rotateTo(finalRotation);
                }
            }
        }
    }
},

_calculateTitleLabelPositionInfo : function (title, titleLabelProps, drawPane) {
    
    var titleRotationMode = this.titleRotationMode || isc.DrawItem._instancePrototype.titleRotationMode,
        dims = drawPane.measureLabel(title, titleLabelProps),
        center,
        p = new Array(2),
        localToDrawingTransform = this._getNormalizeTransform("local", "drawing");
    if (isc.isA.DrawLinePath(this) &&
        (titleRotationMode === "withLine" ||
         titleRotationMode === "withLineAlwaysUp"))
    {
        var segmentPoints = this._segmentPoints;
        
        var centerSegmentStartPoint = segmentPoints[1],
            centerSegmentEndPoint = segmentPoints[2];
        center = [(centerSegmentStartPoint[0] + centerSegmentEndPoint[0]) / 2,
                  (centerSegmentStartPoint[1] + centerSegmentEndPoint[1]) / 2];
    } else {
        center = this.getCenter();
    }
    if (titleRotationMode !== "neverRotate") {
        var halfWidth = dims.width / 2,
            halfHeight = dims.height / 2;
        localToDrawingTransform.transform(center[0] - halfWidth, center[1] - halfHeight, p);
        localToDrawingTransform.transform(center[0], center[1], center);

        var scale = this.scale;
        // If this draw item is scaled, we need to compensate for the scaling to compute the
        // top left point of the titleLabel because the titleLabel is not scaled.
        if (scale != null && scale.length >= 2) {
            
            p[0] -= (1 - scale[0]) * halfWidth;
            p[1] -= (1 - scale[1]) * halfHeight;
        }
    } else {
        localToDrawingTransform.transform(center[0], center[1], center);
        p[0] = center[0] - dims.width / 2;
        p[1] = center[1] - dims.height / 2;
    }
    p[0] = isc.DrawItem._makeCoordinate(p[0]);
    p[1] = isc.DrawItem._makeCoordinate(p[1]);
    return {
        topLeftPoint: p, // in drawing coordinates
        centerPoint: center, // in drawing coordinates
        dims: dims
    };
},


// Returns a bounding box inside which a DrawItem is forced to remain while keepInParentRect
// is enabled.
_getParentRect : function () {
    var box = this.keepInParentRect,
        fromBox = isc.isAn.Array(box),
        boundLeft = (fromBox && box[0] != null ? box[0] : this.drawPane._viewBoxLeft),
        boundTop = (fromBox && box[1] != null ? box[1] : this.drawPane._viewBoxTop),
        boundRight = (fromBox && box[2] != null ? box[2] :
            (this.drawPane._viewBoxLeft + this.drawPane._viewBoxWidth)),
        boundBottom = (fromBox && box[3] != null ? box[3] :
            (this.drawPane._viewBoxTop + this.drawPane._viewBoxHeight));

    // If we're showing the move knob and the move knob is offset in the left and/or top direction,
    // then we want to adjust the parent rect bounds so that the move knob at its offset will
    // also remain in the parent rect.
    var moveKnobOffset = this._getMoveKnobOffset();
    if (this.knobs && this.knobs.contains("move") &&
        moveKnobOffset != null &&
        (moveKnobOffset[0] != 0 || moveKnobOffset[1] != 0))
    {
        var bbox = this.getResizeBoundingBox(this._tempBoundingBox),
            position = this._getKnobPosition(this.moveKnobPoint, bbox);
        if (position != null &&
            !window.isNaN(position[0]) &&
            !window.isNaN(position[1]))
        {
            var moveKnobPosition0 = position[0] + moveKnobOffset[0],
                moveKnobPosition1 = position[1] + moveKnobOffset[1],
                dx,
                dy;

            if ((dy = bbox[1] - moveKnobPosition1) > 0) {
                boundTop += dy;
            }
            if ((dx = bbox[0] - moveKnobPosition0) > 0) {
                boundLeft += dx;
            }
            if ((dy = moveKnobPosition1 - bbox[3]) > 0) {
                boundBottom -= dy;
            }
            if ((dx = moveKnobPosition0 - bbox[2]) > 0) {
                boundRight -= dx;
            }
        }
    }

    return [boundLeft, boundTop, boundRight, boundBottom];
},

//> @method drawItem.dragResizeMove() (A)
// If +link{DrawItem.canDrag} is true and the +link{knobs,control knobs} include "resize" knobs,
// then this notification method will be fired when the user drag-resizes the draw item.
// @param position (string) provides which knob of the +link{resizeKnobPoints} was dragged
// @param x (integer) new x-coordinate of the knob
// @param y (integer) new y-coordinate of the knob
// @param dX (integer) horizontal distance moved
// @param dY (integer) vertical distance moved
// @visibility drawing
//<
dragResizeMove : function (position, x, y, dX, dY, state) {
    var startState = (state == "start"),
        moveState = (state == "move"),
        stopState = (state == "stop");
    

    var implementsSaveShape = (this._saveShape != null);

    var fixedPoint;
    if (startState) {
        var isLeftKnob = position.contains("L"),
            isRightKnob = position.contains("R"),
            isTopKnob = position.contains("T"),
            isBottomKnob = position.contains("B"),
            oppositePosition = (isTopKnob ? "B" : "T") + (isLeftKnob ? "R" : "L");

        fixedPoint = this._dragResizeFixedPoint = this._getKnobPosition(oppositePosition);
        this._dragResizePosition = position;
    } else {
        fixedPoint = this._dragResizeFixedPoint;
    }
    this._dragResizeX = x;
    this._dragResizeY = y;

    // Calculate the original width and height at the start of the drag-resize.  These
    // values will be used below for proportional resizing.
    var bbox = null;
    if (startState) {
        bbox = this.getResizeBoundingBox(this._tempBoundingBox);
        this._dragResizeLeft0 = Math.round(bbox[0]);
        this._dragResizeRight0 = Math.round(bbox[2]);
        this._dragResizeTop0 = Math.round(bbox[1]);
        this._dragResizeBottom0 = Math.round(bbox[3]);
    }
    var origWidth = this._dragResizeRight0 - this._dragResizeLeft0,
        origHeight = this._dragResizeBottom0 - this._dragResizeTop0;

    // Set the new dimensions to match the dimensions of the minimum rectangle
    // spanning the fixedPoint and (x, y).
    var oldLeft = 0, oldRight = 0, oldTop = 0, oldBottom = 0,
        oldCenterX = 0, oldCenterY = 0,
        oldShape = null,
        oldLocalTransform = null;
    if (implementsSaveShape) {
        oldLeft = this._dragResizeLeft0;
        oldRight = this._dragResizeRight0;
        oldTop = this._dragResizeTop0;
        oldBottom = this._dragResizeBottom0;
        if (startState) {
            var center = this._getRotationCenter();
            oldCenterX = this._dragResizeCenterX0 = center.cx;
            oldCenterY = this._dragResizeCenterY0 = center.cy;
            oldShape = this._dragResizeShape0 = this._saveShape();
            oldLocalTransform = this._dragResizeLocalTransform0 = this._getLocalTransform(true);
        } else {
            oldCenterX = this._dragResizeCenterX0;
            oldCenterY = this._dragResizeCenterY0;
            oldShape = this._dragResizeShape0;
            oldLocalTransform = this._dragResizeLocalTransform0;
        }
    } else {
        // Use the current bounding box.  Note that this has already been calculated above if
        // `startState` is true.
        if (!startState) {
            bbox = this.getResizeBoundingBox(this._tempBoundingBox);
        }
        oldLeft = Math.round(bbox[0]);
        oldRight = Math.round(bbox[2]);
        oldTop = Math.round(bbox[1]);
        oldBottom = Math.round(bbox[3]);
        var center = this._getRotationCenter();
        oldCenterX = center.cx;
        oldCenterY = center.cy;
        oldLocalTransform = this._getLocalTransform(true);
    }
    var oldWidth = oldRight - oldLeft,
        oldHeight = oldBottom - oldTop;

    var newLeft = 0, newTop = 0, newRight = 0, newBottom = 0;
    if (position == "L" || position == "R") {
        newTop = oldTop;
        newBottom = oldBottom;
        newLeft = Math.min(fixedPoint[0], x);
        newRight = Math.max(fixedPoint[0], x);
    } else if (position == "T" || position == "B") {
        newLeft = oldLeft;
        newRight = oldRight;
        newTop = Math.min(fixedPoint[1], y);
        newBottom = Math.max(fixedPoint[1], y);
    } else {
        newLeft = Math.min(fixedPoint[0], x);
        newTop = Math.min(fixedPoint[1], y);
        newRight = Math.max(fixedPoint[0], x);
        newBottom = Math.max(fixedPoint[1], y);
    }

    var newWidth = (newRight - newLeft),
        newHeight = (newBottom - newTop);

    if (stopState) {
        delete this._dragResizeFixedPoint;
        delete this._dragResizePosition;
        delete this._dragResizeX;
        delete this._dragResizeY;
    }

    // If keepInParentRect is true or is some bounding box then restrict the new coordinates
    // and dimensions from the resize to within the specified bounds.
    if (this.keepInParentRect) {
        var box = this._getParentRect();
        newLeft = Math.max(newLeft, box[0]);
        newTop = Math.max(newTop, box[1]);
        newRight = Math.min(newRight, box[2]);
        newBottom = Math.min(newBottom, box[3]);
        newWidth = newRight - newLeft;
        newHeight = newBottom - newTop;
    }

    // Implement proportional resizing if it is currently enabled.
    if (this._useProportionalResizing &&
        // This is not well defined if the starting width or height is zero.
        origWidth != 0 && origHeight != 0 &&
        // Dragging must be free in two directions for this to make sense.
        !(position == "L" || position == "R" || position == "T" || position == "B"))
    {
        var sx = (newWidth / origWidth),
            sy = (newHeight / origHeight);
        if (sx < sy) {
            var dy = (newHeight - sx * origHeight);
            if (newTop == fixedPoint[1]) {
                newBottom -= dy;
            } else if (newBottom == fixedPoint[1]) {
                newTop += dy;
            }
            newHeight = newBottom - newTop;
        } else if (sy < sx) {
            var dx = (newWidth - sy * origWidth);
            if (newLeft == fixedPoint[0]) {
                newRight -= dx;
            } else if (newRight == fixedPoint[0]) {
                newLeft += dx;
            }
            newWidth = newRight - newLeft;
        }
    }

    // Disallow shrinking below zero size.
    var zeroSize = (Math.abs(newWidth) < 1 || Math.abs(newHeight) < 1);
    if (!stopState && !zeroSize) {
        this._dragResizeLeft = newLeft;
        this._dragResizeTop = newTop;
        this._dragResizeWidth = newWidth;
        this._dragResizeHeight = newHeight;
    }
    if (stopState && zeroSize) {
        if (this._dragResizeLeft != null) {
            // Revert to the last dragged resize to a nonzero size.
            newLeft = this._dragResizeLeft;
            newTop = this._dragResizeTop;
            newWidth = this._dragResizeWidth;
            newHeight = this._dragResizeHeight;
        } else {
            return false;
        }
    }
    if (stopState) {
        delete this._dragResizeLeft;
        delete this._dragResizeTop;
        delete this._dragResizeWidth;
        delete this._dragResizeHeight;
        delete this._dragResizeLeft0;
        delete this._dragResizeRight0;
        delete this._dragResizeTop0;
        delete this._dragResizeBottom0;
        if (implementsSaveShape) {
            delete this._dragResizeCenterX0;
            delete this._dragResizeCenterY0;
            delete this._dragResizeShape0;
            delete this._dragResizeLocalTransform0;
        }
    }

    // Call the appropriate notification method for SmartGWT.
    if (startState) {
        if (this.onDragResizeStart(newLeft, newTop, newWidth, newHeight) === false) {
            return false;
        }
    } else if (moveState) {
        if (this.onDragResizeMove(newLeft, newTop, newWidth, newHeight) === false) {
            return false;
        }
    }

    var newRight = newLeft + newWidth,
        newBottom = newTop + newHeight;
    this._setResizeBoundingBox(
        oldLeft, oldTop, oldRight, oldBottom,
        oldCenterX, oldCenterY, oldShape, oldLocalTransform,
        newLeft, newTop, newRight, newBottom);

    
    if (stopState) this.onDragResizeStop(newLeft, newTop, newWidth, newHeight);

    return true;
},

_proportionalResizing: "none",
_checkProportionalResizing : function () {
    var oldMode = this._proportionalResizing,
        newMode = this._getProportionalResizing();
    
    if (oldMode == newMode) {
        return;
    }
    if (oldMode == "none") {
        if (newMode == "always") {
            this._enableProportionalResizing(this);
        } else { // newMode is "modifier" or "modifierOff"
            var modifierOff = (newMode == "modifierOff"),
                downAction = this._enableProportionalResizing,
                upAction = this._disableProportionalResizing;
            if (modifierOff) {
                // The actions are swapped in "modifierOff" mode.
                downAction = this._disableProportionalResizing;
                upAction = this._enableProportionalResizing;
            }

            // In "modifier" mode, enable proportional resizing if the
            // `proportionalResizeModifiers` are currently being pressed.  In "modifierOff"
            // mode, enable proportional resizing if the modifier keys are not being pressed.
            var modifiersDown = isc.Page._modifierKeysDown(this.proportionalResizeModifiers);
            if (modifiersDown != modifierOff) {
                this._enableProportionalResizing(this);
            }

            isc.Page._registerModifierKeys(
                this.proportionalResizeModifiers, isc.Page._OR, downAction, upAction, this);
        }
    } else {
        this._disableProportionalResizing(this);
        if (oldMode != "always") { // oldMode is "modifier" or "modifierOff"
            isc.Page._unregisterModifierKeys(
                this.proportionalResizeModifiers, isc.Page._OR, this);
        }
    }

    // Save the new `proportionalResizing` mode.
    this._proportionalResizing = newMode;
},


_enableProportionalResizing : function (me) {
    if (!me._useProportionalResizing) {
        me._useProportionalResizing = true;
        if (me._dragResizeFixedPoint != null) {
            // Call dragResizeMove() again to trigger proportional resizing.
            me.dragResizeMove(
                me._dragResizePosition, me._dragResizeX, me._dragResizeY, 0, 0, "move");
        }
    }
},
_disableProportionalResizing : function (me) {
    if (me._useProportionalResizing) {
        me._useProportionalResizing = false;
        if (me._dragResizeFixedPoint != null) {
            // Call dragResizeMove() again to trigger regular resizing.
            me.dragResizeMove(
                me._dragResizePosition, me._dragResizeX, me._dragResizeY, 0, 0, "move");
        }
    }
},

_setResizeBoundingBox : function (
    oldLeft, oldTop, oldRight, oldBottom,
    oldCenterX, oldCenterY, oldShape, oldLocalTransform,
    newLeft, newTop, newRight, newBottom)
{
    
    if (oldLeft == oldRight || oldTop == oldBottom) {
        return;
    }
    if (newLeft == newRight || newTop == newBottom) {
        // Do not allow moving/resizing a shape to collapse it to a line or a point.
        return;
    }
    if (newLeft > newRight) {
        var swap = newLeft;
        newLeft = newRight;
        newRight = swap;
    }
    if (newTop > newBottom) {
        var swap = newTop;
        newTop = newBottom;
        newBottom = swap;
    }
    
    var sx = (newRight - newLeft) / (oldRight - oldLeft),
        sy = (newBottom - newTop) / (oldBottom - oldTop),
        dx = newLeft - sx * oldLeft,
        dy = newTop - sy * oldTop;
    

    var transform = isc.AffineTransform.create(sx, 0, dx, 0, sy, dy);
    if (this.exemptFromGlobalTransform) {
        var scrollLeft = this.drawPane.scrollLeft,
            scrollTop = this.drawPane.scrollTop;
        transform.preTranslate(-scrollLeft, -scrollTop);
        transform.translate(scrollLeft, scrollTop);
    } else if (this.drawPane != null) {
        
        var g = this.drawPane._getGlobalTransform(),
            gInv = this.drawPane._getInverseGlobalTransform();
        transform.leftMultiply(gInv);
        transform.rightMultiply(g);
    }

    // Combine the transform with the old local transform.
    transform.rightMultiply(oldLocalTransform);

    this._updateLocalTransform(
        transform, oldCenterX, oldCenterY, oldShape, true, this.resizeViaLocalTransformOnly);

    if (oldLeft != newLeft || oldTop != newTop) {
        this._moved(isc.DrawItem._makeCoordinate(newLeft - oldLeft), isc.DrawItem._makeCoordinate(newTop - oldTop));
    }
    if ((oldRight - oldLeft) != (newRight - newLeft) ||
        (oldBottom - oldTop) != (newBottom - newTop))
    {
        this._resized();
    }
},


_updateLocalTransform : function (transform, cx, cy, initialShape, fireReshaped, viaLocalTransformOnly) {
    
    var info = isc.AffineTransform._decomposeTransform(transform, cx, cy),
        translate = this.translate = this.translate || new Array(2),
        scale = this.scale = this.scale || new Array(2);
    translate[0] = info.dx;
    translate[1] = info.dy;
    scale[0] = info.sx;
    scale[1] = info.sy;
    this.xShearFactor = info.kx;
    this.yShearFactor = info.ky;
    this.rotation = info.theta * 180 / Math.PI;
    this._transform = null;

    var t = this._getLocalTransform();
    if (this.drawingVML) {
        this._vmlRendererHandle.setProperty("transform", t);
    }
    if (fireReshaped) {
        if (this.drawingVML) {
            this._vmlRendererHandle.flush();

        } else if (this.drawingSVG && !isc.isA.DrawGroup(this)) {
            this._svgHandle.setAttributeNS(null, "transform", "matrix(" + t.m00 + " " + t.m10 + " " +
                                                                          t.m01 + " " + t.m11 + " " +
                                                                          t.m02 + " " + t.m12 + ")");

        } else if (this.drawingBitmap) {
            this.drawPane.redrawBitmap();
        }

        this._reshaped();
    }
},


_updateRotationCenter : function (cx, cy, newCx, newCy) {
    var dx = 0, dy = 0, sx = 1, sy = 1,
        kx = this.xShearFactor || 0,
        ky = this.yShearFactor || 0,
        theta = 0;
    if (this.translate != null) {
        dx = this.translate[0];
        dy = this.translate[1];
    }
    if (this.scale && this.scale.length === 2) {
        sx = this.scale[0];
        sy = this.scale[1];
    }
    if (this.rotation != null) {
        theta = this.rotation * this._radPerDeg;
    }

    if (sx != 0 && sy != 0) {
        // Commute the translation matrix right, past the scale and two shear matrices.
        var denom = sx * sy,
            ex = (dx * sy - dy * sx * kx) / denom,
            ey = (dy * sx * (kx * ky + 1) - dx * sy * ky) / denom;

        
        var cos = Math.cos(theta),
            sin = Math.sin(theta),
            newEx = ex + (1 - cos) * (cx - newCx) + sin * (cy - newCy),
            newEy = ey - sin * (cx - newCx) + (1 - cos) * (cy - newCy);

        // Commute the translation matrix left, past the two shear and the scale matrices.
        var newDx = sx * ((kx * ky + 1) * newEx + kx * newEy),
            newDy = sy * (ky * newEx + newEy);

        var translate = this.translate = this.translate || new Array(2);
        translate[0] = newDx;
        translate[1] = newDy;
    }
},

//>@method drawItem.onDragResizeStart()
// If +link{DrawItem.canDrag} is true and the +link{knobs,control knobs} include "resize"
// knobs then this notification method will be fired when the user starts to drag the resize
// knobs of the draw item.
// @param newX (int) new X coordinate of this draw item within the DrawPane
// @param newY (int) new Y coordinate of this draw item within the DrawPane
// @param newWidth (int) new width of this draw item
// @param newHeight (int) new height of this draw item
// @return (boolean) return false to cancel the default behavior of allowing the shape to be
// drag-resized
// @visibility sgwt
//<
onDragResizeStart : function (newX, newY, newWidth, newHeight) {},

//>@method drawItem.onDragResizeMove()
// If +link{DrawItem.canDrag} is true and the +link{knobs,control knobs} include "resize"
// knobs then this notification method will be fired when the user drags the resize knobs of
// the draw item.
// @param newX (int) new X coordinate of this draw item within the DrawPane
// @param newY (int) new Y coordinate of this draw item within the DrawPane
// @param newWidth (int) new width of this draw item
// @param newHeight (int) new height of this draw item
// @return (boolean) return false to cancel the default behavior of allowing the shape to be
// drag-resized
// @visibility sgwt
//<
onDragResizeMove : function (newX, newY, newWidth, newHeight) {},

//>@method drawItem.onDragResizeStop()
// If +link{DrawItem.canDrag} is true and the +link{knobs,control knobs} include "resize"
// knobs then this notification method will be fired when the user completes a drag on the
// resize knobs of the draw item.
// @param newX (int) new X coordinate of this draw item within the DrawPane
// @param newY (int) new Y coordinate of this draw item within the DrawPane
// @param newWidth (int) new width of this draw item
// @param newHeight (int) new height of this draw item
// @visibility sgwt
//<
onDragResizeStop : function (newX, newY, newWidth, newHeight) {},

hideResizeKnobs : function () {
    if (this._resizeKnobs) {
        this._resizeKnobs.map("destroy");
        delete this._resizeKnobs;
    }
    if (this.resizeOutline) {
        this.resizeOutline.destroy();
        delete this.resizeOutline;
    }
},

//-----------------------------------------
//other knobs
//Implemented at the drawItem level - not exposed / supported for all drawItems  

//> @attr drawItem.startKnob (AutoChild DrawKnob : null : IR)
// If this item is showing <smartclient>"startPoint"</smartclient>
// <smartgwt>{@link com.smartgwt.client.types.KnobType#STARTPOINT}</smartgwt>
// +link{drawItem.knobs,control knobs}, this attribute specifies the AutoChild for the
// +link{DrawKnob} for start point of current drawItem.
//
// @visibility drawing
//<
startKnobDefaults: {
    knobShapeProperties: {
        _constructor: "DrawOval",
        radius : 4.5,
        lineWidth: 1,
        lineOpacity: 1,
        fillOpacity: 1,
        lineColor: "#333333",
        fillGradient: {
            id: "isc_defaultStartKnobGradient",
            x1: "0%", y1: "0%",
            x2: "0%", y2: "100%",
            colorStops: [
                {color: "#ffffff", offset: 0.15},
                {color: "#CAE9ED", offset: 0.5},
                {color: "#ffffff",   offset: 0.85}
            ] 
        }
    }
},

startKnobConstructor: "DrawKnob",

//> @attr drawItem.endKnob (AutoChild DrawKnob : null : IR)
// If this item is showing <smartclient>"endPoint"</smartclient>
// <smartgwt>{@link com.smartgwt.client.types.KnobType#ENDPOINT}</smartgwt>
// +link{drawItem.knobs,control knobs}, this attribute specifies the AutoChild for the
// +link{DrawKnob} for end point of current drawItem.
//
// @visibility drawing
//<
endKnobDefaults: {
    knobShapeProperties: {
        _constructor: "DrawOval",
        radius : 4.5,
        lineWidth: 1,
        lineOpacity: 1,
        fillOpacity: 1,
        lineColor: "#333333",
        fillGradient: {
            id: "isc_defaultEndKnobGradient",
            x1: "0%", y1: "0%",
            x2: "0%", y2: "100%",
            colorStops: [
                {color: "#ffffff", offset: 0.15},
                {color: "#CAE9ED", offset: 0.5},
                {color: "#ffffff",   offset: 0.85}
            ] 
        }
    }
},

endKnobConstructor: "DrawKnob",

// drawItem.updateControlKnobs()
// Fired in response to moved / resized
updateControlKnobs : function () {
    if (this._moveKnob) {
        var coords = this._getKnobPosition(this.moveKnobPoint),
            x = coords[0], y = coords[1],
            moveKnobOffset = this._getMoveKnobOffset();
        if (moveKnobOffset) {
            x += moveKnobOffset[0];
            y += moveKnobOffset[1];
        }
        this._moveKnob.setCenterPoint(x, y, false);
    }
    if (this._resizeKnobs) {
        for (var i = 0; i < this._resizeKnobs.length; i++) {
            var knob = this._resizeKnobs[i],
                coords = this._getKnobPosition(knob.point);
            knob.setCenterPoint(coords[0], coords[1], false);
        }
    }
    if (this.resizeOutline) {
        var bbox = this.getResizeBoundingBox(this._tempBoundingBox);
        this.resizeOutline.setRect(isc.DrawItem._makeCoordinate(bbox[0]),
                                   isc.DrawItem._makeCoordinate(bbox[1]),
                                   isc.DrawItem._makeCoordinate(bbox[2] - bbox[0]),
                                   isc.DrawItem._makeCoordinate(bbox[3] - bbox[1]));
    }
},

//> @method drawItem.setDrawPane()
// Setter for +link{DrawItem.drawPane,drawPane}.
// @param drawPane (DrawPane) new value for <code>this.drawPane</code>.
// @visibility drawing
//<
setDrawPane : function (drawPane) {
    if (drawPane == this.drawPane) return;
    // Support setDrawPane(null) as an equivalent to deparent type call in Canvas - clear from
    // any parent drawPane (but keep the item around for future use)
    if (drawPane == null) {
        this.erase();
        this.drawPane = null;
        return;
    }
    
    drawPane.addDrawItem(this);
},

// In some cases (currently DrawLinePaths only) we use additional lines to render out arrowheads
// rather than relying on native SVG etc arrows
_drawLineStartArrow : function () {
    return false;
},
_drawLineEndArrow : function () {
    return false;
},

//Computes the closest point to `point` on the line segment from `fixedPoint` to `point` that
//intersects a bounding box.  `point` is modified in-place, so duplicate() it before calling
//this function, if necessary.
_intersectLineSegmentBox : function (fixedPoint, point, box) {
 var slope = new Array(2);
 slope[0] = 1 / (slope[1] = (point[1] - fixedPoint[1]) / (point[0] - fixedPoint[0]));

 // Loop over the four sides of the box.  On each iteration, find the intersection between
 // the constant-x or -y line that forms a side of the box and the line segment from
 // fixedPoint to point.
 for (var m = 0; m < 4; ++m) {
     var k = m % 2, l = (k + 1) % 2, s = (m < 2 ? -1 : 1), bound = box[s + 1 + k];
     var fixedPointWithinBound = !(s * fixedPoint[k] > s * bound),
         pointWithinBound = !(s * point[k] > s * bound);
 
     if (!pointWithinBound && fixedPointWithinBound) {
         
         point[k] = bound;
         point[l] = fixedPoint[l] + (bound - fixedPoint[k]) * slope[l];

     } else if (!(pointWithinBound || fixedPointWithinBound)) {
         // The line does not intersect the box, so this drag cannot be allowed.
         return null;
     }
 }
 return point;
},

// Erasing / Destroying 
// ---------------------------------------------------------------------------------------

//> @method drawItem.erase()
// Erase this drawItem's visual representation and remove it from its DrawGroup (if any) and
// DrawPane.
// <P>
// To re-draw the item within the DrawPane, call +link{drawItem.draw()} again, or use
// +link{drawPane.addDrawItem()} to move to another DrawGroup.
// 
// @visibility drawing
//<
// This is like Canvas.deparent().
erase : function (erasingAll, willRedraw) {
    if (!erasingAll) { // drawPane.erase() or drawGroup.erase() just drops the whole drawItems array
        if (willRedraw) {
            
            if (!this.drawPane._erasedDrawItems) this.drawPane._erasedDrawItems = [ this ];
            else this.drawPane._erasedDrawItems.add(this);
        }
        if (this.drawGroup) {
            this.drawGroup.drawItems.remove(this);

            

        // If this DrawItem is in a DrawGroup, then it is not present in the DrawPane's drawItems array.
        } else if (this.drawPane) {
            if (this.drawPane.drawItems) this.drawPane.drawItems.remove(this);
            if (this.item != null) {
                this.drawPane.quadTree.remove(this.item);
                delete this.item;
            }

            
        }
    }

    this.clear(erasingAll, willRedraw);

    if (this.knobs && this.drawPane != null) {
        for (var i = 0; i < this.knobs.length; i++) {
            this._hideKnobs(this.knobs[i]);
        }
    }

    // hide the titleLabel and background
    var titleLabel = this.titleLabel,
        titleLabelBackground = this.titleLabelBackground;
    if (titleLabel != null) {
        if (this.destroying) {
            titleLabel.destroy(erasingAll);
            if (titleLabelBackground != null) titleLabelBackground.destroy(erasingAll);
        } else {
            if (!titleLabel.destroyed && titleLabel.drawPane != null) {
                titleLabel.drawPane.removeDrawItem(titleLabel);
                titleLabel.setContents("", true);
            }
            if (titleLabelBackground != null &&
                !titleLabelBackground.destroyed && titleLabelBackground.drawPane != null)
            {
                titleLabelBackground.drawPane.removeDrawItem(titleLabelBackground);
            }
        }
    } 

    if (this.exemptFromGlobalTransform) {
        var drawPane = this._getDrawPane();
        if (drawPane != null) {
            
            drawPane._exemptedDrawItems.remove(this);
        }
    }
    this.drawGroup = null;
    this.drawPane = null;
},

// drawItem.clear()
// This is like Canvas.clear().
clear : function (clearingAll, willRedraw) {
    if (!this._drawn) {
        // if we are pending a deferred draw clear it and exit without warning
        var drawPane = this.drawPane;
        if (drawPane != null && drawPane.cancelDeferredDraw(this)) return;
        this.logInfo("DrawItem not yet drawn - exiting clear()"); 
        return;
    }

    if (this.drawingVML) {
        // see comments in Canvas.clearHandle(); note that VML is IE only
        var vmlHandle = this._getVMLHandle();
        if (vmlHandle != null) {
            if (isc.Page.isLoaded()) {
                isc.Log.logDebug("Page is loaded when erasing vml drawItem " + this.ID +
                                    " - clearing outerHTML (drawPane is " +
                                                this.drawPane.ID + ")", "drawing");
                vmlHandle.outerHTML = "";
            } else {
                var isNonVMLLabel = (
                        isc.isA.DrawLabel(this) && !(this.synchTextMove || this.useTextpath)),
                    vmlContainer;
                if (isNonVMLLabel) {
                    vmlContainer = this.drawPane.getHandle();
                } else if (this.drawGroup != null && this.drawGroup.renderGroupElement) {
                    vmlContainer = this.drawGroup._getVMLHandle();
                } else {
                    vmlContainer = isc.Element.get(this.drawPane.getID() + "_vml_box");
                }
                isc.Log.logDebug("Page is not loaded when eraseing vml drawItem " + this.ID +
                                 " - removing vmlHandle from vmlContainer " +
                                 " (drawPane is " + this.drawPane.ID + ")", "drawing");
                vmlContainer.removeChild(vmlHandle);
            }
        }
        this._vmlContainer = null;
        this._vmlHandle = null;
        this._vmlStrokeHandle = null;
        this._vmlFillHandle = null;
        this._vmlTextHandle = null;
        this._vmlRendererHandle = null;

        
        this.drawingVML = false;

    } else if (this.drawingSVG) {
        var svgHandle = this._svgHandle;
        if (willRedraw) this._erasedSVGHandle = svgHandle;
        else if (svgHandle != null) {
            var svgContainer;
            if (this.drawGroup != null && this.drawGroup.renderGroupElement) {
                svgContainer = this.drawGroup._svgHandle;
            } else {
                svgContainer = this._svgContainer;
            }
            
            svgContainer.removeChild(svgHandle);
        }
        this._svgDocument = null;
        this._svgContainer = null;
        this._svgHandle = null;

        
        this.drawingSVG = false;

    } else if (this.drawingBitmap) {
        if (!clearingAll) {
            this.drawPane.redrawBitmap();

        } else {
            // drawPane.erase() will call redrawBitmap()
        }

        this.drawingBitmap = false;
    }

    this._clearEventParent();
    this._drawn = false;
},

//> @method drawItem.destroy()
// Permanently destroys this DrawItem, similar to +link{Canvas.destroy()}.
//
// @see group:memoryLeaks
// @visibility drawing
//<
destroy : function (destroyingAll) {
    // if we're already destroyed don't do it again
    if (this.destroyed) return;

    // set a flag so we don't do unnecessary work during a destroy()
    this.destroying = true;

    this.erase(destroyingAll);

    // clear our global ID (removes the window.ID pointer to us)
    isc.ClassFactory.dereferenceGlobalID(this);

    // Clean up listeners of the `proportionalResizeModifiers` being pressed.
    this._checkProportionalResizing();

    delete this.destroying;
    this.destroyed = true;

    this.Super("destroy", arguments);
},

//> @attr drawItem.destroyed (boolean : null : RA)
// Flag indicating a drawItem has been destroyed, similar to +link{canvas.destroyed}.
// @visibility drawing
//<

//> @attr drawItem.destroying (boolean : null : RA)
// Flag indicating a drawItem is mid-destruction, similar to +link{canvas.destroying}.
// @visibility drawing
//<

//--------------------------------------------------------------------------------
//  DrawItem renderers
//  These convenience methods handle the line (stroke) and fill attributes for all
//  primitives. Override these in subclasses for more advanced compound element
//  renderings.
//  NOTE: empty string is supported for no color (transparent) fillColor and
//      lineColor, so subclasses can specify transparency
//--------------------------------------------------------------------------------
_$eventProxy: isc.Canvas._instancePrototype._$eventProxy,


vmlLineEventsOnly: true,

_$solid: "solid",
_$strokedFalse: " stroked='false'",
_$filledFalse: " filled='false'",
_getElementVML : function (buffer, id, conversionContext) {
    var vmlRenderer = this.drawPane._vmlRenderer;
    vmlRenderer.buffer = buffer;
    this._renderVML(vmlRenderer, id, conversionContext);
},

_hasStroke : function () {
    return this.lineOpacity != 0 && !!this.lineColor;
},
_hasFill : function () {
    return this.fillOpacity != 0 && (this.fillGradient != null || this.fillColor != null);
},

_getVMLHandle : function () {
    if (isc.isA.DrawLabel(this)) {
        if (this._vmlHandle != null) return this._vmlHandle;
        return (this._vmlHandle = isc.Element.get("isc_DrawItem_" + this.drawItemID));
    } else {
        return this._vmlRendererHandle && this._vmlRendererHandle.getHandle();
    }
},

// Implement in subclasses.
_renderVML : function (vmlRenderer, id, conversionContext) {},

_getSvgHandleID : function () {
    var drawPane = this.drawPane;
    return drawPane && (drawPane._useSvgHelper ? "isc_DrawItem_" + this.drawItemID : this.getID() + "_"+"handle");
},
_getSvgHandle : function () {
    var svgDocument = this._svgDocument;
    return svgDocument && svgDocument.getElementById(this._getSvgHandleID());
},

_getEventProxyAttribute : function () {
    var eventProxy;
    if (this.excludeFromQuadTree) {
        eventProxy = this.drawPane;
    } else {
        eventProxy = this.eventProxy;
        if (eventProxy == null) eventProxy = this;

        if (isc.isA.DrawItem(eventProxy)) {
            var topMostGroupRectGroup = null;
            for (var dg = this.drawGroup; dg != null; dg = dg.drawGroup) {
                
                if (dg.useGroupRect) {
                    topMostGroupRectGroup = dg;
                }
            }
            if (topMostGroupRectGroup != null) eventProxy = topMostGroupRectGroup;
        }
    }
    return this._$eventProxy + "='" + eventProxy.ID + "'";
},

//> @method drawItem.getSvgString()
// Generates a string containing the SVG source of this DrawItem.
//
// <p><b>NOTE:</b> The generated SVG source assumes that the default namespace is <code>http://www.w3.org/2000/svg</code>
// and that namespace prefix <code>xlink</code> refers to namespace name <code>http://www.w3.org/1999/xlink</code>.
//
// @visibility drawing
//<
getSvgString : function (conversionContext) {
    conversionContext = conversionContext || isc.SVGStringConversionContext.create();
    var defSvgStrings = conversionContext.defSvgStrings || (conversionContext.defSvgStrings = {});

    var stroke = (this.lineColor ? this.lineColor : "none"),
        fill = "none";

    var gradient = this.fillGradient;
    if (isc.isAn.Object(isc.isA.String(gradient)
                        ? gradient = this.drawPane.getGradient(gradient)
                        : gradient))
    {
        var baseGradientID = gradient[conversionContext.gradientIdProperty];
        if (conversionContext.printForExport === false) {
            
            baseGradientID = this.drawPane.addGradient(gradient);

        } else if (baseGradientID == null) {
            baseGradientID = gradient[conversionContext.gradientIdProperty] = conversionContext.getNextSvgDefID();
        }

        var gradientSvgString = this.drawPane._getGradientSvgString(baseGradientID, gradient, conversionContext, this);
        var gradientDefID = conversionContext.useGradientIDs[this.ID];
        defSvgStrings[gradientDefID] = gradientSvgString;
        fill = "url(#" + gradientDefID + ")";

    } else if (this.fillColor) {
        fill = this.fillColor;
    }

    var svgString = "<" + this.svgElementName;

    var svgHandleID = this._getSvgHandleID();
    if (svgHandleID != null) {
        svgString += " id='" + svgHandleID + "'";
    }

    if (!conversionContext.printForExport) {
        svgString += " " + this._getEventProxyAttribute();
    }

    var currentCursor = this.getCurrentCursor();
    if (currentCursor != null) {
        svgString += " cursor='" + currentCursor + "'";
    }

    svgString += (
        " stroke='" + stroke +
        "' stroke-width='" + this.lineWidth + "px" +
        "' stroke-opacity='" + this.lineOpacity +
        "' stroke-dasharray='" + this._getSVGDashArray() +
        "'"
        );
    
    if (!this._isClosed()) {
        svgString += " stroke-linecap='" + this.lineCap + "'";
    }
    svgString += (
        " fill='" + fill +
        "' fill-opacity='" + this.fillOpacity
        );

    if (this.svgFilter != null && this.drawPane) {
        var filterSvgString = defSvgStrings[this.svgFilter];
        if (!filterSvgString) {
            filterSvgString = defSvgStrings[this.svgFilter] = this.drawPane._getFilterSvgString(this.svgFilter);
        }
        if (filterSvgString) {
            svgString += "' filter='url(#" + this.svgFilter + ")";
        }
    } else if (this.shadow) {
        var shadowFilterID = this._getSVGShadowFilterID();
        if (!defSvgStrings[shadowFilterID]) {
            defSvgStrings[shadowFilterID] = this._getShadowFilterSvgString(shadowFilterID, this.shadow);
        }
        svgString += "' filter='url(#" + shadowFilterID + ")";
    }

    var firstTransform = true,
        translate = this.translate,
        scale = this.scale,
        xShearFactor = this.xShearFactor,
        yShearFactor = this.yShearFactor,
        rotation = this.rotation,
        center = rotation && this._getRotationCenter();
    
    if (translate) {
        svgString += (
            (firstTransform ? "' transform='" : " ") +
            "translate(" + translate[0] + " " + translate[1] + ")");
        firstTransform = false;
    }
    if (scale && scale.length === 2) {
        svgString += (
            (firstTransform ? "' transform='" : " ") +
            "scale(" + scale[0] + " " + scale[1] + ")");
        firstTransform = false;
    }
    if (xShearFactor) {
        svgString += (
            (firstTransform ? "' transform='" : " ") +
            "skewX(" + Math.atan(xShearFactor) + ")");
        firstTransform = false;
    }
    if (yShearFactor) {
        svgString += (
            (firstTransform ? "' transform='" : " ") +
            "skewY(" + Math.atan(yShearFactor) + ")");
    }
    if (rotation && center && center.cx !== null && center.cy !== null) {
        svgString += (
            (firstTransform ? "' transform='" : " ") +
            "rotate(" + rotation + " " + center.cx + " " + center.cy + ")");
    }

    // arrow heads
    if (this._supportsSvgMarkers()) {
        var startArrow = this.startArrow;
        if (startArrow && !this._drawLineStartArrow()) {
            var svgStartArrowID = this._getSVGStartArrowID();
            if (!defSvgStrings[svgStartArrowID]) {
                defSvgStrings[svgStartArrowID] = this._getArrowMarkerSvgString(svgStartArrowID, stroke, this.lineOpacity, true, startArrow);
            }
            svgString += "' marker-start='url(#" + svgStartArrowID + ")";
        }
        var endArrow = this.endArrow;
        if (endArrow && !this._drawLineEndArrow()) {
            var svgEndArrowID = this._getSVGEndArrowID();
            if (!defSvgStrings[svgEndArrowID]) {
                defSvgStrings[svgEndArrowID] = this._getArrowMarkerSvgString(svgEndArrowID, stroke, this.lineOpacity, false, endArrow);
            }
            svgString += "' marker-end='url(#" + svgEndArrowID + ")";
        }
    }

    

    svgString += "' " + this.getAttributesSVG() + ">";
    if (this.contents) {
        svgString += isc.makeXMLSafe(this.contents);
    }
    svgString += "</" + this.svgElementName + ">";
    return svgString;
},

_svgElementNamesWithMarkerSupport: {
    "path": true,
    "line": true,
    "polyline": true,
    "polygon": true
},
_supportsSvgMarkers : function () {
    return this.svgElementName in this._svgElementNamesWithMarkerSupport;
},

getAttributesSVG : function () {
    // implement in subclasses
    return "";
},

_normalizeLinearGradient : function (def) {
    return isc.DrawItem._normalizeLinearGradient(def, this.getBoundingBox());
},

_normalizeRadialGradient : function (def) {
    return isc.DrawItem._normalizeRadialGradient(def, this.getBoundingBox(), this.getCenter());
},

_drawLinePattern : function (x1, y1, x2, y2, context) {
    var dashArray;
    switch (this.linePattern.toLowerCase()) {
    default:
        dashArray = [10, 5];
        break;
    case "dash":
        dashArray = [10, 10];
        break;
    case "dot":
        dashArray = [1, 10];
        break;
    case "longdash":
        dashArray = [20, 10];
        break;
    case "shortdash":
        dashArray = [10, 5];
        break;
    case "shortdot":
        dashArray = [1, 5];
        break;
    }

    var dashCount = dashArray.length,
        dx = (x2 - x1), dy = (y2 - y1),
        dist = isc.Math._hypot(dx, dy),
        distRemaining = dist,
        dashIndex = 0,
        draw = true;
    this.bmMoveTo(x1, y1, context);
    while (distRemaining >= 0.1) {
        var dashLength = Math.min(dashArray[dashIndex++ % dashCount], distRemaining);
        x1 += dx * dashLength / dist;
        y1 += dy * dashLength / dist;
        this[draw ? 'bmLineTo' : 'bmMoveTo'](x1, y1, context);
        distRemaining -= dashLength;
        draw = !draw;
    }
},

drawStroke : function (context) {
    context.lineWidth = this.lineWidth;
    context.lineCap = this.lineCap;
    context.strokeStyle = this.lineColor;
    context.globalAlpha = this.lineOpacity;
    context.beginPath();
    this.drawBitmapPath(context);
    context.stroke();
},

// Note: stroke and fill are each path-ending operations.
// if transparent line or fill color (false, null, empty string), avoid drawing fill or
// filling.
// Subclasses are intended to implement drawBitmapPath() and have a series of lineTo(), arcTo()
// et al. calls.
drawBitmap : function (context) {
    context.save();
    try {
        var t = this._getLocalTransform();
        context.transform(t.m00, t.m10, t.m01, t.m11, t.m02, t.m12);

        if (this.shadow) {
            context.shadowColor = this.shadow.color;
            context.shadowBlur = this.shadow.blur;
            context.shadowOffsetX = this.shadow.offset[0];
            context.shadowOffsetY = this.shadow.offset[1];
        }
        if (this._hasFill()) {
            var fill = this.fillColor,
                def = (typeof(this.fillGradient) === 'string' ? this.drawPane.getGradient(this.fillGradient) : this.fillGradient);
            if (def != null) {
                var vector,
                    offset;
                if (isc.DrawItem._isSimpleGradient(def) || isc.DrawItem._isLinearGradient(def)) {
                    vector = this._normalizeLinearGradient(def);
                    fill = context.createLinearGradient(vector[0],vector[1],vector[2],vector[3]);
                } else {
                    vector = this._normalizeRadialGradient(def);
                    fill = context.createRadialGradient(vector[0],vector[1],vector[2],vector[3],vector[4],vector[5]);
                }
                if (def.startColor && def.endColor) {
                    fill.addColorStop(0.0,def.startColor);
                    fill.addColorStop(1.0,def.endColor);
                } else if (def.colorStops && def.colorStops.length) {
                    for (var i = 0; i < def.colorStops.length; ++i) {
                        offset = def.colorStops[i].offset;
                        if (typeof(offset) === 'string' && offset.endsWith('%')) {
                            offset = parseFloat(offset.substring(0,offset.length-1)) / 100.0;
                        }
                        fill.addColorStop(offset,def.colorStops[i].color);
                    }
                }
            }

            context.fillStyle = fill;
            context.globalAlpha = this.fillOpacity;
            context.beginPath();
            // Temporarily change the linePattern to "solid" so that the path is not broken up
            // drawing the other line pattern styles (e.g. "dashed").
            var origLinePattern = this.linePattern;
            this.linePattern = "solid";
            try {
                this.drawBitmapPath(context);
            } finally {
                this.linePattern = origLinePattern;
            }
            context.fill();
        }
        if (this._hasStroke()) {
            if(this.shadow && fill) {
                context.shadowColor = null;
                context.shadowBlur = null;
                context.shadowOffsetX = 0;
                context.shadowOffsetY = 0;
            }
            this.drawStroke(context);
        }
    } catch (e) {
        if (!isc.Log.supportsOnError) this._reportJSError(e);
        else this.logWarn("exception during drawBitmap(): " + e);
    } finally {
        context.restore();
    }
},


_getSVGShadowFilterID : function () {
    return "isc_DrawItem_" + this.drawItemID + "_"+"shadowFilter";
},

_getShadowFilterSvgString : function (id, shadow) {
    var blur = isc.isA.Number(shadow.blur) ? shadow.blur : 10,
        color = isc.DrawItem._lookUpRGBA(shadow.color),
        offset = shadow.offset || [0, 0];

    blur = Math.max(0, blur);

    var svgString = (
        
        "<filter id='" + id + "' x='-1000%' y='-1000%' width='2100%' height='2100%' color-interpolation-filters='sRGB'>" +
        "<feOffset result='offOut' in='SourceGraphic' dx='" + offset[0] + "' dy='" + offset[1] + "'></feOffset>" +
        // This color matrix sets the R,G,B channels of the offset SourceGraphic to the R,G,B
        // of the shadow color and multiplies the alpha channel of the offset SourceGraphic by
        // the alpha of the shadow color.
        // This is to match steps 5 and 6 of http://www.w3.org/TR/2dcontext2/#when-shadows-are-drawn
        "<feColorMatrix result='cmatOut' in='offOut' type='matrix' values='0 0 0 0 " + color[0] + " " +
                                                                          "0 0 0 0 " + color[1] + " " +
                                                                          "0 0 0 0 " + color[2] + " " +
                                                                          "0 0 0 " + color[3] + " 0'></feColorMatrix>"
        );
    var blendIn2;
    if (blur > 0) {
        
        svgString += "<feGaussianBlur result='blurOut' in='cmatOut' stdDeviation='" + (blur / 2) + "'></feGaussianBlur>";
        blendIn2 = "blurOut";
    } else {
        blendIn2 = "cmatOut";
    }
    svgString += (
        "<feBlend in='SourceGraphic' in2='" + blendIn2 + "' mode='normal'></feBlend>" +
        "</filter>"
        );
    return svgString;
},





_getSVGStartArrowID : function () {
    return "isc_DrawItem_" + this.drawItemID + "_startArrow";
},

_getSVGEndArrowID : function () {
    return "isc_DrawItem_" + this.drawItemID + "_endArrow";
},


_getArrowMarkerSvgString : function (id, color, opacity, start, style) {
    var lineWidth = this.lineWidth,
        arrowDelta = 5 + lineWidth,
        twiceArrowDelta = 2 * arrowDelta,
        _3 = 0.75 * lineWidth,
        _13 = arrowDelta + _3,
        _23 = twiceArrowDelta + _3,
        _6 = 1.5 * lineWidth,
        _16 = arrowDelta + _6,
        _26 = twiceArrowDelta + _6;

    

    var svgString = (
        "<marker id='" + id +
        "' markerUnits='userSpaceOnUse' orient='auto' markerWidth='" + _16 +
        "' markerHeight='" + _26
        );

    if (style === "open") {
        var refX;
        // With lineCap:"square", the line ends of the path overshoot the marker reference point.
        // Compensate for this overshoot here by adjusting the marker reference point.
        if (this.lineCap === "square") {
            refX = (start ? _6 : arrowDelta);
        } else {
            refX = (start ? _3 : _13);
        }
        svgString += (
            "' refX='" + refX +
            "' refY='" + _13 +
            "'>" +
            
            "<polyline stroke-dasharray='9999px' stroke-width='" + lineWidth +
            "px' stroke-opacity='" + opacity +
            "' stroke-linecap='" + this.lineCap +
            "' stroke='" + color +
            "' fill='none' points='" + (start
                                        ? _13 + " " + _3 + " " + _3 + " " + _13 + " " + _13 + " " + _23
                                        : _3 + " " + _3 + " " + _13 + " " + _13 + " " + _3 + " " + _23) +
            "'></polyline>"
            );

    } else {
        
        var refX;
        if (this.lineCap === "square") {
            refX = (start ? _6 + _3 : arrowDelta - _3);
        } else {
            refX = (start ? _6 : arrowDelta);
        }
        svgString += (
            "' refX='" + refX +
            "' refY='" + _13 +
            "'>" +
            "<polygon stroke='none' fill='" + color +
            "' fill-opacity='" + opacity +
            "' points='" + (start
                            ? _16 + " 0 " + _3 + " " + _13 + " " + _16 + " " + _26
                            : "0 0 " + _13 + " " + _13 + " 0 " + _26) +
            "'></polygon>"
            );
    }

    

    svgString += "</marker>";
    return svgString;
},

// Re-generates the <marker> defs for this item's start and end arrow heads.
_regenerateSvgMarkers : function (skipStartMarker, skipEndMarker) {
    

    var drawPane = this.drawPane,
        defSvgStrings = drawPane._batchDrawConversionContext.defSvgStrings,
        stroke = (this.lineColor ? this.lineColor : "none"),
        defsSvgString = "";

    var startArrow = this.startArrow;
    if (!skipStartMarker && startArrow && !this._drawLineStartArrow()) {
        var svgStartArrowID = this._getSVGStartArrowID(),
            startMarkerElem = this._svgDocument.getElementById(svgStartArrowID);
        
        startMarkerElem.parentNode.removeChild(startMarkerElem);

        defsSvgString += (defSvgStrings[svgStartArrowID] = this._getArrowMarkerSvgString(svgStartArrowID, stroke, this.lineOpacity, true, startArrow));
    }

    var endArrow = this.endArrow;
    if (!skipEndMarker && endArrow && !this._drawLineEndArrow()) {
        var svgEndArrowID = this._getSVGEndArrowID(),
            endMarkerElem = this._svgDocument.getElementById(svgEndArrowID);
        
        endMarkerElem.parentNode.removeChild(endMarkerElem);

        defsSvgString += (defSvgStrings[svgEndArrowID] = this._getArrowMarkerSvgString(svgEndArrowID, stroke, this.lineOpacity, false, endArrow));
    }

    if (defsSvgString != "") {
        drawPane._insertDefsSvgString(defsSvgString);
    }
},

//--------------------------------------------------------------------------------
//  visibility
//--------------------------------------------------------------------------------

//> @method drawItem.show()
// Make this drawItem visible.
// @visibility drawing
//<
show : function () {
    if (!this.hidden) return;
    this.hidden = false;
    if (this.drawingVML) {
        if (isc.isA.DrawLabel) {
            this._getVMLHandle().style.visibility = "visible";
        } else {
            this._vmlRendererHandle.setProperty("visibility", "visible").flush();
        }
    } else if (this.drawingSVG) {
        this._svgHandle.setAttributeNS(null, "visibility", "visible");
    } else if (this.drawingBitmap) {
        this.drawPane.redrawBitmap();
    }
},


//> @method drawItem.hide()
// Hide this drawItem.
// @visibility drawing
//<
hide : function () {
    if (this.hidden) return;
    this.hidden = true;
    if (this.drawingVML) {
        if (isc.isA.DrawLabel(this)) {
            this._getVMLHandle().style.visibility = "hidden";
        } else {
            this._vmlRendererHandle.setProperty("visibility", "hidden").flush();
        }
    } else if (this.drawingSVG) {
        this._svgHandle.setAttributeNS(null, "visibility", "hidden");
    } else if (this.drawingBitmap) {
        this.drawPane.redrawBitmap();
    }
},

//--------------------------------------------------------------------------------
//  line & fill
//--------------------------------------------------------------------------------

//> @method drawItem.setLineWidth()
// Update lineWidth for this drawItem.
// @param width (int) new pixel lineWidth
// @visibility drawing
//<
setLineWidth : function (width, alwaysUpdate) {
    if (width == null || (!alwaysUpdate && this.lineWidth == width)) return;
    this.lineWidth = width;
    if (this.drawingVML) {
        // NB: applying zoom correction - so this method is for external callers ONLY
        this._setLineWidthVML(width * this.drawPane.zoomLevel);

    } else if (this.drawingSVG) {
        var svgHandle = this._svgHandle;
        svgHandle.setAttributeNS(null, "stroke-width", width + "px");
        svgHandle.setAttributeNS(null, "stroke-dasharray", this._getSVGDashArray()); // see _getSVGDashArray

        // Also update the arrow heads. Because the size and positioning of the marker depends
        // on the line width, regenerate the <marker> defs.
        if (this._supportsSvgMarkers()) {
            this._regenerateSvgMarkers();
        }

    } else if (this.drawingBitmap) {
        this.drawPane.redrawBitmap();
    }

    this._reshaped();
},
// split out so internal callers can apply zoom correction
_setLineWidthVML : function (width) {
    this._vmlRendererHandle.setProperty("lineWidth", width).flush();
},

//> @method drawItem.setLineColor()
// Update lineColor for this drawItem.
// @param color (CSSColor) new line color.  Pass null for transparent.
// @visibility drawing
//<
setLineColor : function (color) {
    this.lineColor = color;
    if (this.drawingVML) {
        this._vmlRendererHandle.setProperty("lineColor", color)
            .setProperty("stroked", this._hasStroke()).flush();

    } else if (this.drawingSVG) {
        if (!color) color = "none";
        this._svgHandle.setAttributeNS(null, "stroke", color);

        // Also update the arrow heads.
        if (this._supportsSvgMarkers()) {
            var startArrow = this.startArrow;
            if (startArrow && !this._drawLineStartArrow()) {
                var startMarkerElem = this._svgDocument.getElementById(this._getSVGStartArrowID());
                
                if (startArrow === "open") {
                    startMarkerElem.setAttributeNS(null, "stroke", color);
                } else {
                    
                    startMarkerElem.setAttributeNS(null, "fill", color);
                }
            }

            var endArrow = this.endArrow;
            if (endArrow && !this._drawLineEndArrow()) {
                var endMarkerElem = this._svgDocument.getElementById(this._getSVGEndArrowID());
                
                if (startArrow === "open") {
                    endMarkerElem.setAttributeNS(null, "stroke", color);
                } else {
                    
                    endMarkerElem.setAttributeNS(null, "fill", color);
                }
            }
        }

    } else if (this.drawingBitmap) {
        this.drawPane.redrawBitmap();
    }
},


//> @method drawItem.setLineOpacity()
// Update lineOpacity for this drawItem.
// @param opacity (float) new opacity, as a number between 0 (transparent) and 1 (opaque).
// @visibility drawing
//<
setLineOpacity : function (opacity) {
    if (opacity == null) return;
    this.lineOpacity = opacity;
    if (this.drawingVML) {
        this._vmlRendererHandle.setProperty("lineOpacity", opacity)
            .setProperty("stroked", this._hasStroke()).flush();

    } else if (this.drawingSVG) {
        this._svgHandle.setAttributeNS(null, "stroke-opacity", opacity);

        // Also update the arrow heads.
        if (this._supportsSvgMarkers()) {
            var startArrow = this.startArrow;
            if (startArrow && !this._drawLineStartArrow()) {
                var startMarkerElem = this._svgDocument.getElementById(this._getSVGStartArrowID());
                
                if (startArrow === "open") {
                    startMarkerElem.setAttributeNS(null, "stroke-opacity", opacity);
                } else {
                    
                    startMarkerElem.setAttributeNS(null, "fill-opacity", opacity);
                }
            }

            var endArrow = this.endArrow;
            if (endArrow && !this._drawLineEndArrow()) {
                var endMarkerElem = this._svgDocument.getElementById(this._getSVGEndArrowID());
                
                if (startArrow === "open") {
                    endMarkerElem.setAttributeNS(null, "stroke-opacity", opacity);
                } else {
                    
                    endMarkerElem.setAttributeNS(null, "fill-opacity", opacity);
                }
            }
        }

    } else if (this.drawingBitmap) {
        this.drawPane.redrawBitmap();
    }
},


//> @method drawItem.setLinePattern()
// Update linePattern for this drawItem.
// @param pattern (LinePattern) new linePattern to use
// @visibility drawing
//<
setLinePattern : function (pattern) {
    if (pattern == null) return;
    this.linePattern = pattern;
    if (this.drawingVML) {
        this._vmlRendererHandle.setProperty("linePattern", pattern).flush();

    } else if (this.drawingSVG) {
        this._svgHandle.setAttributeNS(null, "stroke-dasharray", this._getSVGDashArray());

    } else if (this.drawingBitmap) {
        this.drawPane.redrawBitmap();
    }
},
// Maps to emulate the preset VML dashstyles using the more powerful (but raw) SVG stroke-dasharray
// attribute. Dash and gap sizes here are multiples of lineWidth, regardless of the lineCap type.  
// VML also provides shortdashdot, shortdashdotdot, dashdot, longdashdot, longdashdotdot; not
// currently emulated. To emulate a dashdot pattern will require 4 values (1 dash, 1 dot, 2 gaps);
// dashdotdot patterns will require 6 values.
_linePatternDashSizeMap : {shortdot:1, dot:1, shortdash:3, dash:4, longdash:8},
_linePatternGapSizeMap : {shortdot:1, dot:3, shortdash:1, dash:3, longdash:3},
// NB: We must take lineWidth and lineCap into account to emulate the VML behavior
//      in SVG, so setLineWidth() and setLineCap() must also reset stroke-dasharray.    
_getSVGDashArray : function () {
    var lp = this.linePattern;
    if (!lp || lp == "solid" || lp == "") return "none";
    // for SVG only - Take an array of dash-gap patterns for maximum flexibility. Dash-gap patterns
    // are specified in VML (eg "4 2 1 2" for dash-dot), but they appear to be broken, at least in
    // my IE6xpsp2.
    // TODO someone else try VML dash-gap patterns (not using this join code!) in case I missed something
    // TODO could process this array to adapt to lineWidth/lineCap, as we do for the preset patterns
    if (isc.isAn.Array(lp)) return lp.join("px,")+"px";
    var dashSize = this._linePatternDashSizeMap[lp];
    var gapSize = this._linePatternGapSizeMap[lp];
    // At least in FFSVG, "round" or "square" caps extend into the gap, effectively stealing
    // 1 lineWidth from the gap size and adding it to the dash size, so we compensate here.
    if (this.lineCap != "butt") { // implies "round" or "square"
        // At least in FFSVG, the SVG renderer throws a scribble fit on a zero dash size, and 
        // loses parts of the line on very small dash sizes. 0.1px seems OK for now.
        dashSize = Math.max(0.1, dashSize-1);
        gapSize++;
    }
    return (dashSize*this.lineWidth)+"px,"+(gapSize*this.lineWidth)+"px";
},


//> @method drawItem.setLineCap()
// Update lineCap for this drawItem.
// @param cap (LineCap) new lineCap to use
// @visibility drawing
//<
setLineCap : function (cap) {
    if (cap == null) return;
    this.lineCap = cap;
    if (this.drawingVML) {
        cap = (this.lineCap == isc.VMLRenderer._$butt ? isc.VMLRenderer._$flat : cap);
        this._vmlRendererHandle.setProperty("lineCap", cap).flush();

    } else if (this.drawingSVG) {
        var svgHandle = this._svgHandle;
        svgHandle.setAttributeNS(null, "stroke-linecap", cap);
        svgHandle.setAttributeNS(null, "stroke-dasharray", this._getSVGDashArray()); // see _getSVGDashArray

        // Also update the arrow heads. Because the position of the <marker>'s reference point
        // depends on the linecap, regenerate the <marker> defs.
        if (this._supportsSvgMarkers()) {
            this._regenerateSvgMarkers();
        }

    } else if (this.drawingBitmap) {
        this.drawPane.redrawBitmap();
    }
},

//> @method drawItem.setShadow(shadow)
// Update shadow for this drawItem.
// @param shadow (Shadow) new shadow
// @visibility drawing
//<
setShadow : function (shadow) {
    this.shadow = shadow;

    if (this.drawingVML) {
        this._vmlRendererHandle.setProperty("shadow", shadow).flush();

    } else if (this.drawingSVG) {
        var shadowFilterID = this._getSVGShadowFilterID();
        var shadowFilterElem = this._svgDocument.getElementById(shadowFilterID);
        if (shadowFilterElem != null) shadowFilterElem.parentNode.removeChild(shadowFilterElem);
        var defSvgStrings = this.drawPane._batchDrawConversionContext.defSvgStrings;
        delete defSvgStrings[shadowFilterID];
        if (shadow == null) {
            this._svgHandle.removeAttributeNS(null, "filter");
        } else {
            var defSvgString = defSvgStrings[shadowFilterID] = this._getShadowFilterSvgString(shadowFilterID, shadow);
            this.drawPane._insertDefsSvgString(defSvgString);
            this._svgHandle.setAttributeNS(null, "filter", "url(#" + shadowFilterID + ")");
        }

    } else if (this.drawingBitmap) {
        this.drawPane.redrawBitmap();
    }
},

// Arrowheads
// ---------------------------------------------------------------------------------------

//> @method drawItem.supportsStartArrow() (A)
// Does this DrawItem +link{Class.isMethodSupported(),support} +link{setStartArrow()}?
// For example, this is false for +link{DrawRect} and +link{DrawOval}, and true for +link{DrawLine}.
// @return (boolean) whether setStartArrow() is supported by this DrawItem.
// @visibility drawing
//<
supportsStartArrow : function () {
    return this.getClass().isMethodSupported("setStartArrow");
},

//> @method drawItem.setStartArrow()
// Set the arrowhead at the beginning of this path.
// <p>
// <b>NOTE:</b> Not all DrawItem classes support arrowheads. You can use +link{supportsStartArrow()}
// to dynamically check whether a DrawItem instance supports this method.
// @param arrowStyle (ArrowStyle) style of arrow to use
// @visibility drawing
//<
// NOTE: we support passing an SVG marker element ID as startArrow so we can reuse the same
// arrowheads for multiple drawItems. Note that element IDs are supported for setting only;
// if you read startArrow after draw() it will always be an arrow style.
setStartArrow : function (startArrow) {    
    var wasLineArrow = this._drawLineStartArrow();    
    var undef;
    if (startArrow !== undef) this.startArrow = startArrow;
    else startArrow = this.startArrow;

    var isLineArrow = this._drawLineStartArrow(); 

    if (wasLineArrow != isLineArrow) {
        this.setStartPoint(this.startPoint);
    }
    // suppress rendering native block arrows with special line-arrows
    if (isLineArrow) startArrow = null;

    if (this.drawingVML) {
        this._vmlRendererHandle.setProperty(
            "startArrow", (startArrow ? startArrow : isc.VMLRenderer._$none)).flush();

    } else if (this.drawingSVG) {
        if (this._supportsSvgMarkers()) {
            this._regenerateSvgMarkers(false, true);
        }

    } else if (this.drawingBitmap) {
        this.drawPane.redrawBitmap();
    }
},

//> @method drawItem.supportsEndArrow() (A)
// Does this DrawItem +link{Class.isMethodSupported(),support} +link{setEndArrow()}?
// For example, this is false for +link{DrawRect} and +link{DrawOval}, and true for +link{DrawLine}.
// @return (boolean) whether setEndArrow() is supported by this DrawItem.
// @visibility drawing
//<
supportsEndArrow : function () {
    return this.getClass().isMethodSupported("setEndArrow");
},

//> @method drawItem.setEndArrow()
// Set the arrowhead at the end of this path.
// <p>
// <b>NOTE:</b> Not all DrawItem classes support arrowheads. You can use +link{supportsEndArrow()}
// to dynamically check whether a DrawItem instance supports this method.
// @param arrowStyle (ArrowStyle) style of arrow to use
// @visibility drawing
//<
setEndArrow : function (endArrow) {
    var wasLineArrow = this._drawLineEndArrow();
    var undef;
    if (endArrow !== undef) this.endArrow = endArrow;
    else endArrow = this.endArrow;

    var isLineArrow = this._drawLineEndArrow();
    if (wasLineArrow != isLineArrow) {
        this.setEndPoint(this.endPoint);
    }

    if (isLineArrow) endArrow = null;
    if (this.drawingVML) {
        this._vmlRendererHandle.setProperty(
            "endArrow", (endArrow ? endArrow : isc.VMLRenderer._$none)).flush();

    } else if (this.drawingSVG) {
        if (this._supportsSvgMarkers()) {
            this._regenerateSvgMarkers(true, false);
        }

    } else if (this.drawingBitmap) {
        this.drawPane.redrawBitmap();
    }
},

//> @method drawItem.moveBy()
// Move the shape by the specified deltas for the left and top coordinate.
//
// @param dX (int) change to left coordinate in pixels
// @param dY (int) change to top coordinate in pixels
// @visibility drawing
//<
moveBy : function (dX, dY) {
    

    var center = this._getRotationCenter(),
        centerX = center.cx,
        centerY = center.cy,
        bbox = this.getResizeBoundingBox(this._tempBoundingBox);
    this._setResizeBoundingBox(
        bbox[0], bbox[1], bbox[2], bbox[3],
        centerX, centerY,
        (this._saveShape ? this._saveShape() : null),
        this._getLocalTransform(true),
        bbox[0] + dX, bbox[1] + dY, bbox[2] + dX, bbox[3] + dY);
},

//> @method drawItem.moveTo()
// Move the DrawItem to the specified coordinates in the global coordinate system.  The
// specified coordinates will become the top-left point of the
// +link{drawItem.getResizeBoundingBox(),resize bounding box}.
// @param left (integer) new left coordinate in pixels
// @param top (integer) new top coordinate in pixels
// @visibility drawing
//<
moveTo : function (left, top) {
    
    var center = this._getRotationCenter(),
        centerX = center.cx,
        centerY = center.cy,
        bbox = this.getResizeBoundingBox(this._tempBoundingBox),
        dx = (left == null ? 0 : left - bbox[0]),
        dy = (top == null ? 0 : top - bbox[1]);
    if (Math.abs(dx) >= 0.00001 ||
        Math.abs(dy) >= 0.00001)
    {
        this._setResizeBoundingBox(
            bbox[0], bbox[1], bbox[2], bbox[3],
            centerX, centerY,
            (this._saveShape ? this._saveShape() : null),
            this._getLocalTransform(true),
            bbox[0] + dx, bbox[1] + dy, bbox[2] + dx, bbox[3] + dy);
    }
},

//> @method drawItem.setCenterPoint()
// Change the center point for this DrawItem.
// @param left (int) X coordinate of the center point (in the global coordinate system).
// @param top (int) Y coordinate of the center point (in the global coordinate system).
// @visibility drawing
//<
setCenterPoint : function (left, top) {
    if (isc.isAn.Array(left)) { // conversion needed for SGWT
        top = left[1];
        left = left[0];
    }

    var centerPoint = this.getCenter();
    this._movePointToPoint(left, top, centerPoint[0], centerPoint[1]);
},


_movePointToPoint : function (left, top, left0, top0) {
    var v = this._normalize(left0, top0, "local", "global"),
        dx = isc.DrawItem._makeCoordinate(left == null ? 0 : left - v[0]),
        dy = isc.DrawItem._makeCoordinate(top == null ? 0 : top - v[1]);
    if (!(dx == 0 && dy == 0)) {
        this.moveBy(dx, dy);
    }
},

//> @method drawItem.resizeBy()
// Resize the shape by the specified deltas.
// @param dX (int) number of pixels to resize by horizontally
// @param dY (int) number of pixels to resize by vertically
// @visibility drawing
//<
resizeBy : function (dX, dY) {
    var center = this._getRotationCenter(),
        centerX = center.cx,
        centerY = center.cy,
        bbox = this.getResizeBoundingBox(this._tempBoundingBox);
    this._setResizeBoundingBox(
        bbox[0], bbox[1], bbox[2], bbox[3],
        centerX, centerY,
        (this._saveShape ? this._saveShape() : null),
        this._getLocalTransform(true),
        bbox[0], bbox[1], bbox[2] + dX, bbox[3] + dY);
},

//> @method drawItem.resizeTo()
// Resize to the specified size
// @param width (integer) new width
// @param height (integer) new height
// @visibility drawing
//<
resizeTo : function (width, height) {
    var center = this._getRotationCenter(),
        centerX = center.cx,
        centerY = center.cy,
        bbox = this.getResizeBoundingBox(this._tempBoundingBox),
        dX = 0, dY = 0;
    if (width != null) {
        dX = width - (bbox[2] - bbox[0]);
    }
    if (height != null) {
        dY = height - (bbox[3] - bbox[1]);
    }
    this._setResizeBoundingBox(
        bbox[0], bbox[1], bbox[2], bbox[3],
        centerX, centerY,
        (this._saveShape ? this._saveShape() : null),
        this._getLocalTransform(true),
        bbox[0], bbox[1], bbox[2] + dX, bbox[3] + dY);
},

//> @method drawItem.rotateBy()
// Rotate the shape by the relative rotation in degrees
// @param degrees (float) number of degrees to rotate from current orientation.
// @visibility drawing
//<
rotateBy : function (degrees) {
    if (Math.abs(degrees % 360) < 0.00001) return;
    var transform = this._getNormalizeTransform("local", "drawing"),
        center0 = this._getRotationCenter(),
        cx0 = center0.cx,
        cy0 = center0.cy,
        v = transform.transform(cx0, cy0),
        r = this._getLocalTransform(true).duplicate()
            .rightMultiply(transform.getInverse())
            .rotate(degrees, v[0], v[1])
            .rightMultiply(transform);
    this._updateLocalTransform(r, cx0, cy0, null, true, true);
},

//> @method drawItem.rotateTo()
// Rotate the shape by the absolute rotation in degrees
// @param degrees (float) number of degrees to rotate
// @visibility drawing
//<
rotateTo : function (degrees) {
    
    
    var transform = this._getNormalizeTransform("local", "drawing"),
        theta = isc.AffineTransform._decomposeTransform(transform, 0, 0).theta,
        rotation = theta * 180 / Math.PI;
    this.rotateBy(degrees - rotation);
},

_redrawAfterModifyingLocalTransform : function () {
    this._transform = null;
    if (this.drawingVML) {
        if (this._vmlRendererHandle != null) {
            this._vmlRendererHandle
                .setProperty("transform", this._getLocalTransform())
                .flush();
        } else if (isc.isA.DrawLabel(this)) {
            var drawPane = this.drawPane;
            this.erase();
            drawPane.addDrawItem(this);
        }

    } else if (this.drawingSVG) {
        var t = this._getLocalTransform();
        this._svgHandle.setAttributeNS(null, "transform", "matrix(" + t.m00 + " " + t.m10 + " " +
                                                                      t.m01 + " " + t.m11 + " " +
                                                                      t.m02 + " " + t.m12 + ")");

    } else if (this.drawingBitmap) {
        this.drawPane.redrawBitmap();
    }
    this._reshaped();
},

//> @method drawItem.scaleBy()
// Scale the shape by the x, y multipliers
// @param x (float) scale in the x direction
// @param y (float) scale in the y direction
// @visibility drawing
//<
scaleBy : function (x, y) {
    var center0 = this._getRotationCenter(),
        cx0 = center0.cx,
        cy0 = center0.cy,
        transform = this._getNormalizeTransform("local", "drawing"),
        s = this._getLocalTransform(true).duplicate()
            .rightMultiply(transform.getInverse())
            .scale(x, y)
            .rightMultiply(transform);
    this._updateLocalTransform(s, cx0, cy0, null, true, true);
},

//> @method drawItem.scaleTo()
// Scale the shape by the x, y multipliers
// @param x (float) scale in the x direction
// @param y (float) scale in the y direction
// @visibility drawing
//<
scaleTo : function (x, y) {
    
    var scale = this.scale;
    if (scale == null) {
        scale = this.scale = new Array(2);
    }
    scale[0] = x;
    scale[1] = y;
    this._redrawAfterModifyingLocalTransform();
},


// Fill
// ---------------------------------------------------------------------------------------

//> @method drawItem.setFillColor()
// Update fillColor for this drawItem.
// @param color (CSSColor) new fillColor to use.  Pass null for transparent.
// @visibility drawing
//<
setFillColor : function (color) {
    this.fillColor = color;
    if (this.drawingVML) {
        var hasFill = this._hasFill();
        var handle = this._vmlRendererHandle.setProperty("fillColor", color);
        var fillOpacity = !this.vmlLineEventsOnly && !hasFill ? 0 : this.fillOpacity;
        if (fillOpacity != null) handle.setProperty("fillOpacity", fillOpacity);
        handle.setProperty("filled", !this.vmlLineEventsOnly || hasFill)
            .flush();
    } else if (this.drawingSVG) {
        this._svgHandle.setAttributeNS(null, "fill",
            (this.fillColor && this.fillColor != "") ? this.fillColor : "none"
        );
    } else if (this.drawingBitmap) {
        this.drawPane.redrawBitmap();
    }
},

//> @method drawItem.setFillGradient()
// Update fillGradient for this drawItem.
// @param gradient (Gradient) new gradient to use.  Pass null for transparent.
// @visibility drawing
//<
setFillGradient : function (gradient) {
    this.fillGradient = gradient;
    if (this.drawingVML) {
        var hasFill = this._hasFill();
        var handle = this._vmlRendererHandle.setProperty("fillGradient", gradient);
        var fillOpacity = !this.vmlLineEventsOnly && !hasFill ? 0 : this.fillOpacity;
        if (fillOpacity != null) handle.setProperty("fillOpacity", fillOpacity);
        handle.setProperty("filled", !this.vmlLineEventsOnly || this._hasFill())
            .flush();

    } else if (this.drawingSVG) {
        var drawPane = this.drawPane,
            batchDrawConversionContext = drawPane._batchDrawConversionContext;

        var gradientDefID;

        // Remove the old "use gradient", which is unique to this item.
        gradientDefID = batchDrawConversionContext.useGradientIDs[this.ID];
        if (gradientDefID != null) {
            var existingDef = this._svgDocument.getElementById(gradientDefID);
            if (existingDef != null) {
                existingDef.parentNode.removeChild(existingDef);
            }
            delete batchDrawConversionContext.defSvgStrings[gradientDefID];
        }

        if (isc.isAn.Object(isc.isA.String(gradient)
                            ? gradient = drawPane.getGradient(gradient)
                            : gradient))
        {
            
            // Make sure that the DrawPane knows about this gradient, because it may be that
            // the gradient was removed and is now being re-set on a draw item.
            var baseGradientID = drawPane.addGradient(gradient),
                baseGradientDefID = drawPane.getID() + "_" + baseGradientID;

            var gradientSvgString = drawPane._getGradientSvgString(baseGradientID, gradient, batchDrawConversionContext, this);
            gradientDefID = batchDrawConversionContext.useGradientIDs[this.ID];
            
            batchDrawConversionContext.defSvgStrings[gradientDefID] = gradientSvgString;

            // Insert the new "use gradient" into the SVG document's defs, as well as the
            // base gradient if not already present.
            var defsSvgString = gradientSvgString,
                existingBaseDef = this._svgDocument.getElementById(baseGradientDefID);
            if (existingBaseDef == null) {
                defsSvgString += batchDrawConversionContext.defSvgStrings[baseGradientDefID];
            }
            drawPane._insertDefsSvgString(defsSvgString);

            this._svgHandle.setAttributeNS(null, "fill", "url(#" + gradientDefID + ")");

        } else {
            this._svgHandle.setAttributeNS(null, "fill", this.fillColor ? this.fillColor : "none");
        }

    } else if (this.drawingBitmap) {
        this.drawPane.redrawBitmap();
    }
},

//> @method drawItem.setFillOpacity()
// Update fillOpacity for this drawItem.
// @param opacity (float) new opacity, as a number between 0 (transparent) and 1 (opaque).
// @visibility drawing
//<
setFillOpacity : function (opacity) {
    if (opacity != null && opacity != this.fillOpacity) {
        this.fillOpacity = opacity;
        if (this.drawingVML) {
            var hasFill = this._hasFill();
            this._vmlRendererHandle.setProperty("fillOpacity", !this.vmlLineEventsOnly && !hasFill ? 0 : opacity)
                .setProperty("filled", !this.vmlLineEventsOnly || hasFill)
                .flush();

        } else if (this.drawingSVG) {
            this._svgHandle.setAttributeNS(null, "fill-opacity", opacity);

        } else if (this.drawingBitmap) {
            this.drawPane.redrawBitmap();
        }
    }
},

// <canvas> methods wrapped with exception handlers / logging and workarounds as needed
// ---------------------------------------------------------------------------------------

bmMoveTo : function (left, top, context) {
    
    var offset = (this.autoOffset && (this.lineWidth == 0 || parseInt(this.lineWidth) % 2) == 1 ? 0.5 : 0);
    try {
        context.moveTo(left+offset, top+offset); 
    } catch (e) {
        this.logWarn("error on moveTo(): left: " + left + ", top: " + top +
                     (this.logIsInfoEnabled() ? this.getStackTrace() : ""));
    }
},

bmArc : function (centerPointX, centerPointY, radius, startRadian, endRadian, context) {
    var offset = (this.autoOffset && (this.lineWidth == 0 || parseInt(this.lineWidth) % 2) == 1 ? 0.5 : 0);
    try {
        context.arc(centerPointX+offset, centerPointY+offset, radius, startRadian, endRadian, false);
    } catch (e) {
        this.logWarn("error on arc(): centerPointX: " + centerPointX + ", centerPointY: " 
                     + ", radius: " + radius + ", startRadian: " + startRadian + ", endRadian: " + endRadian +
                     (this.logIsInfoEnabled() ? this.getStackTrace() : ""));
    }
},

bmLineTo : function (left, top, context) {
    var offset = (this.autoOffset && (this.lineWidth == 0 || parseInt(this.lineWidth) % 2) == 1 ? 0.5 : 0);
    try {
        context.lineTo(left+offset, top+offset); 
    } catch (e) {
        this.logWarn("error on lineTo(): left: " + left + ", top: " + top +
                     (this.logIsInfoEnabled() ? this.getStackTrace() : ""));
    }
},

bmQuadraticCurveTo : function (x1, y1, x2, y2, context) {
    var offset = (this.autoOffset && (this.lineWidth == 0 || parseInt(this.lineWidth) % 2) == 1 ? 0.5 : 0);
    try {
        context.quadraticCurveTo(x1+offset, y1+offset, x2+offset, y2+offset); 
    } catch (e) {
        this.logWarn("error on quadraticCurveTo(): x1: " + x1 + ", y1: " + y1 + ", x2: " + x2 + ", y2: " + y2 +
                     (this.logIsInfoEnabled() ? this.getStackTrace() : ""));
    }
},

bmBezierCurveTo : function (cp1x, cp1y, cp2x, cp2y, p2x, p2y, context) {
    var offset = (this.autoOffset && (this.lineWidth == 0 || parseInt(this.lineWidth) % 2) == 1 ? 0.5 : 0);
    try {
        context.bezierCurveTo(
            cp1x + offset, cp1y + offset,
            cp2x + offset, cp2y + offset,
            p2x + offset, p2y + offset);
    } catch (e) {
        var stackTrace = (this.logIsInfoEnabled() ? this.getStackTrace() : "");
        this.logWarn(
            "error on bezierCurveTo(): cp1x: " + cp1x + ", cp1y: " + cp1y + ", cp2x: " + cp2x +
            ", cp2x: " + cp2y + ", p2x: " + p2x + ", p2y: " + p2y + stackTrace);
    }
},

bmFillText : function (text, left, top, context) {
    try {
        context.fillText(text, left, top); 
    } catch (e) {
        this.logWarn("error on fillText(): text: " + text + 
                     ", left: " + left + ", top: " + top +
                     (this.logIsInfoEnabled() ? this.getStackTrace() : ""));
    }
},

bmStrokeText : function (text, left, top, context) {
    try {
        context.strokeText(text, left, top); 
    } catch (e) {
        this.logWarn("error on fillText(): text: " + text + 
                     ", left: " + left + ", top: " + top +
                     (this.logIsInfoEnabled() ? this.getStackTrace() : ""));
    }
}

}); // end DrawItem.addProperties

isc.DrawItem.addClassProperties({
    _IDCounter:0,   // for unique DOM IDs
    
    // ControlKnobs: see drawItem.knobs
    // For every knobType "<XXX>" we will call a show<XXX>KnobType() method
    // This method creates and caches these function names centrally (avoids reassembling strings)
    _showKnobsFunctionNameTemplate:["show", ,"Knobs"],
    _hideKnobsFunctionNameTemplate:["hide", ,"Knobs"],
    _getShowKnobsFunctionName : function (knobType, hide) {
        
        if (!this._knobTypeFunctionMap) {
            this._knobTypeFunctionMap = {};
        }
        
        if (this._knobTypeFunctionMap[knobType] == null) {
            knobType = knobType.substring(0,1).toUpperCase() + knobType.substring(1);
            
            this._showKnobsFunctionNameTemplate[1] = knobType;
            this._hideKnobsFunctionNameTemplate[1] = knobType;
            
            this._knobTypeFunctionMap[knobType] = [
                this._showKnobsFunctionNameTemplate.join(isc.emptyString),
                this._hideKnobsFunctionNameTemplate.join(isc.emptyString)
            ];
        }
        return hide ? this._knobTypeFunctionMap[knobType][1] 
                     : this._knobTypeFunctionMap[knobType][0];
    },
    
    _allKnobTypes: ["resize","move","startPoint","endPoint","controlPoint1","controlPoint2"],

    
    registerEventStringMethods : function () {
        var EH = isc.EH;
        for (var eventName in EH.eventTypes) {
            // Register all events as string methods using the EventHandler's authoritative list
            this.registerStringMethods(EH.eventTypes[eventName], EH._eventHandlerArgString);
        }
    },

    _normalizeLinearGradient : function (def, boundingBox) {
        // Convert percentages to absolute values within the bounding box.
        var arr = new Array(4),
            width = Math.abs(boundingBox[2] - boundingBox[0]),
            height = Math.abs(boundingBox[3] - boundingBox[1]);
        if (def.direction != null) {
            
            var direction = (typeof(def.direction) === 'string' ? parseFloat(def.direction) : def.direction),
                radians = direction * isc.Math._radPerDeg,
                sin = Math.sin(radians), cos = Math.cos(radians),
                centerX = (boundingBox[0] + boundingBox[2]) / 2,
                centerY = (boundingBox[1] + boundingBox[3]) / 2,
                dx = (sin == 0 ? width : Math.min(width, Math.abs(height * cos / sin))) / 2,
                dy = (cos == 0 ? height : Math.min(height, Math.abs(width * sin / cos))) / 2;
            if (sin < 0) {
                dy = -dy;
            }
            if (cos < 0) {
                dx = -dx;
            }
            arr[0] = Math.round(centerX - dx);
            arr[1] = Math.round(centerY - dy);
            arr[2] = Math.round(centerX + dx);
            arr[3] = Math.round(centerY + dy);
        } else {
            // The x-coordinate of the start point of the gradient
            if (typeof(def.x1) === 'string') {
                if (def.x1.endsWith('%')) {
                    arr[0] = (boundingBox[0] + parseFloat(def.x1) * width / 100.0);
                } else {
                    arr[0] = parseFloat(def.x1);
                }
            } else if (isc.isA.Number(def.x1)) {
                arr[0] = def.x1;
            }
            // The y-coordinate of the start point of the gradient
            if (typeof(def.y1) === 'string') {
                if (def.y1.endsWith('%')) {
                    arr[1] = (boundingBox[1] + parseFloat(def.y1) * height / 100.0);
                } else {
                    arr[1] = parseFloat(def.y1);
                }
            } else if (isc.isA.Number(def.y1)) {
                arr[1] = def.y1;
            }
            // The x-coordinate of the end point of the gradient
            if (typeof(def.x2) === 'string') {
                if (def.x2.endsWith('%')) {
                    arr[2] = (boundingBox[0] + parseFloat(def.x2) * width / 100.0);
                } else {
                    arr[2] = parseFloat(def.x2);
                }
            } else if (isc.isA.Number(def.x2)) {
                arr[2] = def.x2;
            }
            // The y-coordinate of the end point of the gradient
            if (typeof(def.y2) === 'string') {
                if (def.y2.endsWith('%')) {
                    arr[3] = (boundingBox[1] + parseFloat(def.y2) * height / 100.0);
                } else {
                    arr[3] = parseFloat(def.y2);
                }
            } else if (isc.isA.Number(def.y2)) {
                arr[3] = def.y2;
            }
        }
        return arr;
    },

    _normalizeRadialGradient : function (def, boundingBox, center) {
        // Convert percentages to absolute values using the bounding box values.
        var arr = new Array(6);
        // The x-coordinate of the starting circle of the gradient
        if (typeof(def.fx) === 'string') {
            if (def.fx.endsWith('%')) {
                arr[0] = (center[0] + (parseFloat(def.fx) / 100.0));
            } else {
                arr[0] = (center[0] + parseFloat(def.fx));
            }
        } else if (def.fx != null) {
            arr[0] = (center[0] + def.fx);
        } else {
            arr[0] = center[0];
        }
        // The y-coordinate of the starting circle of the gradient
        if (typeof(def.fy) === 'string') {
            if (def.fy.endsWith('%')) {
                arr[1] = (center[1] + (parseFloat(def.fy) / 100.0));
            } else {
                arr[1] = (center[1] + parseFloat(def.fy));
            }
        } else if (def.fy != null) {
            arr[1] = (center[1] + def.fy);
        } else {
            arr[1] = center[1];
        }
        // The radius of the starting circle of the gradient
        arr[2] = 0;
        // The x-coordinate of the ending circle of the gradient
        if (typeof(def.cx) === 'string' && isc.isA.Number(boundingBox[0])) {
            if (def.cx.endsWith('%')) {
                arr[3] = (boundingBox[0] - Math.round(boundingBox[0] * parseInt(def.cx) / 100.0));
            } else {
                arr[3] = (center[0] + parseInt(def.cx));
            }
        } else {
            arr[3] = (center[0] + def.cx);
        }
        // The y-coordinate of the ending circle of the gradient
        if (typeof(def.cy) === 'string' && isc.isA.Number(boundingBox[1])) {
            if (def.cx.endsWith('%')) {
                arr[4] = (boundingBox[1] - Math.round(boundingBox[1] * parseInt(def.cy) / 100.0));
            } else {
                arr[4] = (center[1] + parseInt(def.cy));
            }
        } else {
            arr[4] = (center[1] + def.cy);
        }
        // The radius of the ending circle of the gradient
        if (typeof(def.r) === 'string' && isc.isA.Number(boundingBox[0]) && isc.isA.Number(boundingBox[1])) {
            var width = boundingBox[2] - boundingBox[0],
                height = boundingBox[3] - boundingBox[1],
                r = isc.Math._hypot(width, height);
            if (def.r.endsWith('%')) {
                arr[5] = (r * parseFloat(def.r) / 100.0);
            } else {
                arr[5] = (r * parseFloat(def.r));
            }
        } else {
            arr[5] = def.r;
        }
        return arr;
    }
});

isc.DrawItem.registerEventStringMethods();









//------------------------------------------------------------------------------------------
//> @class DrawGroup
//
// DrawItem subclass to manage a group of other DrawItem instances.
// <P> 
// A DrawGroup has no local visual representation other than that of its drawItems. Adding items
// to a drawGroup allows for central event handling, and allows them to be manipulated
// (drawn, scaled, etc) together.
// <P>
// DrawItems are added to a DrawGroup by creating the DrawItems with +link{drawItem.drawGroup}
// set to the DrawGroup, or by creating a DrawGroup with +link{drawGroup.drawItems}.
// <P>
// DrawGroups handle events by having an explicitly specified group rectangle 
// (see +link{drawGroup.getGroupRect()}). This rectangle has no visual representation within the draw pane
// (is not visible) but any user-interactions within the specified coordinates will trigger
// group level events.
// <P>
// DrawGroups may contain other DrawGroups.
//
// @inheritsFrom DrawItem
// @treeLocation Client Reference/Drawing/DrawItem
// @visibility drawing
//<
//------------------------------------------------------------------------------------------

isc.defineClass("DrawGroup", "DrawItem").addProperties({

    //> @attr drawGroup.knobs
    // DrawGroup only supports the
    // <smartclient>"move"</smartclient>
    // <smartgwt>{@link com.smartgwt.client.types.KnobType#MOVE}</smartgwt>
    // knob type.
    // @see DrawItem.knobs
    // @see DrawGroup.moveItemsWithGroup
    // @include DrawItem.knobs
    //<

    //> @attr drawGroup.moveItemsWithGroup (Boolean : true : IRWA)
    // Should dragging the group (when +link{DrawItem.canDrag,canDrag} is true) or dragging the
    // move knob also move the items within this <code>DrawGroup</code>?
    // @visibility drawing
    //<
    moveItemsWithGroup: true,

    renderGroupElement:true,

    //> @attr drawGroup.useGroupRect (boolean : false : IR)
    // When should this drawGroup receive event notifications?
    // If set to <code>true</code>, the developer can specify an explicit 
    // +link{drawGroup.getGroupRect(),set of coordinates}. Whenever the user interacts with this
    // rectangle, the drawGroup will be notified and the appropriate event handlers will be
    // fired. Note that rectangle need not contain all DrawItems within the group, and 
    // is manually managed by the developer.<br>
    // If set to <code>false</code>, the +link{drawGroup.getGroupRect(),event rectangle} 
    // coordinates are unused - instead
    // as a user interacts with specific drawItems within this group, the appropriate event handler
    // would be fired on the item, then the event would "bubble" to the
    // drawGroup, firing the appropriate event handler at the group level as well.
    // @visibility drawing
    //<
    useGroupRect:false,

    //> @attr drawGroup.left (int : 0 : IRW)
    // Left coordinate of the +link{getGroupRect(),group rectangle} in pixels relative to the
    // +link{DrawPane} (the "drawing coordinate system").
    //
    // @visibility drawing
    //<
    left:0,

    //> @attr drawGroup.top (int : 0 : IRW)
    // Top coordinate of the +link{getGroupRect(),group rectangle} in pixels relative to the
    // +link{DrawPane} (the "drawing coordinate system").
    //
    // @visibility drawing
    //<
    top:0,
    
    //> @attr drawGroup.width (int : 1 : IRW)
    // Width of the +link{getGroupRect(),group rectangle} in pixels relative to the
    // +link{DrawPane} (the "drawing coordinate system").
    //
    // @visibility drawing
    //<
    width:1,

    //> @attr drawGroup.height (int : 1 : IRW)
    // Height of the +link{getGroupRect(),group rectangle} in pixels relative to the
    // +link{DrawPane} (the "drawing coordinate system").
    //
    // @visibility drawing
    //<
    height:1,

    // ---------------------------------------------------------------------------------------
    
    //> @attr drawGroup.drawItems    (Array of DrawItem : null : IR)
    // Initial list of DrawItems for this DrawGroup.
    // <P>
    // DrawItems can be added to a DrawGroup after initialization by setting
    // +link{drawItem.drawGroup}.
    //
    // @visibility drawing
    //<

init : function () {
    var initialDrawItems = this.drawItems;
    if (initialDrawItems == null) {
        this.drawItems = [];

    // ensure each drawItem knows this is its drawGroup
    } else {
        this.drawItems = [];
        for (var i = 0, numDrawItems = initialDrawItems.length; i < numDrawItems; ++i) {
            this.addDrawItem(initialDrawItems[i]);
        }
    }

    // `exemptFromGlobalTransform` is not applicable to DrawGroup.
    this.exemptFromGlobalTransform = false;

    this.Super("init", arguments);
},

_addedToDrawPane : function (thisDrawPane) {
    
    var drawItems = this.drawItems;
    for (var i = 0, len = (drawItems == null ? 0 : drawItems.length); i < len; ++i) {
        var item = drawItems[i];
        item.drawPane = thisDrawPane;
        item._addOrder = this._addOrder;
        if (item.exemptFromGlobalTransform &&
            !thisDrawPane._exemptedDrawItems.contains(item))
        {
            thisDrawPane._exemptedDrawItems.push(item);
        }

        if (isc.isA.DrawGroup(item)) {
            item._addedToDrawPane(thisDrawPane);
        }
    }
},

_removedFromDrawPane : function (oldDrawPane) {
    
    var drawItems = this.drawItems;
    for (var i = 0, len = (drawItems == null ? 0 : drawItems.length); i < len; ++i) {
        var item = drawItems[i];
        
        delete item.drawPane;
        delete item._addOrder;
        if (item.item != null) {
            oldDrawPane.quadTree.remove(item.item);
            delete item.item;
        }
        if (item.exemptFromGlobalTransform) {
            
            oldDrawPane._exemptedDrawItems.remove(item);
        }

        if (isc.isA.DrawGroup(item)) {
            item._removedFromDrawPane(oldDrawPane);
        }
    }
},

//> @method drawGroup.erase()
// Erases all DrawItems in the DrawGroup.
//
// @visibility drawing
//<
erase : function (erasingAll, willRedraw) {
    // erase grouped items first
    for (var i=0; i<this.drawItems.length; i++) {
        this.drawItems[i].erase(true, // pass erasingAll flag
                                willRedraw);
    }

    this.drawItems = [];
    // then erase this group
    this.Super("erase", arguments);
},

// drawGroup.clear()
clear : function (clearingAll, willRedraw) {
    // clear the group's items first
    var drawItems = this.drawItems,
        numDrawItems = (drawItems == null ? 0 : drawItems.length);
    for (var i = 0; i < numDrawItems; ++i) {
        var drawItem = drawItems[i];
        drawItem.clear(clearingAll, willRedraw);
    }
    // then clear this group
    this.Super("clear", arguments);
},

// drawGroup.addDrawItem()
addDrawItem : function (item, name) {
    if (!item) return;

    var oldDrawGroup = item.drawGroup;
    if (oldDrawGroup != null) {
        oldDrawGroup.removeDrawItem(item);
    } else {
        var oldDrawPane = item.drawPane;
        if (oldDrawPane != null) oldDrawPane.removeDrawItem(item);
    }

    item.drawGroup = this;

    item.getZIndex(true);

    var drawPane = this._getDrawPane();
    if (drawPane != null) {
        item.drawPane = drawPane;
        item._addOrder = this._addOrder;
    } else {
        delete item.drawPane;
        delete item._addOrder;
    }

    var drawItems = this.drawItems;
    if (drawItems == null) {
        drawItems = this.drawItems = [item];
    } else {
        
        var i = isc.Array._binarySearch(drawItems, item, isc.DrawItem._zIndexCompareFunc);
        if (i < 0) i = -i - 1;
        // Array._binarySearch() returns the lowest index where `item' could be inserted while
        // maintaining sort order by _zIndexCompareFunc. We want to insert `item' at the highest
        // such index.
        for (; i < drawItems.length && isc.DrawItem._zIndexCompareFunc(drawItems[i], item) == 0; ++i) {
            //empty
        }
        drawItems.addAt(item, i);
    }

    if (drawPane != null) {
        if (item.exemptFromGlobalTransform) {
            
            drawPane._exemptedDrawItems.add(item);
        }
        if (isc.isA.DrawGroup(item)) {
            item._addedToDrawPane(drawPane);
        }
    }

    var itemsMaster = item.masterElement;
    if (itemsMaster && itemsMaster.parentElement !== this) {
        itemsMaster.peers.remove(item);
        if (itemsMaster[name] === item) itemsMaster[name] = null;
        item.masterElement = null;
    }
    var itemPeers = item.peers;
    if (itemPeers != null) {
        for (var i = 0, numItemPeers = itemPeers.length; i < numItemPeers; ++i) {
            this.addDrawItem(itemPeers[i]);
        }
    }
},

// drawGroup.removeDrawItem()
// Removes the item from this group
removeDrawItem : function (item) {
    

    // If the item is drawn, then DrawItem.erase() will take care of removing the item from
    // this DrawGroup's drawItems array.
    if (item._drawn) {
        item.erase();

    } else {
        var wasRemoved = this.drawItems.remove(item);
        if (!wasRemoved && this._eventOnlyDrawItems != null) {
            wasRemoved = this._eventOnlyDrawItems.remove(item);
        }

        if (isc.isA.DrawGroup(item)) {
            var drawPane = this._getDrawPane();
            if (drawPane != null) {
                item._removedFromDrawPane(drawPane);
            }
        } else if (item.exemptFromGlobalTransform) {
            var drawPane = this._getDrawPane();
            
            drawPane._exemptedDrawItems.remove(item);
        }
    }
    delete item.drawGroup;
    delete item.drawPane;
},

//> @attr drawGroup.showGroupRectOutline (Boolean : null : IRW)
// If the <smartclient>"move"</smartclient>
// <smartgwt>{@link com.smartgwt.client.types.KnobType#MOVE}</smartgwt>
// +link{drawItem.knobs,control knob} is shown for this group and +link{DrawGroup.useGroupRect,useGroupRect}
// is true, should the +link{DrawGroup.groupRectOutline} be shown?
// <p>
// Set to false to disable showing the <code>groupRectOutline</code>.
// @visibility drawing
//<
//showGroupRectOutline: null,

setShowGroupRectOutline : function (showGroupRectOutline) {
    var wasEnabled = this.showGroupRectOutline != false,
        isEnabled = showGroupRectOutline != false;
    this.showGroupRectOutline = showGroupRectOutline;

    if (wasEnabled == isEnabled) return;

    if (this.knobs == null || !this.knobs.contains("move")) return;

    if (isEnabled) {
        this.hideKnobs("move");
        this.showKnobs("move");

    } else {
        var groupRectOutline = this.groupRectOutline;
        if (groupRectOutline == null || groupRectOutline.destroyed) return;
        groupRectOutline.destroy();
        this.groupRectOutline = null;
    }
},

//> @attr drawGroup.groupRectOutline (AutoChild DrawRect : null : IR)
// If this group is showing a <smartclient>"move"</smartclient>
// <smartgwt>{@link com.smartgwt.client.types.KnobType#MOVE}</smartgwt>
// +link{drawItem.knobs,control knob}, the <code>groupRectOutline</code> is a +link{DrawRect} AutoChild
// that identifies the group's group rect (see +link{DrawGroup.useGroupRect}).
// <p>
// <code>useGroupRect</code> must be true and the <smartclient>"move"</smartclient>
// <smartgwt>{@link com.smartgwt.client.types.KnobType#MOVE}</smartgwt> control knob must be
// showing for the <code>groupRectOutline</code> AutoChild to be created and shown.
// @visibility drawing
//<
groupRectOutlineDefaults: {
    _constructor: "DrawRect",
    autoDraw: true,
    lineWidth: 1,
    lineColor: "#cbe1ca",
    linePattern: "solid",
    _internal:true
},

// drawGroup.showMoveKnobs()
showMoveKnobs : function () {
    if (this.useGroupRect && this.showGroupRectOutline != false &&
        (this.groupRectOutline == null || this.groupRectOutline.destroyed))
    {
        var groupRectOutline = this.groupRectOutline = this.createAutoChild("groupRectOutline", {
            left: this.left,
            top: this.top,
            width: this.width,
            height: this.height,
            drawPane: this.drawPane,
            excludeFromQuadTree: true,
            knobs: null,
            title: null,
            zIndex: this.getZIndex(true),
            _sameZIndexAsMaster: true
        });
        this.addPeer(groupRectOutline);
    }

    this.Super("showMoveKnobs", arguments);
},

// drawGroup.hideMoveKnobs()
hideMoveKnobs : function () {
    this.Super("hideMoveKnobs", arguments);
    if (this.groupRectOutline != null) {
        this.groupRectOutline.destroy();
        this.groupRectOutline = null;
    }
},

showResizeKnobs : null,
hideResizeKnobs : null,

// drawGroup.updateControlKnobs()
updateControlKnobs : function () {
    this.Super("updateControlKnobs", arguments);
    if (this.groupRectOutline != null) {
        this.groupRectOutline.setRect(this.left, this.top, this.width, this.height);
    }
},



_vmlGroupConfigAdapter: new isc.VMLRenderer.shapeConfig(),
_renderVML : function (vmlRenderer, id, conversionContext) {
    var buffer = vmlRenderer.buffer,
        config = this._vmlGroupConfigAdapter;
    if (this.renderGroupElement) this._vmlRendererHandle = vmlRenderer.group(id, config);

    var drawItems = this.drawItems;
    if (drawItems != null) {
        for (var i = 0, numDrawItems = drawItems.length; i < numDrawItems; ++i) {
            var drawItem = drawItems[i];
            drawItem._getElementVML(
                buffer, "isc_DrawItem_" + drawItem.drawItemID, conversionContext);
        }
    }

    if (this.renderGroupElement) vmlRenderer.endGroup(id);
},

getSvgString : function (conversionContext) {
    conversionContext = conversionContext || isc.SVGStringConversionContext.create();
    var svgString = "";
    if (this.renderGroupElement) {
        svgString += "<g";
        var svgHandleID = this._getSvgHandleID();
        if (svgHandleID != null) svgString += " id='" + svgHandleID + "'";
        if (!conversionContext.printForExport) {
            svgString += " " + this._getEventProxyAttribute();
        }
        var attributesSVG = this.getAttributesSVG();
        if (attributesSVG) svgString += " " + attributesSVG;
        svgString += ">";
    }
    var drawItems = this.drawItems,
        numDrawItems = (drawItems == null ? 0 : drawItems.length);
    if (numDrawItems > 0) {
        for (var i = 0; i < numDrawItems; ++i) {
            svgString += drawItems[i].getSvgString(conversionContext);
        }
    }
    if (this.renderGroupElement) svgString += "</g>";
    return svgString;
},



// drawGroup._completeDraw()
_completeDraw : function () {
    
    var drawItems = this.drawItems,
        numDrawItems = (drawItems == null ? 0 : drawItems.length);
    for (var i = 0; i < numDrawItems; ++i) {
        var drawItem = drawItems[i];
        
        drawItem._completeDraw();
    }
    this.Super("_completeDraw", arguments);
},


drawBitmap : function (context) {
    // Same loop as in DrawPane.redrawBitmapNow().
    this.drawPane._drawBitmapDrawItems(context, this.drawItems);
},


_setLineWidthVML : isc.Class.NO_OP,


//--------------------------------------------------------------------------------
//  position/translation
//--------------------------------------------------------------------------------

//> @method drawGroup.setLeft()
// Sets the left coordinate of this <code>DrawGroup</code>'s +link{DrawGroup.getGroupRect(),group rectangle}.
// Note that setting the left coordinate will not move the items in this <code>DrawGroup</code>.
//
// @param left (int) new left coordinate
// @visibility drawing
//<
setLeft : function (left) {
    this._setGroupRect(left, this.top, this.width, this.height);
},

//> @method drawGroup.setTop()
// Sets the top coordinate of this <code>DrawGroup</code>'s +link{DrawGroup.getGroupRect(),group rectangle}.
// Note that setting the top coordinate will not move the items in this <code>DrawGroup</code>.
//
// @param top (int) new top coordinate in pixels
// @visibility drawing
//<
setTop : function (top) {
    this._setGroupRect(this.left, top, this.width, this.height);
},

//> @method drawGroup.setWidth()
// Sets the width of this <code>DrawGroup</code>'s +link{DrawGroup.getGroupRect(),group rectangle}.
// Note that setting the width will not move or resize the items in this <code>DrawGroup</code>.
//
// @param width (int) new width for the group rectangle
// @visibility drawing
//<
setWidth : function (width) {
    this._setGroupRect(this.left, this.top, width, this.height);
},

//> @method drawGroup.setHeight()
// Sets the height of this <code>DrawGroup</code>'s +link{DrawGroup.getGroupRect(),group rectangle}.
// Note that setting the height will not move or resize the items in this <code>DrawGroup</code>.
//
// @param height (int) new height for the group rectangle
// @visibility drawing
//<
setHeight : function (height) {
    this._setGroupRect(this.left, this.top, this.width, height);
},

//> @method drawGroup.getGroupRect()
// This method will return an array of integers (left, top, width, height) defining the area
// of the "group rectangle" for the group. If +link{useGroupRect,useGroupRect} is true, this is
// the area of the DrawPane where user interactions will fire event notifications on this DrawGroup.
// <P>
// This is a convienence method to get the current coordinates of the
// +link{drawGroup.useGroupRect,group rectangle}.  Developers must use
// +link{drawGroup.setLeft()}, +link{drawGroup.setTop()}, +link{drawGroup.setWidth()} or
// +link{drawGroup.setHeight()} to set each coordinate directly.
// @return (Array of int) 4 element array containing left, top, width, height of the group rectangle.
// @visibility drawing
//<
getGroupRect : function () {
    return [this.left, this.top, this.width, this.height];
},

_setGroupRect : function (left, top, width, height) {
    var center0 = this._getRotationCenter(),
        cx0 = center0.cx,
        cy0 = center0.cy;

    this.left = left;
    this.top = top;
    this.width = width;
    this.height = height;

    var center = this._getRotationCenter();
    this._updateRotationCenter(cx0, cy0, center.cx, center.cy);

    this._reshaped();
},

// drawGroup._moveTo()
_moveTo : function (left, top, cx0, cy0) {
    if (left == null) left = this.left;
    if (top == null) top = this.top;

    

    var oldLeft = this.left,
        oldTop = this.top;

    this.left = left;
    this.top = top;

    if (this.moveItemsWithGroup) {
        var drawItems = this.drawItems,
            dX = left - oldLeft,
            dY = top - oldTop;
        for (var i = 0, numItems = drawItems == null ? 0 : drawItems.length; i < numItems; ++i) {
            drawItems[i].moveBy(dX, dY);
        }
    }

    this._reshaped();
},

// drawGroup._updateLocalTransform()
_updateLocalTransform : function (transform, cx, cy, initialShape, fireReshaped, viaLocalTransformOnly) {
    if (initialShape == null) {
        initialShape = this;
    }
    if (!viaLocalTransformOnly) {
        var info = isc.DrawItem._fitBestRect(
                transform, cx, cy,
                initialShape.left, initialShape.top, initialShape.width, initialShape.height,
                0);
        if (info.success) {
            this.Super("_updateLocalTransform", [transform, cx, cy, initialShape, false, true], arguments);
            this._moveTo(isc.DrawItem._makeCoordinate(info.left),
                         isc.DrawItem._makeCoordinate(info.top),
                         isc.DrawItem._makeCoordinate(cx),
                         isc.DrawItem._makeCoordinate(cy));
            return;
        }
    }
    this.Super("_updateLocalTransform", [transform, cx, cy, initialShape, fireReshaped, true], arguments);
},

//> @method drawGroup.moveTo()
// Sets both the left and top coordinates of this <code>DrawGroup</code>'s +link{drawGroup.getGroupRect(),group rectangle}.
// Note that this does not move or resize the items in this <code>DrawGroup</code>.
//
// @param left (integer) new left coordinate in pixels
// @param top (integer) new top coordinate in pixels
// @visibility drawing
//<

//> @method drawGroup.moveBy()
// Updates the <code>DrawGroup</code>'s left coordinate by <code>dX</code> and the top coordinate
// by <code>dY</code>. Note that this does not move or resize the items in this <code>DrawGroup</code>.
//
// @param dX (int) change to left coordinate in pixels
// @param dY (int) change to top coordinate in pixels
// @visibility drawing
//<

//> @method drawGroup.rotateTo()
// Rotate each item in the group to the specified number of degrees.
//
// @param degrees (float)
// @visibility drawing
//<
rotateTo : function (degrees) {
    for (var i = 0; i < this.drawItems.length; i++) {
        var drawItem = this.drawItems[i];
        drawItem.rotateTo(degrees);
    }
},

//> @method drawGroup.rotateBy()
// Rotate each item in the group by the specified number of degrees.
//
// @param degrees (float)
// @visibility drawing
//<
rotateBy : function (degrees) {
    for(var i = 0; i < this.drawItems.length; ++i) {
        var drawItem = this.drawItems[i];
        drawItem.rotateBy(degrees);
    }
},

//> @method drawGroup.scaleBy()
// Scale all drawItem[] shapes by the x, y multipliers
// @param x (float) scale in the x direction
// @param y (float) scale in the y direction
// @visibility drawing
//<
scaleBy : function (x, y) {
    for(var i = 0; i < this.drawItems.length; ++i) {
        var drawItem = this.drawItems[i];
        drawItem.scaleBy(x,y);
    }
},

//> @method drawGroup.getCenter()
// Get the center point of the +link{DrawGroup.getGroupRect(),group rectangle}.
// @return (Point) the center point
// @visibility drawing
//<
getCenter : function () {
    return [this.left + isc.DrawItem._makeCoordinate(this.width / 2), this.top + isc.DrawItem._makeCoordinate(this.height / 2)];
},

//> @method drawGroup.getBoundingBox()
// Returns the left, top, (left + width), and (top + height) values
// @return (Array[] of double) x1, y1, x2, y2 coordinates
// @visibility drawing
//<
getBoundingBox : function (includeStroke, outputBox) {
    
    var box = (outputBox || new Array(4));
    box[0] = this.left;
    box[1] = this.top;
    box[2] = this.left + this.width;
    box[3] = this.top + this.height;
    return box;
},

isPointInPath : isc.DrawItem.getInstanceProperty("isInBounds"),

//> @method drawGroup.scaleTo()
// Scale the each item in the drawGroup by the x, y multipliers
// @param x (float) scale in the x direction
// @param y (float) scale in the y direction
// @visibility drawing
//<
scaleTo : function (x, y) {
    this.scale = this.scale || [];
    this.scale[0] += (this.scale[0]||0) + x;
    this.scale[1] += (this.scale[1]||0) + y;
    this._transform = null;
    this.scaleBy(x, y);
}


// shape events

//> @method drawGroup.dragStart()
// Notification fired when the user starts to drag this DrawGroup. Will only fire if +link{DrawItem.canDrag,canDrag}
// is true for this group.
// <P>
// Note that if +link{useGroupRect,useGroupRect} is true, this notification will be triggered
// by the user interacting with the specified +link{getGroupRect(),group rectangle} for the group.
// If +link{useGroupRect,useGroupRect} is false, the notification will bubble up from interactions
// with individual items within the group.
// <P>
// Default drag behavior will be to reposition all items in the group (and update the group
// rectangle).
// @include drawItem.dragStart()
// @visibility drawing
//<


//> @method drawGroup.dragMove()
// Notification fired for every mouseMove event triggered while the user is dragging this
// DrawGroup. Will only fire if +link{DrawItem.canDrag,canDrag} is true for this group.
// <P>
// Note that if +link{useGroupRect,useGroupRect} is true, this notification will be triggered
// by the user interacting with the specified +link{getGroupRect(),group rectangle} for the group.
// If +link{useGroupRect,useGroupRect} is false, the notification will bubble up from interactions
// with individual items within the group.
// <P>
// Default drag behavior will be to reposition all items in the group (and update the group rectangle).
// @include drawItem.dragMove()
// @visibility drawing
//<

//> @method drawGroup.dragStop()
// Notification fired when the user stops dragging this DrawGroup. Will only fire if +link{DrawItem.canDrag,canDrag}
// is true for this group.
// <P>
// Note that if +link{useGroupRect,useGroupRect} is true, this notification will be triggered
// by the user interacting with the specified +link{getGroupRect(),group rectangle} for the group.
// If +link{useGroupRect,useGroupRect} is false, the notification will bubble up from interactions
// with individual items within the group.
// @include drawItem.dragStop()
// @visibility drawing
//<

//> @method drawGroup.mouseDown()
// Notification fired when the user presses the left mouse button on this DrawGroup.
// <p>
// Note that if +link{useGroupRect,useGroupRect} is true, this notification will be triggered
// by the user interacting with the specified +link{getGroupRect(),group rectangle} for the group.
// If +link{useGroupRect,useGroupRect} is false, the notification will bubble up from interactions
// with individual items within the group.
// @include drawItem.mouseDown()
// @visibility drawing
//<

//> @method drawGroup.mouseUp()
// Notification fired when the user releases the left mouse button on this DrawGroup.
// <p>
// Note that if +link{useGroupRect,useGroupRect} is true, this notification will be triggered
// by the user interacting with the specified +link{getGroupRect(),group rectangle} for the group.
// If +link{useGroupRect,useGroupRect} is false, the notification will bubble up from interactions
// with individual items within the group.
// @include drawItem.mouseUp()
// @visibility drawing
//<

//> @method drawGroup.click()
// Notification fired when the user clicks on this DrawGroup.
// <p>
// Note that if +link{useGroupRect,useGroupRect} is true, this notification will be triggered
// by the user interacting with the specified +link{getGroupRect(),group rectangle} for the group.
// If +link{useGroupRect,useGroupRect} is false, the notification will bubble up from interactions
// with individual items within the group.
// @include drawItem.click()
// @visibility drawing
//<

//> @method drawGroup.mouseOver()
// Notification fired when the mouse enters this DrawGroup.
// <p>
// Note that if +link{useGroupRect,useGroupRect} is true, this notification will be triggered
// by the user interacting with the specified +link{getGroupRect(),group rectangle} for the group.
// If +link{useGroupRect,useGroupRect} is false, the notification will bubble up from interactions
// with individual items within the group.
// @include drawItem.mouseOver()
// @visibility drawing
//<

//> @method drawGroup.mouseMove()
// Notification fired when the user moves the mouse over this DrawGroup.
// <p>
// Note that if +link{useGroupRect,useGroupRect} is true, this notification will be triggered
// by the user interacting with the specified +link{getGroupRect(),group rectangle} for the group.
// If +link{useGroupRect,useGroupRect} is false, the notification will bubble up from interactions
// with individual items within the group.
// @include drawItem.mouseMove()
// @visibility drawing
//<

//> @method drawGroup.mouseOut()
// Notification fired when the mouse leaves this DrawGroup.
// <p>
// Note that if +link{useGroupRect,useGroupRect} is true, this notification will be triggered
// by the user interacting with the specified +link{getGroupRect(),group rectangle} for the group.
// If +link{useGroupRect,useGroupRect} is false, the notification will bubble up from interactions
// with individual items within the group.
// @include drawItem.mouseOut()
// @visibility drawing
//<

// end of shape events

}); // end DrawGroup.addProperties


isc.DrawGroup.markUnsupportedMethods(null, ["setLineWidth", "setLineColor", "setLineOpacity",
        "setLinePattern", "setLineCap", "setShadow", "setFillColor", "setFillGradient", "setFillOpacity"]);
isc.DrawGroup.markUnsupportedMethods(null, ["setStartArrow", "setEndArrow"]);








//------------------------------------------------------------------------------------------
//> @class DrawLine
//
//  DrawItem subclass to render line segments.
//
//  @inheritsFrom DrawItem
//  @treeLocation Client Reference/Drawing/DrawItem
//  @visibility drawing
//<
//------------------------------------------------------------------------------------------

//> @object Point
// X/Y position in pixels, specified as an Array with two members, for example: [30, 50]
//
// @treeLocation Client Reference/Drawing
// @visibility drawing
//<
//> @attr point.x (int: 0: IR) 
// The x coordinate of this point.
//
// @visibility drawing
//<
//> @attr point.y (int: 0: IR) 
// The y coordinate of this point.
//
// @visibility drawing
//<

isc.defineClass("DrawLine", "DrawItem").addProperties({
    //> @attr drawLine.knobs
    // Array of control knobs to display for this item. Each +link{knobType} specified in this
    // will turn on UI element(s) allowing the user to manipulate this DrawLine.  To update the
    // set of knobs at runtime use +link{drawItem.showKnobs()} and +link{drawItem.hideKnobs()}.
    // <p>
    // DrawLine supports the
    // <smartclient>"startPoint", "endPoint", and "move"</smartclient>
    // <smartgwt>{@link com.smartgwt.client.types.KnobType#STARTPOINT}, {@link com.smartgwt.client.types.KnobType#ENDPOINT},
    // and {@link com.smartgwt.client.types.KnobType#MOVE}</smartgwt>
    // knob types.
    // @include DrawItem.knobs
    //<

    //> @attr drawLine.titleRotationMode (TitleRotationMode : "withLineAlwaysUp" : IRA)
    // @include DrawItem.titleRotationMode
    //<
    titleRotationMode: "withLineAlwaysUp",

    //> @attr drawLine.showTitleLabelBackground (boolean : true : IRA)
    // @include DrawItem.showTitleLabelBackground
    //<
    showTitleLabelBackground: true,

    //> @attr drawLine.startPoint     (Point : [0,0] : IRW)
    // Start point of the line
    // 
    // @visibility drawing
    //<
    startPoint: [0,0],

    //> @attr drawLine.endPoint       (Point : [100,100] : IRW)
    // End point of the line
    // 
    // @visibility drawing
    //<
    endPoint: [100,100],

    //> @attr drawLine.startLeft      (int : 0 : IR)
    // Starting left coordinate of the line.  Overrides left coordinate of +link{startPoint} if
    // both are set.
    // 
    // @visibility drawing
    //<
//    startLeft:0,

    //> @attr drawLine.startTop       (int : 0 : IR)
    // Starting top coordinate of the line.  Overrides top coordinate of +link{startPoint} if
    // both are set.
    // 
    // @visibility drawing
    //<
//    startTop:0,

    //> @attr drawLine.endLeft        (int : 100 : IR)
    // Ending left coordinate of the line.  Overrides left coordinate of +link{endPoint} if
    // both are set.
    // 
    // @visibility drawing
    //<
//    endLeft:100,

    //> @attr drawLine.endTop         (int : 100 : IR)
    // Ending top coordinate of the line.  Overrides top coordinate of +link{endPoint} if
    // both are set.
    // 
    // @visibility drawing
    //<
//    endTop:100,

    
    svgElementName: isc.Browser.isOpera ? "polyline" : "line",

init : function () {
    this.startPoint = this.startPoint.duplicate();
    this.endPoint = this.endPoint.duplicate();

    this.startPoint[0] = this.startLeft = (this.startLeft == 0 ? 0 : (this.startLeft || this.startPoint[0]));
    this.startPoint[1] = this.startTop = (this.startTop == 0 ? 0 : (this.startTop || this.startPoint[1]));
    this.endPoint[0] = this.endLeft = (this.endLeft == 0 ? 0 : (this.endLeft || this.endPoint[0]));
    this.endPoint[1] = this.endTop = (this.endTop == 0 ? 0 : (this.endTop || this.endPoint[1]));
    this.Super("init", arguments);
},


//----------------------------------------
// SVG arrowhead dependencies
//----------------------------------------

// changing the line width or arrows will affect the size
// of the arrows, so call setStartPoint and setEndPoint to update the size of the line segment
// (setStartPoint will take care of the endPoint as well if necessary)
setLineWidth : function (width) {
    this.Super("setLineWidth", arguments);
    if (this.drawingSVG) this.setStartPoint(this.startPoint);
},
setStartArrow : function (startArrow) {
    this.Super("setStartArrow", arguments);
    if (this.drawingSVG) this.setStartPoint(this.startPoint);
},
setEndArrow : function (endArrow) {
    this.Super("setEndArrow", arguments);
    // NB: cannot use setStartPoint here, since we might be clearing endArrow
    if (this.drawingSVG) this.setEndPoint(this.endPoint);
},


//----------------------------------------
//  DrawLine renderers
//----------------------------------------

_vmlLineConfigAdapter: new isc.VMLRenderer.shapeConfig(),
_renderVML : function (vmlRenderer, id, conversionContext) {
    var config = this._vmlLineConfigAdapter;
    isc.VMLRenderer.drawItemToShapeConfig(this, config);
    config.fromX = this.startPoint[0];
    config.fromY = this.startPoint[1];
    config.toX = this.endPoint[0];
    config.toY = this.endPoint[1];
    this._vmlRendererHandle = vmlRenderer.line(id, config);
},

// drawLine.getAttributesSVG()
getAttributesSVG : function () {
    if (this.svgElementName === "polyline") {
        return ("points='" + this.startLeft + "," + this.startTop + " " +
                             this.endLeft + "," + this.endTop + "'");
    } else {
        return ("x1='" + this.startLeft +
                "' y1='" + this.startTop +
                "' x2='" + this.endLeft +
                "' y2='" + this.endTop +
                "'");
    }
},

// drawLine.drawBitmapPath()
drawBitmapPath : function (context) {
    
    var offset = (this.lineWidth == 0 || parseInt(this.lineWidth) % 2) == 1 ? 0.5 : 0,
        startLeft = parseFloat(this.startLeft)+offset,
        startTop = parseFloat(this.startTop)+offset,
        endLeft = parseFloat(this.endLeft)+offset,
        endTop = parseFloat(this.endTop)+offset,
        angle,
        arrowDelta = 10,
        originX,
        originY;
    if (this.startArrow == "open") {
        context.save();
        context.beginPath();
        context.strokeStyle = this.lineColor;
        context.lineWidth = this.lineWidth;
        context.lineCap = this.lineCap;
        angle = this.computeAngle(startLeft,startTop,endLeft,endTop);
        originX = startLeft;
        originY = startTop;
        context.scale(1,1);
        context.translate(originX, originY);
        context.rotate(angle*this._radPerDeg);
        this.bmMoveTo(arrowDelta,-arrowDelta, context);
        this.bmLineTo(0,0, context);
        this.bmLineTo(arrowDelta,arrowDelta, context);
        context.stroke();
        context.restore();
    } else if (this.startArrow == "block") {
        context.save();
        context.beginPath();
        context.fillStyle = this.lineColor;
        context.lineWidth = this.lineWidth;
        context.lineCap = this.lineCap;
        angle = this.computeAngle(startLeft,startTop,endLeft,endTop);
        originX = startLeft;
        originY = startTop;
        context.translate(originX, originY);
        context.rotate(angle*this._radPerDeg);
        this.bmMoveTo(arrowDelta,-arrowDelta, context);
        this.bmLineTo(0,0, context);
        this.bmLineTo(arrowDelta,arrowDelta, context);
        context.closePath();
        context.fill();
        context.stroke();
        context.restore();
    }
    if (this.endArrow == "open") {
        context.save();
        context.beginPath();
        context.strokeStyle = this.lineColor;
        context.lineWidth = this.lineWidth;
        context.lineCap = this.lineCap;
        angle = this.computeAngle(startLeft,startTop,endLeft,endTop);
        originX = endLeft;
        originY = endTop;
        context.translate(originX, originY);
        context.rotate(angle*this._radPerDeg);
        this.bmMoveTo(-arrowDelta,arrowDelta, context);
        this.bmLineTo(0,0, context);
        this.bmLineTo(-arrowDelta,-arrowDelta, context);
        context.stroke();
        context.restore();
    } else if (this.endArrow == "block") {
        context.save();
        context.beginPath();
        context.fillStyle = this.lineColor;
        context.lineWidth = this.lineWidth;
        context.lineCap = this.lineCap;
        angle = this.computeAngle(startLeft,startTop,endLeft,endTop);
        originX = endLeft;
        originY = endTop;
        context.translate(originX, originY);
        context.rotate(angle*this._radPerDeg);
        this.bmMoveTo(-arrowDelta,arrowDelta, context);
        this.bmLineTo(0,0, context);
        this.bmLineTo(-arrowDelta,-arrowDelta, context);
        context.closePath();
        context.fill();
        context.restore();
    }
    if(this.linePattern.toLowerCase() !== "solid") {
        this._drawLinePattern(startLeft, startTop, endLeft, endTop, context);
    } else {
        this.bmMoveTo(this.startLeft, this.startTop, context);
        this.bmLineTo(this.endLeft, this.endTop, context);
    }
}, 

//----------------------------------------
//  DrawLine attribute setters
//----------------------------------------

// NOTE: setStartPoint/EndPoint doc is @included, use generic phrasing

//> @method drawLine.setStartPoint()
// Update the startPoint
//
// @param left (int) left coordinate for start point, in pixels
// @param top (int) top coordinate for start point, in pixels
// @visibility drawing
//< 
setStartPoint : function (left, top, fireMovedAndResized) {
    if (isc.isAn.Array(left)) { // conversion needed for SGWT
        top = left[1];
        left = left[0];
    }

    

    this._setStartAndEndPoints(
        left, top, this.endLeft, this.endTop, null, null, fireMovedAndResized);
},

//> @method drawLine.setEndPoint()
// Update the endPoint
//
// @param left (int) left coordinate for end point, in pixels
// @param top (int) top coordinate for end point, in pixels
// @visibility drawing
//< 
setEndPoint : function (left, top, fireMovedAndResized) {
    if (isc.isAn.Array(left)) { // conversion needed for SGWT
        top = left[1];
        left = left[0];
    }

    

    this._setStartAndEndPoints(
        this.startLeft, this.startTop, left, top, null, null, fireMovedAndResized);
},

_setStartAndEndPoints : function (startLeft, startTop, endLeft, endTop, cx0, cy0, fireMovedAndResized) {
    
    var startPointChanged = (this.startLeft != startLeft || this.startTop != startTop),
        endPointChanged = (this.endLeft != endLeft || this.endTop != endTop);
    if (!(startPointChanged || endPointChanged)) {
        return;
    }
    if (cx0 == null || cy0 == null) {
        var center0 = this._getRotationCenter();
        cx0 = center0.cx;
        cy0 = center0.cy;
    }
    var rbboxLeft0 = 0, rbboxTop0 = 0, rbboxWidth0 = 0, rbboxHeight0 = 0;
    if (fireMovedAndResized) {
        var rbbox0 = this.getResizeBoundingBox(this._tempBoundingBox);
        rbboxLeft0 = isc.DrawItem._makeCoordinate(rbbox0[0]);
        rbboxTop0 = isc.DrawItem._makeCoordinate(rbbox0[1]);
        rbboxWidth0 = isc.DrawItem._makeCoordinate(rbbox0[2] - rbbox0[0]);
        rbboxHeight0 = isc.DrawItem._makeCoordinate(rbbox0[3] - rbbox0[1]);
    }

    this.startLeft = startLeft;
    this.startTop = startTop
    this.endLeft = endLeft;
    this.endTop = endTop;
    if (this.startPoint) {
        this.startPoint[0] = startLeft;
        this.startPoint[1] = startTop;
    }
    if (this.endPoint) {
        this.endPoint[0] = endLeft;
        this.endPoint[1] = endTop;
    }

    var center = this._getRotationCenter();
    this._updateRotationCenter(cx0, cy0, center.cx, center.cy);

    if (this.drawingVML) {
        var vmlRendererHandle = this._vmlRendererHandle;
        if (cx0 != center.cx || cy0 != center.cy) {
            this._transform = null;
            vmlRendererHandle.setProperty("transform", this._getLocalTransform());
        }
        if (startPointChanged) {
            vmlRendererHandle.setProperty("fromX", startLeft).setProperty("fromY", startTop);
        }
        if (endPointChanged) {
            vmlRendererHandle.setProperty("toX", endLeft).setProperty("toY", endTop);
        }
        vmlRendererHandle.flush();

    } else if (this.drawingSVG) {
        var svgHandle = this._svgHandle;
        if (cx0 != center.cx || cy0 != center.cy) {
            this._transform = null;
            var t = this._getLocalTransform();
            svgHandle.setAttributeNS(null, "transform", "matrix(" + t.m00 + " " + t.m10 + " " +
                                                                    t.m01 + " " + t.m11 + " " +
                                                                    t.m02 + " " + t.m12 + ")");
        }
        if (this.svgElementName === "polyline") {
            svgHandle.setAttributeNS(null, "points", startLeft + "," + startTop + " " +
                                                     endLeft + "," + endTop);
        } else {
            if (startPointChanged ||
                // Changing endPoint also changes the angle of the startArrow.
                (endPointChanged && this.startArrow))
            {
                svgHandle.setAttributeNS(null, "x1", startLeft);
                svgHandle.setAttributeNS(null, "y1", startTop);
            }
            if (endPointChanged ||
                // Changing startPoint also changes the angle of the endArrow.
                (startPointChanged && this.endArrow))
            {
                svgHandle.setAttributeNS(null, "x2", endLeft);
                svgHandle.setAttributeNS(null, "y2", endTop);
            }
        }

    } else if (this.drawingBitmap) {
        this.drawPane.redrawBitmap();
    }
    this._reshaped();

    if (fireMovedAndResized) {
        var rbbox = this.getResizeBoundingBox(this._tempBoundingBox),
            deltaX = isc.DrawItem._makeCoordinate(rbbox[0]) - rbboxLeft0,
            deltaY = isc.DrawItem._makeCoordinate(rbbox[1]) - rbboxTop0,
            moved = (deltaX != 0 || deltaY != 0),
            deltaWidth = isc.DrawItem._makeCoordinate(rbbox[2] - rbbox[0]) - rbboxWidth0,
            deltaHeight = isc.DrawItem._makeCoordinate(rbbox[3] - rbbox[1]) - rbboxHeight0,
            resized = (deltaWidth != 0 || deltaHeight != 0);
        if (moved) {
            this.moved(deltaX, deltaY);
        }
        if (resized) {
            this.resized();
        }
    }
},


_updateLocalTransform : function (transform, cx, cy, initialShape, fireReshaped, viaLocalTransformOnly) {
    
    var points = this.points;

    this.points = [[this.startLeft, this.startTop], [this.endLeft, this.endTop]];
    this._convexHull = [0, 1];
    isc.DrawPath.getInstanceProperty("_updateLocalTransform").apply(this, arguments);
    delete this._convexHull;
    delete this._left;
    delete this._top;
    delete this._right;
    delete this._bottom;
    delete this._centerX;
    delete this._centerY;

    this.points = points;
},


_redrawAfterSetPoints : function (updateVMLRendererHandleTransform) {
    this._setStartAndEndPoints(
        isc.DrawItem._makeCoordinate(this.points[0][0]),
        isc.DrawItem._makeCoordinate(this.points[0][1]),
        isc.DrawItem._makeCoordinate(this.points[1][0]),
        isc.DrawItem._makeCoordinate(this.points[1][1]),
        null, null, false);
},


_saveShape : function () {
    var points = this.points;
    this.points = [[this.startLeft, this.startTop], [this.endLeft, this.endTop]];
    this._centerX = (this.startLeft + this.endLeft) / 2;
    this._centerY = (this.startTop + this.endTop) / 2;
    var ret = isc.DrawPath.getInstanceProperty("_saveShape").apply(this, arguments);
    delete this._centerX;
    delete this._centerY;
    this.points = points;
    return ret;
},

//> @method drawLine.getBoundingBox()
// Returns a bounding box for the <code>DrawLine</code>, taking into account the
// +link{DrawItem.lineWidth,lineWidth}.
//
// @return (Array[] of double) x1, y1, x2, y2 coordinates
// @visibility drawing
//<
getBoundingBox : function (includeStroke, outputBox) {
    
    var x1 = this.startPoint[0],
        y1 = this.startPoint[1],
        x2 = this.endPoint[0],
        y2 = this.endPoint[1],
        dY = y2 - y1,
        dX = x2 - x1;

    // Calculate the angle and length of the line segment. We model the line as a filled rectangle
    // from (-halfLineWidth, -halfLineWidth) to (l + halfLineWidth, halfLineWidth) that is rotated
    // about (0, 0) by `a' and then translated to (x1, y1). From this perspective, all we're doing
    // is a _getTransformedBoundingBox() calculation.

    var a = Math.atan2(dY, dX) / isc.Math._radPerDeg,
        l = isc.Math._hypot(dX, dY),
        bbox = (outputBox || new Array(4));

    if (includeStroke != true || !this._hasStroke()) {
        bbox[0] = bbox[1] = bbox[3] = 0;
        bbox[2] = l;
    } else {
        var halfLineWidth = this.lineWidth / 2;
        bbox[0] = bbox[1] = -halfLineWidth;
        bbox[2] = l + halfLineWidth;
        bbox[3] = halfLineWidth;
    }

    var t = isc.AffineTransform.getTranslateTransform(x1, y1)
                               .rotate(a, 0, 0);

    var v1 = t.transform(bbox[0], bbox[1]),
        v2 = t.transform(bbox[2], bbox[1]),
        v3 = t.transform(bbox[2], bbox[3]),
        v4 = t.transform(bbox[0], bbox[3]);

    bbox[0] = Math.min(v1[0], v2[0], v3[0], v4[0]);
    bbox[1] = Math.min(v1[1], v2[1], v3[1], v4[1]);
    bbox[2] = Math.max(v1[0], v2[0], v3[0], v4[0]);
    bbox[3] = Math.max(v1[1], v2[1], v3[1], v4[1]);
    return bbox;
},

_getBoundingBoxOfTransformedShape : function (
    transform, includeStroke, includeHitTolerance, outputBox)
{
    
    var box = (outputBox || new Array(4));

    // Adjust the bounding box by the lineWidth and/or hitTolerance.
    var halfLineWidth = (includeStroke && this._hasStroke() ? (this.lineWidth / 2) : 0),
        hitTolerance = (includeHitTolerance ? this.hitTolerance : 0),
        radius = halfLineWidth + hitTolerance;

    var m00 = transform.m00, m01 = transform.m01, m02 = transform.m02,
        m10 = transform.m10, m11 = transform.m11, m12 = transform.m12;

    var x1 = m00 * this.startPoint[0] + m01 * this.startPoint[1] + m02,
        y1 = m10 * this.startPoint[0] + m11 * this.startPoint[1] + m12,
        x2 = m00 * this.endPoint[0] + m01 * this.endPoint[1] + m02,
        y2 = m10 * this.endPoint[0] + m11 * this.endPoint[1] + m12;

    box[0] = Math.min(x1, x2);
    box[1] = Math.min(y1, y2);
    box[2] = Math.max(x1, x2);
    box[3] = Math.max(y1, y2);
    if (radius != 0) {
        var halfOffsetWidth = radius * isc.Math._hypot(m00, m01),
            halfOffsetHeight = radius * isc.Math._hypot(m10, m11);

        box[0] -= halfOffsetWidth;
        box[1] -= halfOffsetHeight;
        box[2] += halfOffsetWidth;
        box[3] += halfOffsetHeight;
    }
    return box;
},

//> @method drawLine.getCenter()
// Get the midpoint of the line.
// @return (Point) the midpoint
// @visibility drawing
//<
getCenter : function () {
    return [this.startPoint[0] + isc.DrawItem._makeCoordinate((this.endPoint[0]-this.startPoint[0])/2), 
      this.startPoint[1] + isc.DrawItem._makeCoordinate((this.endPoint[1]-this.startPoint[1])/2)];
},

//> @method drawLine.isPointInPath()
// @include DrawItem.isPointInPath()
//<
isPointInPath : function (x, y) {
    
    var tolerance = Math.max(this.lineWidth / 2, 2) + this.hitTolerance;
    var normalized = this._normalize(x, y);
    return isc.Math.euclideanDistanceToLine(this.startLeft, this.startTop, this.endLeft, this.endTop, normalized[0], normalized[1]) < tolerance;
},

showKnobs : function (knobType) {
    if (isc.isAn.Array(knobType)) return this.Super("showKnobs", arguments);

    var oldMoveKnobOffset = this._getMoveKnobOffset();
    this.Super("showKnobs", arguments);
    if (this.knobs.length > 1 && (knobType == "move" || knobType == "startPoint" || knobType == "endPoint") &&
        (this._moveKnob != null && !this._moveKnob.destroyed) &&
        (this.knobs.contains("startPoint") || this.knobs.contains("endPoint")))
    {
        var moveKnobOffset = this._getMoveKnobOffset();
        if (moveKnobOffset != null) {
            if (!oldMoveKnobOffset) oldMoveKnobOffset = [0, 0];
            this._moveKnob.moveBy(moveKnobOffset[0] - oldMoveKnobOffset[0],
                                  moveKnobOffset[1] - oldMoveKnobOffset[1]);
        }
    }
},

showResizeKnobs : null,
hideResizeKnobs : null,

_getKnobPosition : function (position) {
    var point = (position.contains("R") ? this.endPoint : this.startPoint);
    return this._normalize(point[0], point[1], "local", "global");
},

_getMoveKnobOffset : function () {
    if (this.moveKnobOffset != null) {
        return this.moveKnobOffset;
    }
    if (this.knobs == null || this.knobs.length == 0 || !this.knobs.contains("move")) {
        return null;
    }
    var isEndPointKnob = this.moveKnobPoint.contains("R"),
        knobPosition = this._getKnobPosition(this.moveKnobPoint);
    if (!this.knobs.contains(isEndPointKnob ? "endPoint" : "startPoint")) {
        return null;
    }
    return this._moveKnobPointOffsets[this.moveKnobPoint];
},

// Support controlKnobs for start/endPoints
// (Documented under DrawKnobs type definition) 
showStartPointKnobs : function () {
    if (this._startKnob != null && !this._startKnob.destroyed) return;
    

    var v = this._normalize(this.startLeft, this.startTop, "local", "global");

    this._startKnob = this.createAutoChild("startKnob", {
        x: v[0],
        y: v[1],
        drawPane: this.drawPane,

        resetKnobPosition : function () {
            var drawItem = this.creator,
                v = drawItem._normalize(
                    drawItem.startLeft, drawItem.startTop, "local", "global");
            this.setCenterPoint(v[0], v[1], false);
        },

        updatePoints : function (x, y, dx, dy, state) {
            var drawItem = this.creator,
                startState = (state == "start"),
                moveState = (state == "move"),
                stopState = (state == "stop");

            var fixedPoint;
            if (startState) {
                fixedPoint = drawItem._dragStartPointFixedPoint = drawItem.endPoint.duplicate();
                var v = drawItem._normalize(fixedPoint[0], fixedPoint[1], "local", "global");
                fixedPoint[0] = v[0];
                fixedPoint[1] = v[1];
            } else {
                fixedPoint = drawItem._dragStartPointFixedPoint;
            }

            if (drawItem.keepInParentRect) {
                var box = drawItem._getParentRect(),
                    point = drawItem._intersectLineSegmentBox(fixedPoint, [x, y], box);

                if (point == null) {
                    if (stopState) {
                        delete drawItem._dragStartPointFixedPoint;
                    }
                    return false;
                } else {
                    x = point[0];
                    y = point[1];
                }
            } else if (stopState) {
                delete drawItem._dragStartPointFixedPoint;
            }

            var v = drawItem._normalize(x, y, "global", "local"),
                cx0,
                cy0;
            if (isc.isA.DrawLinePath(drawItem)) {
                var center = drawItem._getRotationCenter();
                cx0 = center.cx;
                cy0 = center.cy;
            }
            drawItem.setStartPoint(
                isc.DrawItem._makeCoordinate(v[0]), isc.DrawItem._makeCoordinate(v[1]),
                // Manipulating the start point of a DrawLine changes its resize bounding box,
                // so moved() or resized() (or both) should fire.
                true,
                cx0, cy0);
        }
    });
},

hideStartPointKnobs : function () {
    if (this._startKnob) {
        this._startKnob.destroy();
        delete this._startKnob;
    }
},

showEndPointKnobs : function () {
    if (this._endKnob != null && !this._endKnob.destroyed) return;
    

    var v = this._normalize(this.endLeft, this.endTop, "local", "global");

    this._endKnob = this.createAutoChild("endKnob", {
        x: v[0],
        y: v[1],
        drawPane: this.drawPane,

        resetKnobPosition : function () {
            var drawItem = this.creator,
                v = drawItem._normalize(drawItem.endLeft, drawItem.endTop, "local", "global");
            this.setCenterPoint(v[0], v[1], false);
        },

        updatePoints : function (x, y, dx, dy, state) {
            var drawItem = this.creator,
                startState = (state == "start"),
                moveState = (state == "move"),
                stopState = (state == "stop"),
                fixedPoint;

            if (startState) {
                fixedPoint = drawItem._dragEndPointFixedPoint = drawItem.startPoint.duplicate();
                var v = drawItem._normalize(fixedPoint[0], fixedPoint[1], "local", "global");
                fixedPoint[0] = v[0];
                fixedPoint[1] = v[1];
            } else {
                fixedPoint = drawItem._dragEndPointFixedPoint;
            }

            if (drawItem.keepInParentRect) {
                var box = drawItem._getParentRect(),
                    point = drawItem._intersectLineSegmentBox(fixedPoint, [x, y], box);

                if (point == null) {
                    if (stopState) {
                        delete drawItem._dragEndPointFixedPoint;
                    }
                    return false;
                } else {
                    x = point[0];
                    y = point[1];
                }
            } else if (stopState) {
                delete drawItem._dragEndPointFixedPoint;
            }

            var v = drawItem._normalize(x, y, "global", "local"),
                cx0,
                cy0;
            if (isc.isA.DrawLinePath(drawItem)) {
                var center = drawItem._getRotationCenter();
                cx0 = center.cx;
                cy0 = center.cy;
            }
            drawItem.setEndPoint(
                isc.DrawItem._makeCoordinate(v[0]), isc.DrawItem._makeCoordinate(v[1]),
                // Manipulating the end point of a DrawLine changes its resize bounding box,
                // so moved() or resized() (or both) should fire.
                true,
                cx0, cy0);
        }
    });
},

hideEndPointKnobs : function () {
    if (this._endKnob) {
        this._endKnob.destroy();
        delete this._endKnob;
    }
},

//> @method drawLine.moveBy()
// Move both the start and end points of the line by a relative amount.
//
// @param left (int) change to left coordinate in pixels
// @param top (int) change to top coordinate in pixels
// @visibility drawing
//< 

//> @method drawLine.moveStartPointTo()
// Move both the start and end points of the line such that the +link{startPoint} ends up at
// the specified coordinate and the line length and angle are unchanged.
//
// @param left (integer) new left coordinate in pixels
// @param top (integer) new top coordinate in pixels
// @visibility drawing
//<
moveStartPointTo : function (left, top) {
    this._movePointToPoint(left, top, this.startLeft, this.startTop);
},

updateControlKnobs : function () {
    // update the position of our start/end point knobs when we update our other control points
    this.Super("updateControlKnobs", arguments);
    if (this._startKnob) {
        var v = this._normalize(this.startLeft, this.startTop, "local", "global");
        this._startKnob.setCenterPoint(v[0], v[1]);
    }
    if (this._endKnob) {
        var v = this._normalize(this.endLeft, this.endTop, "local", "global");
        this._endKnob.setCenterPoint(v[0], v[1]);
    }
}

}); // end DrawLine.addProperties








//------------------------------------------------------------------------------------------
//> @class DrawRect
//
//  DrawItem subclass to render rectangle shapes, optionally with rounded corners.
//
//  @inheritsFrom DrawItem
//  @treeLocation Client Reference/Drawing/DrawItem
//  @visibility drawing
//<
//------------------------------------------------------------------------------------------

isc.defineClass("DrawRect", "DrawItem").addProperties({
    // NOTE: left|top|width|height @included elsewhere so should be phrased generically

    //> @attr drawRect.left (int : 0 : IRW)
    // Left coordinate in pixels relative to the +link{DrawPane,local coordinate system}.
    //
    // @visibility drawing
    //<
    left:0, 

    //> @attr drawRect.top (int : 0 : IRW)
    // Top coordinate in pixels relative to the +link{DrawPane,local coordinate system}.
    //
    // @visibility drawing
    //<
    top:0, 

    //> @attr drawRect.width        (int : 100 : IRW)
    // Width in pixels relative to the +link{DrawPane,local coordinate system}.
    // @visibility drawing
    //<
    width:100, 

    //> @attr drawRect.height       (int : 100 : IRW)
    // Height in pixels relative to the +link{DrawPane,local coordinate system}.
    // @visibility drawing
    //<
    height:100,

    //> @attr drawRect.rounding      (float : 0 : IR)
    // Rounding of corners, from 0 (square corners) to 1.0 (shorter edge is a semicircle).
    //
    // @visibility drawing
    //<
    rounding:0,

    //> @attr drawRect.lineCap     (LineCap : "butt" : IRW)
    // Style of drawing the endpoints of a line.
    // <P>
    // Note that for dashed and dotted lines, the lineCap style affects each dash or dot.
    //
    // @group line
    // @visibility drawing
    //<
    lineCap: "butt",

    //> @attr drawRect.titleRotationMode (TitleRotationMode : "withItemAlwaysUp" : IRA)
    // @include DrawItem.titleRotationMode
    //<
    titleRotationMode: "withItemAlwaysUp",

    svgElementName: "rect",

_isClosed : function () {
    return true;
},

//----------------------------------------
//  DrawRect renderers
//----------------------------------------

_vmlRoundrectConfigAdapter: new isc.VMLRenderer.shapeConfig(),
_renderVML : function (vmlRenderer, id, conversionContext) {
    var config = this._vmlRoundrectConfigAdapter;
    isc.VMLRenderer.drawItemToShapeConfig(this, config);
    var left = this.left,
        top = this.top,
        width = this.width,
        height = this.height;
    if (width < 0) {
        left += width;
        width = -width;
    }
    if (height < 0) {
        top += height;
        height = -height;
    }
    config.left = left;
    config.top = top;
    config.width = width;
    config.height = height;
    
    config.arcsize = (this.rounding / 2);
    this._vmlRendererHandle = vmlRenderer.roundrect(id, config);
},

getAttributesSVG : function () {
    var left = this.left,
        top = this.top,
        width = this.width,
        height = this.height;
    if (width < 0) {
        left += width;
        width = -width;
    }
    if (height < 0) {
        top += height;
        height = -height;
    }
    return isc.SB.concat(
        "x='", left, "' y='", top, "' width='", width, "' height='", height, "'",
        (this.rounding ? (" rx='" + (this.rounding * Math.min(width, height) / 2) + "'") : ""));
},

// drawRect.drawBitmapPath()
drawBitmapPath : function (context) {
    var left = this.left,
        top = this.top,
        width = this.width,
        height = this.height;
    if (width < 0) {
        left += width;
        width = -width;
    }
    if (height < 0) {
        top += height;
        height = -height;
    }
    var right = left + width,
        bottom = top + height;

    if (this.rounding == 0) {
        this.bmMoveTo(left, top, context);
        if (this.linePattern.toLowerCase() !== "solid") {
            this._drawLinePattern(left, top, right, top, context);
            this._drawLinePattern(right, top, right, bottom, context);
            this._drawLinePattern(right, bottom, left, bottom, context);
            this._drawLinePattern(left, bottom, left, top, context);
        } else {
            this.bmLineTo(right, top, context);
            this.bmLineTo(right, bottom, context);
            this.bmLineTo(left, bottom, context);
            this.bmLineTo(left, top, context);
        }
    } else {
        
        var kappa = isc.DrawItem._kappa,
            r = Math.round(this.rounding * Math.min(width, height) / 2),
            r1mkappa = r * (1 - kappa);
        this.bmMoveTo(left + r, top, context);
        this.bmLineTo(right - r, top, context);
        this.bmBezierCurveTo(right - r1mkappa, top, right, top + r1mkappa, right, top + r, context);
        this.bmLineTo(right, bottom - r, context);
        this.bmBezierCurveTo(right, bottom - r1mkappa, right - r1mkappa, bottom, right - r, bottom, context);
        this.bmLineTo(left + r, bottom, context);
        this.bmBezierCurveTo(left + r1mkappa, bottom, left, bottom - r1mkappa, left, bottom - r, context);
        this.bmLineTo(left, top + r, context);
        this.bmBezierCurveTo(left, top + r1mkappa, left + r1mkappa, top, left + r, top, context);
    }
    context.closePath();
},

//----------------------------------------
//  DrawRect attribute setters
//----------------------------------------

//> @method drawRect.setCenter()
// Move the drawRect such that it is centered over the specified coordinates.
// @param left (int) left coordinate for new center position
// @param top (int) top coordinate for new center postiion
// @visibility drawing
//<
setCenter : function (left, top) {
    if (isc.isAn.Array(left)) {
        top = left[1];
        left = left[0];
    }
    this.setRect(
        (left == null ? this.left : left - isc.DrawItem._makeCoordinate(this.width / 2)),
        (top == null ? this.top : top - isc.DrawItem._makeCoordinate(this.height / 2)),
        this.width,
        this.height);
},

//> @method drawRect.getCenter()
// Get the center point of the rectangle.
// @return (Point) the center point
// @visibility drawing
//<
getCenter : function () {
    return [this.left + isc.DrawItem._makeCoordinate(this.width/2), this.top + isc.DrawItem._makeCoordinate(this.height/2)];
},

//> @method drawRect.getBoundingBox()
// Returns the top, left, top+height, left+width 
// @return (Array[] of double) x1, y1, x2, y2 coordinates
// @visibility drawing
//<
getBoundingBox : function (includeStroke, outputBox) {
    
    var bbox = (outputBox || new Array(4));
    bbox[0] = this.left;
    bbox[1] = this.top;
    bbox[2] = this.left + this.width;
    bbox[3] = this.top + this.height;
    return includeStroke != true ? bbox : this._adjustBoundingBox(true, false, bbox);
},


_getBoundingBoxOfTransformedShape : function (
    transform, includeStroke, includeHitTolerance, outputBox)
{
    
    var rounding = this.rounding || 0;
    if (rounding == 0) {
        
        return this.Super("_getBoundingBoxOfTransformedShape", arguments);
    }

    var box = this.getBoundingBox(includeStroke, outputBox);
    this._adjustBoundingBox(false, includeHitTolerance, box);
    var left = box[0],
        top = box[1],
        right = box[2],
        bottom = box[3],
        width = right - left,
        height = bottom - top;

    

    // Use the same radius from DrawRect.getAttributesSVG() and drawBitmapPath().
    var radius = (rounding * Math.min(width, height) / 2),
        x0 = (left + right) / 2,
        y0 = (top + bottom) / 2;

    // The comment in DrawOval's implementation of this method explains the following formulas.
    var m00 = transform.m00, m01 = transform.m01, m02 = transform.m02,
        m10 = transform.m10, m11 = transform.m11, m12 = transform.m12,
        halfRoundingWidth = radius * isc.Math._hypot(m00, m01),
        halfRoundingHeight = radius * isc.Math._hypot(m10, m11),
        centerX = (m00 * x0 + m01 * y0 + m02),
        centerY = (m10 * x0 + m11 * y0 + m12);

    var halfWidth = width / 2,
        halfHeight = height / 2,
        halfInnerWidth = (halfWidth - radius),
        halfInnerHeight = (halfHeight - radius),
        // Calculate the coordinates of four transformed points.
        sideX1 = Math.abs(m00 * halfWidth + m01 * -halfInnerHeight),
        sideY1 = Math.abs(m10 * halfWidth + m11 * -halfInnerHeight),
        sideX2 = Math.abs(m00 * halfInnerWidth + m01 * -halfHeight),
        sideY2 = Math.abs(m10 * halfInnerWidth + m11 * -halfHeight),
        sideX3 = Math.abs(m00 * -halfInnerWidth + m01 * -halfHeight),
        sideY3 = Math.abs(m10 * -halfInnerWidth + m11 * -halfHeight),
        sideX4 = Math.abs(m00 * -halfWidth + m01 * -halfInnerHeight),
        sideY4 = Math.abs(m10 * -halfWidth + m11 * -halfInnerHeight),
        // The bounding box of region A is `[minSideX, minSideY, maxSideX, maxSideY]`.
        minSideX = centerX - Math.max(sideX1, sideX2, sideX3, sideX4),
        maxSideX = 2 * centerX - minSideX,
        minSideY = centerY - Math.max(sideY1, sideY2, sideY2, sideY4),
        maxSideY = 2 * centerY - minSideY;

    // The bounding box of region B is `[minCornerX, minCornerY, maxCornerX, maxCornerY]`.
    var innerX1 = Math.abs(m00 * halfInnerWidth + m01 * -halfInnerHeight),
        innerY1 = Math.abs(m10 * halfInnerWidth + m11 * -halfInnerHeight),
        innerX2 = Math.abs(m00 * halfInnerWidth + m01 * halfInnerHeight),
        innerY2 = Math.abs(m10 * halfInnerWidth + m11 * halfInnerHeight),
        minCornerX = centerX - (halfRoundingWidth + Math.max(innerX1, innerX2)),
        maxCornerX = 2 * centerX - minCornerX,
        minCornerY = centerY - (halfRoundingHeight + Math.max(innerY1, innerY2)),
        maxCornerY = 2 * centerY - minCornerY;

    box[0] = Math.min(minSideX, minCornerX);
    box[1] = Math.min(minSideY, minCornerY);
    box[2] = Math.max(maxSideX, maxCornerX);
    box[3] = Math.max(maxSideY, maxCornerY);
    return box;
},

//> @method drawRect.isPointInPath()
// @include DrawItem.isPointInPath()
//<
isPointInPath : function (x, y, pageX, pageY) {
    if (this.rounding != 0) return this.Super("isPointInPath", arguments);

    var normalized = this._normalize(x, y),
        b = this.getBoundingBox(true, this._tempBoundingBox),
        hitTolerance = this.hitTolerance;

    // If the point is not within the bounds of this DrawRect (see DrawItem.isInBounds()), then
    // the point is not in the path.
    if (!(((b[0] - hitTolerance <= normalized[0] && normalized[0] <= b[2] + hitTolerance) ||
           (b[2] - hitTolerance <= normalized[0] && normalized[0] <= b[0] + hitTolerance)) &&
          ((b[1] - hitTolerance <= normalized[1] && normalized[1] <= b[3] + hitTolerance) ||
           (b[3] - hitTolerance <= normalized[1] && normalized[1] <= b[1] + hitTolerance))))
    {
        return false;
    }

    // If this DrawRect is unfilled, we need to verify that the point is not in the whitespace.
    // This is roughly modeled after _adjustBoundingBox(), but instead of expanding the box
    // by half the lineWidth plus the hitTolerance, the box is shrunken by that amount.
    if (!this._hasFill()) {
        var halfLineWidth = this._hasStroke() ? this.lineWidth / 2 : 0,
            offset = halfLineWidth + hitTolerance;

        if (offset <= 0) return false;

        var left = this.left,
            top = this.top;
        b[0] = left;
        b[1] = top;
        b[2] = left + this.width;
        b[3] = top + this.height;

        if (b[0] <= b[2]) {
            b[0] += offset;
            b[2] = Math.max(b[0], b[2] - offset);
        } else {
            b[2] += offset;
            b[0] = Math.max(b[2], b[0] - offset);
        }

        if (b[1] <= b[3]) {
            b[1] += offset;
            b[3] = Math.max(b[1], b[3] - offset);
        } else {
            b[3] += offset;
            b[1] = Math.max(b[3], b[1] - offset);
        }

        // If the point is within the "whitespace" box, then return false.
        if (((b[0] <= normalized[0] && normalized[0] <= b[2]) ||
             (b[2] <= normalized[0] && normalized[0] <= b[0])) &&
            ((b[1] <= normalized[1] && normalized[1] <= b[3]) ||
             (b[3] <= normalized[1] && normalized[1] <= b[1])))
        {
            return false;
        }
    }

    return true;
},

//> @method drawRect.moveBy()
// Move the drawRect by the specified delta
// @param dX (int) number of pixels to move horizontally
// @param dY (int) number of pixels to move vertically
// @visibility drawing
//<

//> @method drawRect.moveTo()
// Move the drawRect to the specified position
// @param left (integer) new left coordinate
// @param top (integer) new top coordinate
// @visibility drawing
//<

//> @method drawRect.setLeft()
// Set the left coordinate of the drawRect
// @param left (int) new left coordinate
// @visibility drawing
//<
setLeft : function (left) {
    this.setRect(left, this.top, this.width, this.height);
},

//> @method drawRect.setTop()
// Set the top coordinate of the drawRect
// @param top (int) new top coordinate
// @visibility drawing
//<
setTop : function (top) {
    this.setRect(this.left, top, this.width, this.height);
},

//> @method drawRect.resizeTo()
// Resize to the specified size
// @param width (integer) new width
// @param height (integer) new height
// @visibility drawing
//<

//> @method drawRect.resizeBy()
// Resize by the specified delta
// @param dX (int) number of pixels to resize by horizontally
// @param dY (int) number of pixels to resize by vertically
// @visibility drawing
//<

//> @method drawRect.setWidth()
// Set the width of the drawRect
// @param width (int) new width
// @visibility drawing
//<
setWidth : function (width) {
    this.setRect(this.left, this.top, width, this.height);
},

//> @method drawRect.setHeight()
// Set the height of the drawRect
// @param height (int) new height
// @visibility drawing
//<
setHeight : function (height) {
    this.setRect(this.left, this.top, this.width, height);
},

//> @method drawRect.setRect()
// Move and resize the drawRect to match the specified coordinates and size.
// @param left (integer) new left coordinate
// @param top (integer) new top coordinate
// @param width (integer) new width
// @param height (integer) new height
// @visibility drawing
//<
setRect : function (left, top, width, height, cx0, cy0) {
    if (left == null) left = this.left;
    if (top == null) top = this.top;
    if (width == null) width = this.width;
    if (height == null) height = this.height;

    

    var sameLTWH = false;
    if (this.left == left &&
        this.top == top &&
        this.width == width &&
        this.height == height)
    {
        sameLTWH = true;
        if (cx0 == null && cy0 == null) return;
    }

    
    if (cx0 == null || cy0 == null) {
        var center0 = this._getRotationCenter();
        if (sameLTWH &&
            (cx0 == null || cx0 == center0.cx) &&
            (cy0 == null || cy0 == center0.cy))
        {
            return;
        }
        cx0 = center0.cx;
        cy0 = center0.cy;
    }

    this.left = left;
    this.top = top;
    this.width = width;
    this.height = height;

    var center = this._getRotationCenter();
    if (sameLTWH &&
        cx0 == center.cx &&
        cy0 == center.cy)
    {
        return;
    }
    this._updateRotationCenter(cx0, cy0, center.cx, center.cy);

    if (this.drawingVML) {
        var vmlRendererHandle = this._vmlRendererHandle;
        if (cx0 != center.cx || cy0 != center.cy) {
            this._transform = null;
            vmlRendererHandle.setProperty("transform", this._getLocalTransform());
        }
        vmlRendererHandle.setProperty("left", left).setProperty("top", top)
            .setProperty("width", width).setProperty("height", height)
            .flush();

    } else if (this.drawingSVG) {
        var svgHandle = this._svgHandle;
        if (cx0 != center.cx || cy0 != center.cy) {
            this._transform = null;
            var t = this._getLocalTransform();
            svgHandle.setAttributeNS(null, "transform", "matrix(" + t.m00 + " " + t.m10 + " " +
                                                                    t.m01 + " " + t.m11 + " " +
                                                                    t.m02 + " " + t.m12 + ")");
        }
        svgHandle.setAttributeNS(null, "x", left);
        svgHandle.setAttributeNS(null, "y", top);
        svgHandle.setAttributeNS(null, "width", width);
        svgHandle.setAttributeNS(null, "height", height);

    } else if (this.drawingBitmap) {
        this.drawPane.redrawBitmap();
    }

    this._reshaped();
},

// drawRect._updateLocalTransform()

_updateLocalTransform : function (transform, cx, cy, initialShape, fireReshaped, viaLocalTransformOnly) {
    
    if (initialShape == null) {
        initialShape = this;
    }
    if (!viaLocalTransformOnly && initialShape.rounding == 0) {
        var info = isc.DrawItem._fitBestRect(
                transform, cx, cy,
                initialShape.left, initialShape.top, initialShape.width, initialShape.height,
                (this._hasStroke() ? (this.lineWidth / 2) : 0));
        if (info.success) {
            this.Super("_updateLocalTransform", [transform, cx, cy, initialShape, false, true], arguments);
            this.setRect(isc.DrawItem._makeCoordinate(info.left),
                         isc.DrawItem._makeCoordinate(info.top),
                         isc.DrawItem._makeCoordinate(info.width),
                         isc.DrawItem._makeCoordinate(info.height),
                         isc.DrawItem._makeCoordinate(cx),
                         isc.DrawItem._makeCoordinate(cy));
            return;
        }
    }
    this.Super("_updateLocalTransform", [transform, cx, cy, initialShape, fireReshaped, true], arguments);
},

_saveShape : function () {
    return {
        left: this.left,
        top: this.top,
        width: this.width,
        height: this.height,
        rounding: this.rounding
    };
},


//> @method drawRect.setRounding()
// Setter method for +link{drawRect.rounding}
// @param rounding (float) new rounding value. Should be between zero (a rectangle) and 1 (shorter
//   edge is a semicircle)
// @visibility drawing
//<
setRounding : function (rounding) {
    this.rounding = rounding;
    if (this.drawingVML) {
        this._vmlRendererHandle.setProperty("arcsize", rounding / 2).flush();
    } else if (this.drawingSVG) {      
        this._svgHandle.setAttributeNS(null, "rx", (this.rounding ?
            (this.rounding * Math.min(Math.abs(this.width), Math.abs(this.height)) / 2) :
            null));
    } else if (this.drawingBitmap) {
        this.drawPane.redrawBitmap();
    }
}

}); // end DrawRect.addProperties

isc.DrawRect.markUnsupportedMethods(null, ["setStartArrow", "setEndArrow"]);








//------------------------------------------------------------------------------------------
//> @class DrawOval
//
//  DrawItem subclass to render oval shapes, including circles.
//
//  @inheritsFrom DrawItem
//  @treeLocation Client Reference/Drawing/DrawItem
//  @visibility drawing
//<
//------------------------------------------------------------------------------------------

isc.defineClass("DrawOval", "DrawItem").addProperties({

    //> @attr drawOval.left (int : 0 : IRW)
    // @include drawRect.left
    //<
    left:0, 

    //> @attr drawOval.top (int : 0 : IRW)
    // @include drawRect.top
    //<
    top:0, 

    //> @attr drawOval.width (int : 100 : IRW)
    // @include drawRect.width
    //<
    width:100, 

    //> @attr drawOval.height (int : 100 : IRW)
    // @include drawRect.height
    //<
    height:100,

    //> @attr drawOval.centerPoint    (Point : null : IRW)
    // Center point of the oval.  If unset, derived from left/top/width/height.
    // @visibility drawing
    //<

    //> @attr drawOval.radius (int : null : IW)
    // Radius of the oval. Since this is used to initialize the +link{DrawOval.getRadiusX(),horizontal}
    // and +link{DrawOval.getRadiusY(),vertical} radii, then the oval is a circle.
    // <p>
    // If unset, the horizontal and vertical radii are set to half the +link{DrawOval.width,width}
    // and +link{DrawOval.height,height}.
    // @setter setRadius()
    // @see DrawOval.getRadiusX()
    // @see DrawOval.getRadiusY()
    // @visibility drawing
    //<

    //rx
    //ry

    //> @attr drawOval.titleRotationMode (TitleRotationMode : "withItemAlwaysUp" : IRA)
    // @include DrawItem.titleRotationMode
    //<
    titleRotationMode: "withItemAlwaysUp",

    svgElementName: "ellipse",

// TODO review init property precedence and null/zero check    

// drawOval.init()
init : function () {
    // convert rect to center/radii
    if (!this.radius) this._deriveCenterPointFromRect();
    else this._deriveRectFromRadius();

    this.Super("init", arguments);
},

_isClosed : function () {
    return true;
},

// Helpers to synch rect coords with specified centerPoint / radius and vice versa 
_deriveRectFromRadius : function () {
    // convert center/radii to rect
    if (this.rx == null) this.rx = this.radius;
    if (this.ry == null) this.ry = this.radius;
    this.width = this.rx * 2;
    this.height = this.ry * 2;
    // support either centerpoint or direct left/top
    if (this.centerPoint != null) {
        this.left = this.centerPoint[0] - this.width/2;
        this.top = this.centerPoint[1] - this.height/2;
    } else {
        this.centerPoint = new Array(2);
        this.centerPoint[0] = this.left + this.width/2;
        this.centerPoint[1] = this.top + this.height/2;
    }
},
_deriveCenterPointFromRect : function () {
    this.centerPoint = new Array(2);
    var halfWidth = (this.width / 2) << 0,
        halfHeight = (this.height / 2) << 0;
    this.centerPoint[0] = this.left + halfWidth;
    this.centerPoint[1] = this.top + halfHeight;
    this.rx = halfWidth;
    this.ry = halfHeight;
},
//----------------------------------------
//  DrawOval renderers
//----------------------------------------

_vmlOvalConfigAdapter: new isc.VMLRenderer.shapeConfig(),
_renderVML : function (vmlRenderer, id, conversionContext) {
    var config = this._vmlOvalConfigAdapter;
    isc.VMLRenderer.drawItemToShapeConfig(this, config);
    this._vmlRendererHandle = vmlRenderer.oval(id, config);
},

getAttributesSVG : function () {
    return  "cx='" + this.centerPoint[0] +
            "' cy='" + this.centerPoint[1] +
            "' rx='" + this.rx +
            "' ry='" + this.ry +
            "'"
},

//> @method drawOval.getBoundingBox()
// Returns the top, left, top+height, left+width 
// @return (Array[] of double) x1, y1, x2, y2 coordinates
// @visibility drawing
//<
getBoundingBox : function (includeStroke, outputBox) {
    
    var bbox = (outputBox || new Array(4));
    bbox[0] = this.left;
    bbox[1] = this.top;
    bbox[2] = this.left + this.width;
    bbox[3] = this.top + this.height;
    return includeStroke != true ? bbox : this._adjustBoundingBox(true, false, bbox);
},


_getBoundingBoxOfTransformedShape : function (
    transform, includeStroke, includeHitTolerance, outputBox)
{
    
    var box = this.getBoundingBox(includeStroke, outputBox);
    this._adjustBoundingBox(false, includeHitTolerance, box);
    var left = box[0],
        top = box[1],
        right = box[2],
        bottom = box[3];

    // Equation of an axis-aligned ellipse is:  (x - x0)^2 / alpha^2 + (y - y0)^2 / beta^2 = 1
    var x0 = (left + right) / 2,
        y0 = (top + bottom) / 2,
        alpha = Math.abs(right - left) / 2,
        beta = Math.abs(bottom - top) / 2;

    
    var m00 = transform.m00, m01 = transform.m01, m02 = transform.m02,
        m10 = transform.m10, m11 = transform.m11, m12 = transform.m12,
        halfWidth = isc.Math._hypot(alpha * m00, beta * m01),
        halfHeight = isc.Math._hypot(alpha * m10, beta * m11),
        centerX = (m00 * x0 + m01 * y0 + m02),
        centerY = (m10 * x0 + m11 * y0 + m12);

    box[0] = centerX - halfWidth;
    box[1] = centerY - halfHeight;
    box[2] = centerX + halfWidth;
    box[3] = centerY + halfHeight;
    return box;
},



drawBitmapPath : function (context) {
    var kappa = isc.DrawItem._kappa;
    var rx = this.rx;
    var ry = this.ry;
    var cx = this.centerPoint[0];
    var cy = this.centerPoint[1];
    context.moveTo(cx, cy - ry);
    context.bezierCurveTo(cx + (kappa * rx), cy - ry,  cx + rx, cy - (kappa * ry), cx + rx, cy);
    context.bezierCurveTo(cx + rx, cy + (kappa * ry), cx + (kappa * rx), cy + ry, cx, cy + ry);
    context.bezierCurveTo(cx - (kappa * rx), cy + ry, cx - rx, cy + (kappa * ry), cx - rx, cy);
    context.bezierCurveTo(cx - rx, cy - (kappa * ry), cx - (kappa * rx), cy - ry, cx, cy - ry);
    context.closePath();
},

//----------------------------------------
//  DrawOval attribute setters
//----------------------------------------

//> @method drawOval.setCenterPoint()
// Change the center point for this oval.
// @param left (int) left coordinate (in the global coordinate system)
// @param top (int) top coordinate (in the global coordinate system)
// @visibility drawing
//<

//> @method drawOval.moveBy()
// Move the drawOval by the specified delta
// @param dX (int) number of pixels to move horizontally
// @param dY (int) number of pixels to move vertically
// @visibility drawing
//<

//> @method drawOval.moveTo()
// Move the drawOval to the specified left/top position. You may also call
// +link{drawOval.setCenterPoint} to reposition the oval around a new center position.
// @param left (integer) new left coordinate
// @param top (integer) new top coordinate
// @visibility drawing
//<

//> @method drawOval.setLeft()
// Set the left coordinate of the drawOval
// @param left (int) new left coordinate
// @visibility drawing
//<
setLeft : function (left) {
    this.setRect(left, this.top, this.width, this.height);
},

//> @method drawOval.setTop()
// Set the top coordinate of the drawOval
// @param top (int) new top coordinate
// @visibility drawing
//<
setTop : function (top) {
    this.setRect(this.left, top, this.width, this.height);
},

//> @method drawOval.resizeBy()
// Resize by the specified delta. Note that the resize will occur from the current top/left 
// coordinates, meaning the center positon of the oval may change. You may also use
// +link{drawOval.setRadii()} to change the radius in either direction without modifying the
// centerpoint.
// @param dX (int) number of pixels to resize by horizontally
// @param dY (int) number of pixels to resize by vertically
// @visibility drawing
//<

//> @method drawOval.resizeTo()
// Resize to the specified size. Note that the resize will occur from the current top/left 
// coordinates, meaning the center positon of the oval may change. You may also use
// +link{drawOval.setRadii()} to change the radius in either direction without modifying the
// centerpoint.
// @param width (integer) new width
// @param height (integer) new height
// @visibility drawing
//<

//> @method drawOval.setWidth()
// Set the width of the drawOval
// @param width (int) new width
// @visibility drawing
//<
setWidth : function (width) {
    this.setRect(this.left, this.top, width, this.height);
},

//> @method drawOval.setHeight()
// Set the height of the drawOval
// @param height (int) new height
// @visibility drawing
//<
setHeight : function (height) {
    this.setRect(this.left, this.top, this.width, height);
},

//> @method drawOval.setRect()
// Move and resize the drawOval to match the specified coordinates and size.
// @param left (integer) new left coordinate
// @param top (integer) new top coordinate
// @param width (integer) new width
// @param height (integer) new height
// @visibility drawing
//<
setRect : function (left, top, width, height, cx0, cy0) {
    if (left == null) left = this.left;
    if (top == null) top = this.top;
    if (width == null) width = this.width;
    if (height == null) height = this.height;

    

    var sameLTWH = false;
    if (this.left == left &&
        this.top == top &&
        this.width == width &&
        this.height == height)
    {
        sameLTWH = true;
        if (cx0 == null && cy0 == null) return;
    }

    var halfWidth = (width / 2) << 0,
        halfHeight = (height / 2) << 0,
        centerX = left + halfWidth,
        centerY = top + halfHeight;

    
    if (cx0 == null || cy0 == null) {
        var center0 = this._getRotationCenter();
        if (sameLTWH &&
            (cx0 == null || cx0 == center0.cx) &&
            (cy0 == null || cy0 == center0.cy))
        {
            return;
        }
        cx0 = center0.cx;
        cy0 = center0.cy;
    }

    this.left = left;
    this.top = top;
    this.width = width;
    this.height = height;
    this.centerPoint[0] = centerX;
    this.centerPoint[1] = centerY;
    this.rx = halfWidth;
    this.ry = halfHeight;

    var center = this._getRotationCenter();
    if (sameLTWH &&
        cx0 == center.cx &&
        cy0 == center.cy)
    {
        return;
    }
    this._updateRotationCenter(cx0, cy0, center.cx, center.cy);

    if (this.drawingVML) {
        var vmlRendererHandle = this._vmlRendererHandle;
        if (cx0 != center.cx || cy0 != center.cy) {
            this._transform = null;
            vmlRendererHandle.setProperty("transform", this._getLocalTransform());
        }
        vmlRendererHandle.setProperty("left", left).setProperty("top", top)
            .setProperty("width", width).setProperty("height", height).flush();

    } else if (this.drawingSVG) {
        var svgHandle = this._svgHandle;
        if (cx0 != center.cx || cy0 != center.cy) {
            this._transform = null;
            var t = this._getLocalTransform();
            svgHandle.setAttributeNS(null, "transform", "matrix(" + t.m00 + " " + t.m10 + " " +
                                                                    t.m01 + " " + t.m11 + " " +
                                                                    t.m02 + " " + t.m12 + ")");
        }
        svgHandle.setAttributeNS(null, "rx", halfWidth);
        svgHandle.setAttributeNS(null, "ry", halfHeight);
        svgHandle.setAttributeNS(null, "cx", centerX);
        svgHandle.setAttributeNS(null, "cy", centerY);

    } else if (this.drawingBitmap) {
        this.drawPane.redrawBitmap();
    }
    this._reshaped();
},

// Override DrawItem._updateLocalTransform() similar to DrawRect.
_updateLocalTransform : function (transform, cx, cy, initialShape, fireReshaped, viaLocalTransformOnly) {
    
    if (!viaLocalTransformOnly) {
        if (initialShape == null) {
            initialShape = this;
        }
        var info = isc.DrawItem._fitBestRect(
                transform, cx, cy,
                initialShape.left, initialShape.top, initialShape.width, initialShape.height,
                (this._hasStroke() ? (this.lineWidth / 2) : 0));
        if (info.success) {
            this.Super("_updateLocalTransform", [transform, cx, cy, initialShape, false, true], arguments);
            this.setRect(isc.DrawItem._makeCoordinate(info.left),
                         isc.DrawItem._makeCoordinate(info.top),
                         isc.DrawItem._makeCoordinate(info.width),
                         isc.DrawItem._makeCoordinate(info.height),
                         isc.DrawItem._makeCoordinate(cx),
                         isc.DrawItem._makeCoordinate(cy));
            return;
        }
    }
    this.Super("_updateLocalTransform", [transform, cx, cy, initialShape, fireReshaped, true], arguments);
},

_saveShape : function () {
    return {
        left: this.left,
        top: this.top,
        width: this.width,
        height: this.height
    };
},

//> @method drawOval.getRadiusX()
// Returns the horizontal radius of the DrawOval.
// @return (int) the horizontal radius.
// @see DrawOval.setRadii()
// @visibility drawing
//<
getRadiusX : function () {
    return this.rx;
},

//> @method drawOval.getRadiusY()
// Returns the vertical radius of the DrawOval.
// @return (int) the vertical radius.
// @see DrawOval.setRadii()
// @visibility drawing
//<
getRadiusY : function () {
    return this.ry;
},

//> @method drawOval.setRadii()
// Resize the drawOval by setting its horizontal and vertical radius, and retaining its current
// center point.
// @param rx (int) new horizontal radius
// @param ry (int) new vertical radius
// @see DrawOval.getRadiusX()
// @see DrawOval.getRadiusY()
// @visibility drawing
//<
setRadii : function (rx, ry) {
    if (isc.isAn.Array(rx)) {
        ry = rx[1];
        rx = rx[0];
    }
    this.setOval(this.centerPoint[0], this.centerPoint[1], rx, ry);
},

//> @method drawOval.setRadius()
// Resize the drawOval by setting its radius, and retaining its current center point.
// Equivalent to <code>setRadii(radius, radius)</code>.
// @param radius (int) new radius. This will be applied on both axes, meaning calling this
// method will always result in the DrawOval being a circle.
// @see DrawOval.setRadii()
// @visibility drawing
//<
setRadius : function (radius) {
    this.setRadii(radius, radius);
},

//> @method drawOval.setOval()
// Resize and reposition the drawOval by setting its radius, and centerPoint.
// @param cx (int) new horizontal center point coordinate
// @param cy (int) new vertical center point coordinate
// @param rx (int) new horizontal radius
// @param ry (int) new vertical radius
// @visibility drawing
//<
setOval : function (cx, cy, rx, ry) {
    if (cx == null) cx = this.centerPoint[0];
    if (cy == null) cy = this.centerPoint[1];
    if (rx == null) rx = this.rx;
    if (ry == null) ry = this.ry;
    this.setRect(cx - rx, cy - ry, 2 * rx, 2 * ry);
},

//> @method drawOval.getCenter()
// Get the center of the oval.
// @return (Point) the center point
// @visibility drawing
//<
getCenter : function () {
    return this.centerPoint.duplicate();
}

}); // end DrawOval.addProperties

isc.DrawOval.markUnsupportedMethods(null, ["setStartArrow", "setEndArrow"]);








//------------------------------------------------------------------------------------------
//> @class DrawSector
//
//  DrawItem subclass to render Pie Slices.
//
//  @inheritsFrom DrawItem
//  @treeLocation Client Reference/Drawing/DrawItem
//  @visibility drawing
//<
//------------------------------------------------------------------------------------------

isc.defineClass("DrawSector", "DrawItem");

isc.DrawSector.addClassProperties({
    //> @classMethod drawSector.getArcMidpoint()
    // Calculates the midpoint coordinates of the circular arc of the sector defined by the
    // given centerPoint, startAngle, endAngle, and radius. The formula for this point is:
    // <blockquote>
    // <pre>var averageAngle = (startAngle + endAngle) / 2; // in degrees
    //[centerX + radius * cosdeg(averageAngle), centerY + radius * sindeg(averageAngle)]</pre>
    // </blockquote>
    // @param centerX (double) X coordinate of the center point of the sector.
    // @param centerY (double) Y coordinate of the center point of the sector.
    // @param startAngle (double) start angle of the sector in degrees.
    // @param endAngle (double) end angle of the sector in degrees.
    // @param radius (double) radius of the sector.
    // @return (Point) the coordinates of the midpoint of the arc.
    // @visibility drawing
    //<
    getArcMidpoint : function (centerX, centerY, startAngle, endAngle, radius) {
        var averageAngle = (startAngle + endAngle) / 2;
        return [centerX + radius * isc.Math.cosdeg(averageAngle),
                centerY + radius * isc.Math.sindeg(averageAngle)];
    }
});

isc.DrawSector.addProperties({
    //> @attr drawSector.knobs
    // DrawSector only supports the
    // <smartclient>"move"</smartclient>
    // <smartgwt>{@link com.smartgwt.client.types.KnobType#MOVE}</smartgwt>
    // knob type.
    // @see DrawItem.knobs
    // @include DrawItem.knobs
    //<

    //> @attr drawSector.showTitleLabelBackground (boolean : true : IRA)
    // @include DrawItem.showTitleLabelBackground
    //<
    showTitleLabelBackground: true,

    //> @attr drawSector.rotation
    // Rotation in degrees about the +link{DrawSector.centerPoint,centerPoint} of the DrawSector.
    // The positive direction is clockwise.
    // @include DrawItem.rotation
    //<

    //> @attr drawSector.centerPoint     (Point : [0,0] : IRW)
    // Center point of the sector
    // @visibility drawing
    //<
    centerPoint: [0,0],

    //> @attr drawSector.startAngle      (float: 0.0 : IR)
    // Start angle of the sector in degrees.  Default of 0.0 will create a sector that starts
    // with a line from the +link{centerPoint} and extends to the right for the indicated
    // +link{radius}, then sweeps clockwise toward the +link{endAngle}.
    // @visibility drawing
    //<
    startAngle: 0.0,

    //> @attr drawSector.endAngle      (float: 20.0 : IR)
    // End angle of the sector in degrees.
    // @visibility drawing
    //<
    endAngle: 20.0,

    //> @attr drawSector.radius         (int : 100: IR)
    // Radius of the sector. 
    // @visibility drawing
    //<
    radius: 100,

    
    _avoidRadialLineSegment: false,

    svgElementName: "path",

init : function () {
    this.Super("init", arguments);
    this.centerPoint = this.centerPoint.duplicate();
},

_isClosed : function () {
    return true;
},

//> @method drawSector.getBoundingBox()
// Returns the centerPoint endPoint
// @return (Array[] of double) x1, y1, x2, y2 coordinates
// @visibility drawing
//<
getBoundingBox : function (includeStroke, outputBox) {
    
    var convert = this._radPerDeg,
        centerPoint = this.centerPoint,
        radius = this.radius,
        startAngle = this.startAngle,
        endAngle = this.endAngle,
        diffAngle = (endAngle - startAngle),
        wholeCircle = (diffAngle >= 360 || (diffAngle != 0 && Math.round(diffAngle) % 360 == 0)),
        minSin = 0, maxSin = 0, minCos = 0, maxCos = 0;

    if (wholeCircle) {
        minSin = minCos = -1;
        maxSin = maxCos = 1;
    } else {
        if (!(0 <= startAngle && startAngle < 360)) {
            startAngle = (360 + (startAngle % 360)) % 360;
        }
        if (!(0 <= endAngle && endAngle < 360)) {
            endAngle = (360 + (endAngle % 360)) % 360;
        }

        var wrap = (startAngle > endAngle);
        if (wrap ? (startAngle < 270 || 270 < endAngle) : (startAngle < 270 && 270 < endAngle)) {
            minSin = -1;
        } else {
            minSin = Math.min(0, Math.sin(convert * startAngle), Math.sin(convert * endAngle));
        }
        if (wrap ? (startAngle < 90 || 90 < endAngle) : (startAngle < 90 && 90 < endAngle)) {
            maxSin = 1;
        } else {
            maxSin = Math.max(0, Math.sin(convert * startAngle), Math.sin(convert * endAngle));
        }
        if (wrap ? (startAngle < 180 || 180 < endAngle) : (startAngle < 180 && 180 < endAngle)) {
            minCos = -1;
        } else {
            minCos = Math.min(0, Math.cos(convert * startAngle), Math.cos(convert * endAngle));
        }
        if (wrap) {
            maxCos = 1;
        } else {
            maxCos = Math.max(0, Math.cos(convert * startAngle), Math.cos(convert * endAngle));
        }
    }

    var bbox = (outputBox || new Array(4));
    bbox[0] = centerPoint[0] + radius * minCos;
    bbox[1] = centerPoint[1] + radius * minSin;
    bbox[2] = centerPoint[0] + radius * maxCos;
    bbox[3] = centerPoint[1] + radius * maxSin;
    return includeStroke != true ? bbox : this._adjustBoundingBox(true, false, bbox);
},


_getBoundingBoxOfTransformedShape : function (
    transform, includeStroke, includeHitTolerance, outputBox)
{
    
    var convert = this._radPerDeg,
        centerPoint = this.centerPoint,
        radius = this.radius,
        startAngle = this.startAngle,
        endAngle = this.endAngle,
        diffAngle = (endAngle - startAngle),
        wholeCircle = (diffAngle >= 360 || (diffAngle != 0 && Math.round(diffAngle) % 360 == 0));

    // Equation of a circle is:  (x - x0)^2 + (y - y0)^2 = radius^2
    var x0 = centerPoint[0],
        y0 = centerPoint[1];

    var m00 = transform.m00, m01 = transform.m01, m02 = transform.m02,
        m10 = transform.m10, m11 = transform.m11, m12 = transform.m12,
        dm00m01 = isc.Math._hypot(m00, m01),
        halfWidth = radius * dm00m01,
        dm10m11 = isc.Math._hypot(m10, m11),
        halfHeight = radius * dm10m11,
        centerX = (m00 * x0 + m01 * y0 + m02),
        centerY = (m10 * x0 + m11 * y0 + m12),
        minArcX = centerX - halfWidth,
        minArcY = centerY - halfHeight,
        maxArcX = centerX + halfWidth,
        maxArcY = centerY + halfHeight;

    var box = outputBox || new Array(4);
    if (wholeCircle) {
        box[0] = minArcX;
        box[1] = minArcY;
        box[2] = maxArcX;
        box[3] = maxArcY;
    } else {
        
        var startX = x0 + radius * Math.cos(startAngle * convert),
            startY = y0 + radius * Math.sin(startAngle * convert),
            endX = x0 + radius * Math.cos(endAngle * convert),
            endY = y0 + radius * Math.sin(endAngle * convert),
            // The coordinates of the transformed start and end points are:
            newStartX = m00 * startX + m01 * startY + m02,
            newStartY = m10 * startX + m11 * startY + m12,
            newEndX = m00 * endX + m01 * endY + m02,
            newEndY = m10 * endX + m11 * endY + m12,
            // The two points' angles with respect to `(centerX,centerY)` are:
            newStartAngle = Math.atan2(newStartY - centerY, newStartX - centerX),
            newEndAngle = Math.atan2(newEndY - centerY, newEndX - centerX),
            // The bounding box must contain the three transformed points and so the following
            // are minimum dimensions of the bounding box:
            minPointX = Math.min(newStartX, newEndX, centerX),
            minPointY = Math.min(newStartY, newEndY, centerY),
            maxPointX = Math.max(newStartX, newEndX, centerX),
            maxPointY = Math.max(newStartY, newEndY, centerY);
        
        // compensate for rounding effects
        var tmp = minArcX;
        minArcX = Math.min(minArcX, minPointX);
        minPointX = Math.max(tmp, minPointX);
        tmp = minArcY;
        minArcY = Math.min(minArcY, minPointY);
        minPointY = Math.max(tmp, minPointY);
        tmp = maxPointX;
        maxPointX = Math.min(maxPointX, maxArcX);
        maxArcX = Math.max(tmp, maxArcX);
        tmp = maxPointY;
        maxPointY = Math.min(maxPointY, maxArcY);
        maxArcY = Math.max(tmp, maxArcY);

        var temp = radius * (m00 * m10 + m01 * m11),
            tempXy = (dm00m01 <= 0 ? 0 : (temp / dm00m01)),
            tempYx = (dm10m11 <= 0 ? 0 : (temp / dm10m11)),
            // The min/max points along the arc are `(minArcX,minArcXy)`, `(minArcYx,minArcY)`,
            // `(maxArcX,maxArcXy)`, and `(maxArcYx,maxArcY)`.
            minArcXy = (centerY - tempXy),
            minArcYx = (centerX - tempYx),
            maxArcXy = (centerY + tempXy),
            maxArcYx = (centerX + tempYx),
            // Calculate the angles at which the min/max x/y occur along the arc.
            minArcXTheta = Math.atan2(minArcXy - centerY, minArcX - centerX),
            minArcYTheta = Math.atan2(minArcY - centerY, minArcYx - centerX),
            maxArcXTheta = Math.atan2(maxArcXy - centerY, maxArcX - centerX),
            maxArcYTheta = Math.atan2(maxArcY - centerY, maxArcYx - centerX);

        var wrap = (newStartAngle > newEndAngle);
        if (wrap ?
            (newStartAngle < minArcXTheta || minArcXTheta < newEndAngle) :
            (newStartAngle < minArcXTheta && minArcXTheta < newEndAngle))
        {
            box[0] = minArcX;
        } else {
            box[0] = minPointX;
        }
        if (wrap ?
            (newStartAngle < minArcYTheta || minArcYTheta < newEndAngle) :
            (newStartAngle < minArcYTheta && minArcYTheta < newEndAngle))
        {
            box[1] = minArcY;
        } else {
            box[1] = minPointY;
        }
        if (wrap ?
            (newStartAngle < maxArcXTheta || maxArcXTheta < newEndAngle) :
            (newStartAngle < maxArcXTheta && maxArcXTheta < newEndAngle))
        {
            box[2] = maxArcX;
        } else {
            box[2] = maxPointX;
        }
        if (wrap ?
            (newStartAngle < maxArcYTheta || maxArcYTheta < newEndAngle) :
            (newStartAngle < maxArcYTheta && maxArcYTheta < newEndAngle))
        {
            box[3] = maxArcY;
        } else {
            box[3] = maxPointY;
        }
    }

    // Adjust the bounding box by the lineWidth and/or hitTolerance.
    var halfLineWidth = (includeStroke && this._hasStroke() ? (this.lineWidth / 2) : 0),
        hitTolerance = (includeHitTolerance ? this.hitTolerance : 0),
        strokeRadius = halfLineWidth + hitTolerance,
        halfStrokeRadiusWidth = dm00m01 * strokeRadius,
        halfStrokeRadiusHeight = dm10m11 * strokeRadius;
    box[0] -= halfStrokeRadiusWidth;
    box[1] -= halfStrokeRadiusHeight;
    box[2] += halfStrokeRadiusWidth;
    box[3] += halfStrokeRadiusHeight;

    return box;
},

showResizeKnobs : null,
hideResizeKnobs : null,


_normalizeLinearGradient : function (def) {
    var center = this.centerPoint,
        radius = this.radius,
        boundingBox = [center[0], center[1], center[0] + radius, center[1] + radius];
    return isc.DrawItem._normalizeLinearGradient(def, boundingBox);
},
_normalizeRadialGradient : function (def) {
    var center = this.getCenter(),
        radius = this.radius,
        boundingBox = [center[0], center[1], center[0] + radius, center[1] + radius];
    return isc.DrawItem._normalizeRadialGradient(def, boundingBox, center);
},

//> @method drawSector.getCenter()
// Returns the sector's +link{DrawSector.centerPoint,centerPoint}.
// @return (Point) the current centerPoint
// @visibility drawing
//<
getCenter : function () {
    return this.centerPoint.duplicate();
},

//> @method drawSector.getArcMidpoint()
// Calculates the coordinates of the midpoint of this DrawSector's circular arc. The formula
// for this point is:
// <blockquote>
// <pre>var averageAngle = (startAngle + endAngle) / 2; // in degrees
//[centerX + radius * cosdeg(averageAngle), centerY + radius * sindeg(averageAngle)]</pre>
// </blockquote>
// @return (Point) the coordinates of the midpoint of the arc.
// @visibility drawing
//<
getArcMidpoint : function () {
    return isc.DrawSector.getArcMidpoint(this.centerPoint[0], this.centerPoint[1],
                                         this.startAngle,
                                         this.endAngle,
                                         this.radius);
},

_vmlArcConfigAdapter: new isc.VMLRenderer.shapeConfig(),
_renderVML : function (vmlRenderer, id, conversionContext) {
    var config = this._vmlArcConfigAdapter;
    isc.VMLRenderer.drawItemToShapeConfig(this, config);
    this._vmlRendererHandle = vmlRenderer.shape(id, config);
},
_getPathVML : function (path) {
    var centerPoint = this.centerPoint,
        cx = centerPoint[0],
        cy = centerPoint[1],
        r = this.radius,
        startAngle = this.startAngle,
        diffAngle = (this.endAngle - startAngle),
        wholeCircle = (diffAngle >= 360 || (diffAngle != 0 && Math.round(diffAngle) % 360 == 0));
    if (wholeCircle) {
        diffAngle = 360;
    } else {
        diffAngle = (360 + (diffAngle % 360)) % 360;
        if (diffAngle < 0) {
            startAngle = -startAngle;
            diffAngle = 360 - diffAngle;
        }
    }

    if (wholeCircle && this._avoidRadialLineSegment) {
        path.al(cx, cy, r, r, 0, 360);
    } else {
        path.m(cx, cy);
        // Multiply the angles by -1 to convert positive being clockwise to positive being
        // counter-clockwise.
        path.ae(cx, cy, r, r, -startAngle, -diffAngle);
        path.x();
    }
},

getAttributesSVG : function () {
    return "d='" + this.getPathSVG() + "'";
},

getPathSVG : function () {
    var startAngle = this.startAngle * this._radPerDeg,
        endAngle = this.endAngle * this._radPerDeg,
        diffAngle = (this.endAngle - this.startAngle),
        radius = this.radius,
        cx = this.centerPoint[0],
        cy = this.centerPoint[1],
        largeArcFlag = ((360 + (diffAngle % 360)) % 360 > 180 ? "1" : "0"),
        wholeCircle = (diffAngle >= 360 || (diffAngle != 0 && Math.round(diffAngle) % 360 == 0)),
        c = Math.cos(startAngle),
        s = Math.sin(startAngle),
        x0 = cx + radius * c,
        y0 = cy + radius * s;
    if (wholeCircle) {
        var x1 = cx - radius * c,
            y1 = cy - radius * s,
            buffer = isc.StringBuffer.create();
        if (this._avoidRadialLineSegment) {
            buffer.append("M", x0, " ", y0);
        } else {
            buffer.append("M", cx, " ", cy, " L", x0, " ", y0);
        }
        buffer.append(
            " A", radius, " ", radius, " 0 ", largeArcFlag, " 1 ", x1, " ", y1,
            " A", radius, " ", radius, " 0 ", largeArcFlag, " 1 ", x0, " ", y0,
            " Z");
        return buffer.release(false);
    } else {
        var x1 = cx + radius * Math.cos(endAngle),
            y1 = cy + radius * Math.sin(endAngle);
        return isc.SB.concat(
            "M", cx, " ", cy,
            " L", x0, " ", y0,
            " A", radius, " ", radius, " 0 ", largeArcFlag, " 1 ", x1, " ", y1,
            " Z");
    }
},

drawBitmapPath : function (context) {
    var startAngle = this.startAngle * this._radPerDeg,
        endAngle = this.endAngle * this._radPerDeg,
        radius = this.radius,
        x = this.centerPoint[0],
        y = this.centerPoint[1];

    
    var flag = this._avoidRadialLineSegment;
    if (flag) {
        var diffAngle = (this.endAngle - this.startAngle);
        flag = (
            diffAngle >= 360 || (diffAngle != 0 && Math.round(diffAngle) % 360 == 0));
    }

    if (flag) {
        this.bmMoveTo(x + radius, y, context);
        this.bmArc(x, y, radius, 0, Math.PI, context);
        this.bmArc(x, y, radius, Math.PI, 2 * Math.PI, context);
    } else {
        this.bmMoveTo(x, y, context);
        this.bmArc(x, y, radius, startAngle, endAngle, context);
    }
    context.closePath();
}

//> @method drawSector.setCenterPoint()
// Change the center point for this sector.
// @param left (int) X coordinate of the center point (in the global coordinate system).
// @param top (int) Y coordinate of the center point (in the global coordinate system.
// @visibility drawing
//<

//> @method drawSector.moveBy()
// Move the DrawSector by the specified amounts.
// @param x (int) number of pixels to move by horizontally
// @param y (int) number of pixels to move by vertically
// @visibility drawing
//<

}); // end DrawSector.addProperties

isc.DrawSector.markUnsupportedMethods(null, ["setStartArrow", "setEndArrow"]);








//------------------------------------------------------------------------------------------
//> @class DrawLabel
//
//  DrawItem subclass to render a single-line text label.
//
//  @inheritsFrom DrawItem
//  @treeLocation Client Reference/Drawing/DrawItem
//  @visibility drawing
//<
//------------------------------------------------------------------------------------------

isc.defineClass("DrawLabel", "DrawItem").addClassProperties({

    //> @classAttr DrawLabel.defaultCursiveFont (String : "Zapf-Chancery" : IRW)
    // This setting exists due to platform limitations in some versions of Internet Explorer
    // where the browser does not recognize the five generic font families
    // (<code>cursive</code>, <code>fantasy</code>, <code>monospace</code>,
    // <code>sans-serif</code>, and <code>serif</code>) and instead uses a non-configurable,
    // default font.  This setting specifies a specific font to replace the
    // <code>cursive</code> keyword should a DrawLabel list it in its
    // +link{drawLabel.fontFamily,font family}.
    // @visibility drawing
    //<
    defaultCursiveFont: "Zapf-Chancery",

    //> @classAttr DrawLabel.defaultFantasyFont (String : "Western" : IRW)
    // This setting exists due to platform limitations in some versions of Internet Explorer
    // where the browser does not recognize the five generic font families
    // (<code>cursive</code>, <code>fantasy</code>, <code>monospace</code>,
    // <code>sans-serif</code>, and <code>serif</code>) and instead uses a non-configurable,
    // default font.  This setting specifies a specific font to replace the
    // <code>fantasy</code> keyword should a DrawLabel list it in its
    // +link{drawLabel.fontFamily,font family}.
    // @visibility drawing
    //<
    defaultFantasyFont: "Western",

    //> @classAttr DrawLabel.defaultMonospaceFont (String : "Courier" : IRW)
    // This setting exists due to platform limitations in some versions of Internet Explorer
    // where the browser does not recognize the five generic font families
    // (<code>cursive</code>, <code>fantasy</code>, <code>monospace</code>,
    // <code>sans-serif</code>, and <code>serif</code>) and instead uses a non-configurable,
    // default font.  This setting specifies a specific font to replace the
    // <code>monospace</code> keyword should a DrawLabel list it in its
    // +link{drawLabel.fontFamily,font family}.
    // @visibility drawing
    //<
    defaultMonospaceFont: "Courier",

    //> @classAttr DrawLabel.defaultSansSerifFont (String : "Helvetica" : IRW)
    // This setting exists due to platform limitations in some versions of Internet Explorer
    // where the browser does not recognize the five generic font families
    // (<code>cursive</code>, <code>fantasy</code>, <code>monospace</code>,
    // <code>sans-serif</code>, and <code>serif</code>) and instead uses a non-configurable,
    // default font.  This setting specifies a specific font to replace the
    // <code>sans-serif</code> keyword should a DrawLabel list it in its
    // +link{drawLabel.fontFamily,font family}.
    // @visibility drawing
    //<
    defaultSansSerifFont: "Helvetica",

    //> @classAttr DrawLabel.defaultSerifFont (String : "Times" : IRW)
    // This setting exists due to platform limitations in some versions of Internet Explorer
    // where the browser does not recognize the five generic font families
    // (<code>cursive</code>, <code>fantasy</code>, <code>monospace</code>,
    // <code>sans-serif</code>, and <code>serif</code>) and instead uses a non-configurable,
    // default font.  This setting specifies a specific font to replace the
    // <code>serif</code> keyword should a DrawLabel list it in its
    // +link{drawLabel.fontFamily,font family}.
    // @visibility drawing
    //<
    defaultSerifFont: "Times",


    suppressArbitraryRotationWarning: false,
    _checkRotation : function (rotation) {
        if (!this.suppressArbitraryRotationWarning && rotation) {
            rotation = ((rotation % 360) + 360) % 360;
            if (rotation != 0 && rotation != 90) {
                this.logWarn("Arbitrary rotation is not supported in IE8 and earlier. Only rotation by 0 or 90 degrees is supported.");
                this.suppressArbitraryRotationWarning = true;
            }
        }
    },

    _newlineRegExp: /\r\n|\r|\n/g
});

isc.DrawLabel.addProperties({

//> @attr drawLabel.knobs
// DrawLabel only supports the
// <smartclient>"move"</smartclient>
// <smartgwt>{@link com.smartgwt.client.types.KnobType#MOVE}</smartgwt>
// knob type.
// @see DrawItem.knobs
// @include DrawItem.knobs
//<

//> @attr drawLabel.contents (String : null : IRW)
// This is the content that will exist as the label.
// @visibility drawing
//<

//> @attr drawLabel.left (int : 0 : IR)
// Sets the amount from the left of its positioning that the element should be placed.
// @visibility drawing
//<
left:0, 

//> @attr drawLabel.top (int : 0 : IR)
// Sets the amount from the top of its positioning that the element should be placed.
// @visibility drawing
//<
top:0,
//> @attr drawLabel.alignment (String : "start" : IR)
// Sets the text alignment from the x position. Similar to html5 context.textAlign, eg "start", "center", "end"
// @visibility drawing
//<
alignment: 'start',

//> @attr drawLabel.fontFamily (String : "Tahoma" : IR)
// Font family name, similar to the CSS font-family attribute.
// @visibility drawing
//<
fontFamily:"Tahoma",

//> @attr drawLabel.fontSize (int : 18 : IRW)
// Font size in pixels, similar to the CSS font-size attribute.
// @visibility drawing
//<
fontSize:18,

//> @attr drawLabel.fontWeight (String : "bold" : IR)
// Font weight, similar to the CSS font-weight attribute, eg "normal", "bold".
// @visibility drawing
//<
fontWeight:"bold",

//> @attr drawLabel.fontStyle (String : "normal" : IR)
// Font style, similar to the CSS font-style attribute, eg "normal", "italic".
// @visibility drawing
//<
fontStyle:"normal",

//> @attr drawLabel.lineColor
// The text color of the label.
// @include DrawItem.lineColor
//<


//> @attr drawLabel.rotation
// Rotation in degrees about the +link{top,top} +link{left,left} corner.  The positive
// direction is clockwise.
// @include DrawItem.rotation
//<


init : function () {
    this._setContentLines();
    this._setLineHeight();

    // `exemptFromGlobalTransform` is not implemented for DrawLabel.
    this.exemptFromGlobalTransform = false;

    this.Super("init", arguments);
    if (!this.useTextpath && this.suppressArbitraryRotationWarning != true) {
        isc.DrawLabel._checkRotation(this.rotation);
    }
},

setCursor : function (newCursor) {
    this.Super("setCursor", arguments);
    if (this._htmlText != null) {
        this._htmlText.setCursor(newCursor);
    }
},

setCanDrag : function (canDrag) {
    this.Super("setCanDrag", arguments);
    if (this._htmlText != null) {
        this._htmlText.setProperty("canDragReposition", canDrag);
    }
},

setDragRepositionCursor : function (dragRepositionCursor) {
    this.Super("setDragRepositionCursor", arguments);
    if (this._htmlText != null) {
        this._htmlText.setProperty("dragRepositionCursor", dragRepositionCursor);
    }
},


isPointInPath : isc.DrawItem.getInstanceProperty("isInBounds"),

showResizeKnobs : null,
hideResizeKnobs : null,

//----------------------------------------
//  DrawLabel renderers
//----------------------------------------


useTextpath: true,

//synchTextMove:true,
_getElementVML : function (buffer, id, conversionContext) {

    if (this.useTextpath) {
        return this.Super("_getElementVML", arguments);
    } else if (this.synchTextMove) {
        // TEXTBOX implementation
        var styleQuote = isc.DrawPane._getEnclosingQuote(this.fontFamily);
        buffer.append(
            this.drawPane.startTagVML("RECT")," ID='", id,
            "' STYLE='position:absolute;left:", this.left * this.drawPane._pow10,
            "px; top:", this.top * this.drawPane._pow10,
            
            (this.alignment ? "px; text-align:" + (this.alignment == "start" ? "left" : "right"): "px"),
            ";'>",this.drawPane.startTagVML("TEXTBOX")," INSET='0px, 0px, 0px, 0px' STYLE=", styleQuote, "overflow:visible",
            
            //(this.rotation != 0 ? ";rotation:" + 90 : ""),
            // NOTE: manual zoom
            "; font-size:", this.fontSize * this.drawPane.zoomLevel,
            "px; font-weight:", this.fontWeight,
            "; font-style:", this.fontStyle,
            "; font-family:", this.fontFamily,
            ";", styleQuote, "><NOBR style='color:", this.lineColor, "'>",
            this.contents == null ? null : String(this.contents).asHTML(),
            "</NOBR>",
            this.drawPane.endTagVML("TEXTBOX"),
            this.drawPane.endTagVML("RECT"));
    } else if (conversionContext == null) {
        // DIV implementation
        var screenCoords = this.drawPane.viewbox2global([this.left, this.top, 0, 0]),
            left = screenCoords[0],
            top = screenCoords[1],
            labelDims = this.drawPane.measureLabel(this.contents, this),
            rotation = (((this.rotation == null ? 0 : this.rotation) % 360) + 360) % 360,
            useWritingModeTbRl = rotation != 0;

        if (useWritingModeTbRl) {
            
            if (isc.Browser.isIE && isc.Browser.version >= 7 && isc.Browser.isStrict) {
                // the height as returned by measureLabel() will be the width of label when it's rotated
                if (rotation > 180) left += labelDims.height;
            } else {
                if (rotation <= 180) left -= labelDims.height;
            }
            if (rotation > 180) {
                top -= labelDims.width;
            }
        }

        var styleQuote = isc.DrawPane._getEnclosingQuote(this.fontFamily);
        buffer.append(
             "<DIV ID='", id,
            "' STYLE=", styleQuote, "position:absolute; overflow:visible; width:1px; height:1px",
            // the top-level VML container is relatively positioned hence 0,0 in drawing space
            // is inside the CSS padding of the draw pane.  This DIV is absolutely positioning
            // hence its 0,0 would be inside the border.
            "; left:", left, "px; top:", top, "px",
            
            (useWritingModeTbRl ? ";writing-mode: tb-rl" : ""),
            "; font-size:", (this.fontSize * this.drawPane.zoomLevel),
            "px; font-weight:", this.fontWeight,
            "; font-style:", this.fontStyle,
            "; font-family:", this.fontFamily,
            ";", styleQuote, "><NOBR style='color:", this.lineColor, "'>", this.contents == null ? null : String(this.contents).asHTML(), "</NOBR></DIV>");
    } else {
        conversionContext.drawLabelsAccumulator.add(this);
    }
},

_vmlConfigAdapter: new isc.VMLRenderer.shapeConfig(),
_renderVML : function (vmlRenderer, id, conversionContext) {
    
    var config = this._vmlConfigAdapter;
    isc.VMLRenderer.drawItemToShapeConfig(this, config);
    config.stroked = false;
    config.filled = true;
    config.fillColor = this.lineColor;
    var isRtl = this.drawPane.isRTL();
    if (this.alignment == "start") {
        config.textAlign = (isRtl ? "right" : "left");
    } else if (this.alignment == "center") {
        config.textAlign = "center";
    } else if (this.alignment == "end") {
        config.textAlign = (isRtl ? "left" : "right");
    } else {
        config.textAlign = null;
    }
    config.fontSize = this.fontSize + "px";
    this._vmlRendererHandle = vmlRenderer.text(id, config);
},

_getVMLTextHandle : function () {
    
    var vmlTextHandle = this._vmlTextHandle;
    if (vmlTextHandle != null) return vmlTextHandle;
    var vmlHandle = this._getVMLHandle();
    if (vmlHandle == null) return null;
    if (this.synchTextMove) {
        vmlTextHandle = vmlHandle.firstChild.style; // VML TEXTBOX
    } else {
        vmlTextHandle = vmlHandle.style; // external DIV
    }
    return (this._vmlTextHandle = vmlTextHandle);
},

// drawLabel.getSvgString()
getSvgString : function (conversionContext) {
    conversionContext = conversionContext || isc.SVGStringConversionContext.create();
    var left = this.left,
        top = this.top;
    if (this.alignment === "center") {
        left += this.getTextWidth() / 2;
    } else if (this.alignment === "end") {
        left += this.getTextWidth();
    }
    var rotation = this.rotation,
        
        y = (conversionContext._printForServerExport !== false || isc.Browser.isOpera || isc.Browser.isIE
             ? top + this.fontSize
             : top),
        fontFamilyQuote = isc.DrawPane._getEnclosingQuote(this.fontFamily),
        svgString = "<text";
    var svgHandleID = this._getSvgHandleID();
    if (svgHandleID != null) svgString += " id='" + svgHandleID + "'";
    if (!conversionContext.printForExport) {
        svgString += " " + this._getEventProxyAttribute();
    }
    svgString += (
        " x='" + left +
        "' y='" + y +
        "' dominant-baseline='text-before-edge" +
        
        "' font-family=" + fontFamilyQuote + this.fontFamily + fontFamilyQuote +
        " font-size='" + this.fontSize + "px" +
        "' font-weight='" + this.fontWeight +
        "' font-style='" + this.fontStyle +
        "' fill='" + this.lineColor
        );
    if (this.shadow) {
        var shadowFilterID = this._getSVGShadowFilterID();
        if (!conversionContext.defSvgStrings[shadowFilterID]) {
            conversionContext.defSvgStrings[shadowFilterID] = this._getShadowFilterSvgString(shadowFilterID, this.shadow);
        }
        svgString += "' filter='url(#" + shadowFilterID + ")";
    }
    if (rotation) {
        
        svgString += "' transform='rotate(" + rotation + " " + left + " " + top + ")";
    }
    svgString += "'";
    var attributesSVG = this.getAttributesSVG();
    if (attributesSVG) svgString += " " + attributesSVG;
    if (this.alignment == "start") {
        svgString += " text-anchor='start'";
    } else if (this.alignment == "center") {
        svgString += " text-anchor='middle'";
    } else if (this.alignment == "end") {
        svgString += " text-anchor='end'";
    }
    
    svgString += ">" + this._getInnerSvgString(left) + "</text>";
    return svgString;
},
_getInnerSvgString : function (left) {
    var contents = this.contents;
    if (contents == null) return "";

    contents = String(contents);

    var contentLines = this._contentLines,
        numContentLines;
    if (contentLines == null || (numContentLines = contentLines.length) <= 1) {
        return isc.makeXMLSafe(contents);

    } else {
        // Handle multiple lines of text using SVG's <tspan> elements.
        var svgString = "",
            lineHeight = this._lineHeight;
        for (var i = 0, j = 1; i < numContentLines; ++i) {
            var line = contentLines[i];
            if (line == "") {
                // Increment the count of consecutive newlines.
                ++j;

            } else {
                svgString += (
                    "<tspan x='" + left +
                    "' dy='" + (lineHeight * j) +
                    "'>" +
                    isc.makeXMLSafe(line) +
                    "</tspan>"
                    );

                j = 1;
            }
        }
        return svgString;
    }
},

// drawLabel._moveTo()
_moveTo : function (left, top, cx0, cy0) {
    if (left == null) left = this.left;
    if (top == null) top = this.top;

    

    var sameLT = false;
    if (this.left == left &&
        this.top == top)
    {
        sameLT = true;
        if (cx0 == null && cy0 == null) return;
    }

    if (cx0 == null || cy0 == null) {
        var center0 = this._getRotationCenter();
        if (sameLT &&
            (cx0 == null || cx0 == center0.cx) &&
            (cy0 == null || cy0 == center0.cy))
        {
            return;
        }
        cx0 = center0.cx;
        cy0 = center0.cy;
    }

    // This is modeled after DrawRect.setRect(), minus the setting of the width and height part.

    this.left = left;
    this.top = top;

    var center = this._getRotationCenter();
    if (sameLT &&
        cx0 == center.cx &&
        cy0 == center.cy)
    {
        return;
    }
    this._updateRotationCenter(cx0, cy0, center.cx, center.cy);

    if (this.drawingVML) {
        if (this.useTextpath) {
            var vmlRendererHandle = this._vmlRendererHandle;
            if (cx0 != center.cx || cy0 != center.cy) {
                this._transform = null;
                vmlRendererHandle.setProperty("transform", this._getLocalTransform());
            }
            vmlRendererHandle.setProperty("left", this.left).setProperty("top", this.top).flush();
        } else {
            var vmlHandleStyle = this._getVMLHandle().style;
            if (this.synchTextMove) {
                vmlHandleStyle.left = (this.left * this.drawPane._pow10) + isc.px;
                vmlHandleStyle.top = (this.top * this.drawPane._pow10) + isc.px;
            } else {
                vmlHandleStyle.left = this.left + isc.px;
                vmlHandleStyle.top = this.top + isc.px;
            }
        }

    } else if (this.drawingSVG) {
        var svgHandle = this._svgHandle;
        if (cx0 != center.cx || cy0 != center.cy) {
            this._transform = null;
            var t = this._getLocalTransform();
            svgHandle.setAttributeNS(null, "transform", "matrix(" + t.m00 + " " + t.m10 + " " +
                                                                    t.m01 + " " + t.m11 + " " +
                                                                    t.m02 + " " + t.m12 + ")");
        }

        var left = this.left,
            top = this.top;
        if (this.alignment === "center") {
            left += this.getTextWidth() / 2;
        } else if (this.alignment === "end") {
            left += this.getTextWidth();
        }
        
        if (isc.Browser.isOpera || isc.Browser.isIE) {
            top += this.fontSize;
        }
        svgHandle.setAttributeNS(null, "x", left);
        svgHandle.setAttributeNS(null, "y", top);

        // Iterate the <tspan> children of the <text> element, updating their 'x' attributes.
        var children = svgHandle.childNodes,
            numChildren = children.length;
        for (var i = 0; i < numChildren; ++i) {
            var child = children[i];
            if (child.nodeType != 1 || child.nodeName != "tspan") continue;
            child.setAttributeNS(null, "x", left);
        }


    } else if (this.drawingBitmap) {
        if (this._useHTML()) {
            if (this._htmlText != null) {
                // The "live update" to the htmlText component requires a newly computed local transform.
                this._transform = null;

                isc.Element._updateTransformStyle(this._htmlText, this._htmlText._getTransformFunctions());
            }
        } else {
            this.drawPane.redrawBitmap();
        }
    }
    this._reshaped();
},

// drawLabel._updateLocalTransform()

_updateLocalTransform : function (transform, cx, cy, initialShape, fireReshaped, viaLocalTransformOnly) {
    
    if (initialShape == null) {
        initialShape = this;
    }
    if (!viaLocalTransformOnly) {
        var m = initialShape._getTextMeasurements(true, true),
            textWidth = m.width,
            textHeight = m.height,
            info = isc.DrawItem._fitBestRect(
                transform, cx, cy,
                initialShape.left, initialShape.top, textWidth, textHeight,
                0);
        if (info.success) {
            this.Super("_updateLocalTransform", [transform, cx, cy, initialShape, false, true], arguments);
            this._moveTo(isc.DrawItem._makeCoordinate(info.left + (info.width - textWidth) / 2),
                         isc.DrawItem._makeCoordinate(info.top + (info.height - textHeight) / 2),
                         isc.DrawItem._makeCoordinate(cx),
                         isc.DrawItem._makeCoordinate(cy));
            return;
        }
    }
    this.Super("_updateLocalTransform", [transform, cx, cy, initialShape, fireReshaped, true], arguments);

    
},

rotateBy : function (degrees) {
    if (this.useTextpath) {
        return this.Super("rotateBy", arguments);
    } else {
        this.rotateTo(+this.rotation + degrees);
    }
},

rotateTo : function (degrees) {
    if (this.useTextpath) {
        return this.Super("rotateTo", arguments);
    } else {
        isc.DrawLabel._checkRotation(degrees);
        this.rotation = degrees;
        this._redrawAfterModifyingLocalTransform();
    }
},

//> @method drawLabel.getCenter()
// Get the center point of the label.
// @return (Point) the center point
// @visibility drawing
//<
getCenter : function () {
    var m = this._getTextMeasurements(true, true),
        textWidth = m.width,
        textHeight = m.height;
    return [this.left + isc.DrawItem._makeCoordinate(textWidth / 2), this.top + isc.DrawItem._makeCoordinate(textHeight / 2)];
},

//> @method drawLabel.getBoundingBox()
// Returns the top, left, top + textHeight, left + textWidth
// @return (Array[] of double) x1, y1, x2, y2 coordinates
// @visibility drawing
//<
getBoundingBox : function (includeStroke, outputBox) {
    
    var m = this._getTextMeasurements(true, true),
        textWidth = m.width,
        textHeight = m.height,
        box = (outputBox || new Array(4));
    box[0] = this.left;
    box[1] = this.top;
    box[2] = this.left + textWidth;
    box[3] = this.top + textHeight;
    return box;
},

_getHtmlTextContents : function () {
    // Same as in DrawPane.measureLabel().
    var contents;
    if (this.contents != null) {
        contents = String(this.contents)
            .replace(isc.DrawPane._spaceCharsRegExp, "\u0020").asHTML();
    }
    var styleQuote = isc.DrawPane._getEnclosingQuote(this.fontFamily);
    return isc.SB.concat(
        "<span style=", styleQuote, "font-weight:", this.fontWeight,
        ";font-size:", this.fontSize, "px;font-style:", this.fontStyle,
        ";white-space:pre;font-family:", this.fontFamily, styleQuote, ">", contents, "</span>");
},

makeHTMLText : function () {
    var label = this._htmlText = isc.HTMLFlow.create({
        creator: this,
        left: 0, top: 0,
        width:1, height:1,
        cursor: this.cursor,
        canDragReposition: this.canDrag,
        dragAppearance: "target",
        dragRepositionCursor: this.dragRepositionCursor,
        contents: this._getHtmlTextContents(),
        canSelectText: false,
        textColor: this.lineColor,
        autoDraw: false,
        _getTransformFunctions : function () {
            var creator = this.creator,
                drawPane = creator.drawPane;
            

            var transformFunctions = " translate(" + creator.left + "px, " + creator.top + "px)";

            
            var t = creator._getNormalizeTransform("local", "global");
            var precision = 7;
            transformFunctions = " matrix(" + t.m00.toFixed(precision) + ", " +
                                              t.m10.toFixed(precision) + ", " +
                                              t.m01.toFixed(precision) + ", " +
                                              t.m11.toFixed(precision) + ", " +
                                              t.m02.toFixed(precision) + ", " +
                                              t.m12.toFixed(precision) + ")" + transformFunctions;

            transformFunctions = "translate(" + drawPane.getLeftPadding() + "px, " +
                                                drawPane.getTopPadding() + "px)" + transformFunctions;

            return transformFunctions;
        },
        getTransformCSS : function () {
            var transformFunctions = this._getTransformFunctions();
            return (
                ";" +
                isc.Element._transformCSSName + ":" + transformFunctions + ";" +
                isc.Element._transformOriginCSSName + ":0 0;"
            );
        },
        dragRepositionStop : function () {
            var creator = this.creator,
                drawPane = creator.drawPane;

            

            // At the end of a drag, this HTMLFlow's left and top have been changed as part of the
            // effect of dragAppearance:"target". For example, if the HTMLFlow is dragged 5px
            // down and 10px to the left, then getLeft() will be -10 and getTop() will be 5.
            // What we want to do is convert this translation in screen coordinates to a transform
            // in drawing coordinates (to update the position of the DrawLabel):
            //
            //     T|left,top * G = G * T|X
            //
            // .. where `G' is the global transform matrix, `T|left,top' is the translation matrix
            // by getLeft()/getTop(), and `T|X' is the transformation matrix that we want to solve for.
            // We have:
            //
            //     T|X = G^-1 * T|left,top * G
            //
            // In mathematical terms, `T|X' and `T|left,top' are similar matrices.
            // http://mathworld.wolfram.com/SimilarityTransformation.html
            //
            // Similar matrices represent the same linear transformation after a change of basis.
            // http://planetmath.org/similarmatrix
            // Thus, `T|X' is a translation matrix.

            // Since `T|X' is a translation matrix, we just move the DrawLabel by m02, m12.
            var t = (
                drawPane._getInverseGlobalTransform()
                    .translate(this.getLeft(), this.getTop())
                    .rightMultiply(drawPane._getGlobalTransform()));
            creator.moveBy(t.m02, t.m12);

            // Reset our left, top back to 0, 0.
            this.moveTo(0, 0);
        }
    });
    this.drawPane.addChild(label);
},


_useHTML : function () {
    return this.drawingBitmap && (this.useHTML != null ? this.useHTML : isc.Browser.isIPhone);
},

drawBitmap : function (context) {
    var drawBitmapState = this.drawPane._drawBitmapState;
    
    if ((drawBitmapState == null || drawBitmapState._tmpBitmapContext == null) && this._useHTML()) {
        // option to render as HTML.  Needed for some older browsers or on mobile devices so
        // that the text is not blurry.
        if (this._htmlText == null) {
            this.makeHTMLText();

        // update the htmlText transform
        } else {
            isc.Element._updateTransformStyle(this._htmlText, this._htmlText._getTransformFunctions());
        }
    } else {
        this.Super("drawBitmap", arguments);
    }
},

erase : function () {
    if (this._htmlText != null) this._htmlText.destroy();
    this.Super("erase", arguments);
},

destroy : function () {
    if (this._htmlText != null) this._htmlText.destroy();
    this.Super("destroy", arguments);
},

getFontString : function () {
    return (this.fontStyle != "normal" ? this.fontStyle + " " : "") +
                     (this.fontWeight != "normal" ? this.fontWeight + " " : "") +
                     this.fontSize + "px " + this.fontFamily;
},


_calculateAlignMiddleCorrection : function () {
    if (!(isc.Browser.isFirefox && isc.Browser.hasCANVAS && this._verticalAlignMiddle)) {
        return 0;
    } else {
        var cache = this.drawPane._alignMiddleCorrectionCache;
        if (cache == null) {
            cache = this.drawPane._alignMiddleCorrectionCache = {};
        }

        var font = this.getFontString();
        if (cache.hasOwnProperty(font)) {
            return cache[font];
        }

        var measureCanvas = isc.DrawPane._getMeasureCanvas();
        measureCanvas.setContents("<CANVAS></CANVAS>");
        measureCanvas.redraw();
        var canvas = measureCanvas.getHandle().getElementsByTagName("canvas")[0];
        var context = canvas.getContext("2d");

        context.font = font;
        var text = "The quick, brown fox",
            textMeasure = context.measureText(text),
            textWidth = textMeasure.width,
            textHeight = textMeasure.height || this.fontSize;

        var canvasWidth = context.canvas.width = Math.round(textWidth);
        var canvasHeight = context.canvas.height = Math.round(3 * textHeight);

        // Get the bottom of the text when textBaseline is "middle"
        context.textBaseline = "middle";
        context.font = font;
        context.fillText(text, textHeight, 0);
        var imageData = context.getImageData(0, 0, canvasWidth, canvasHeight),
            data = imageData.data;
        var middleBottom = 0;
        for (var i = data.length - 4; i > 0; i -= 4) {
            if (data[i + 3]) {
                middleBottom = Math.floor(i / (4 * canvasWidth)) - textHeight;
                break;
            }
        }

        // Get the top and bottom of the text when textBaseline is "hanging"
        context.clearRect(0, 0, canvasWidth, canvasHeight);
        context.textBaseline = "hanging";
        context.font = font;
        context.fillText(text, textHeight, 0);
        imageData = context.getImageData(0, 0, canvasWidth, canvasHeight);
        data = imageData.data;
        var hangingBottom = 0;
        for (var i = data.length - 4; i > 0; i -=4) {
            if (data[i + 3]) {
                hangingBottom = Math.floor(i / (4 * canvasWidth)) - textHeight;
                break;
            }
        };
        var top = 0;
        for (var i = 0, dataLength = data.length; i < dataLength; i += 4) {
            if (data[i + 3]) {
                top = Math.floor(i / (4 * canvasWidth)) - textHeight;
            }
        }
        var actualTextHeight = hangingBottom - top;

        return (cache[font] = -Math.round((actualTextHeight - (hangingBottom - middleBottom)) / 2));
    }
},

// drawLabel.drawBitmapPath()

drawBitmapPath : function (context) {

    
    context.textBaseline = "top";

    //this.logWarn("fontString: " + fontString);
    context.font = this.getFontString();

    context.fillStyle = this.lineColor;
    context.textAlign = this.alignment;
    var left0 = this.left,
        top0 = this.top + this._calculateAlignMiddleCorrection();
    if (this.alignment == "center") {
        left0 += this.getTextWidth() / 2;
    } else if (this.alignment == "end") {
        left0 += this.getTextWidth();
    }
    if (this._contentLines == null) {
        this.bmFillText(this.contents, left0, top0, context);
    } else {
        var lineHeight = this._lineHeight;
        if (lineHeight == null) lineHeight = this._setLineHeight(true);
        for (var i = this._contentLines.length; i--; ) {
            this.bmFillText(this._contentLines[i], left0, top0 + i * lineHeight, context);
        }
    }
},

_setLineHeight : function (force) {
    if (force ||
        (!this.drawingVML &&
         (this.drawingBitmap || this.drawingSVG ||
             (this.drawPane && (
                 this.drawPane.drawingType == "bitmap" ||
                 this.drawPane.drawingType == "svg"))) &&
         !this._useHTML()))
    {
        this._lineHeight = (
            this.drawPane.measureLabel("Xy\nXy", this).height -
            this.drawPane.measureLabel("Xy", this).height);
    }
    return this._lineHeight;
},

//----------------------------------------
//  DrawLabel attribute setters
//----------------------------------------

//> @method drawLabel.setLineColor()
// Sets the text color of the label.
// @param color (CSSColor) new text color.
// @include DrawItem.setLineColor()
//<
setLineColor : function (color) {
    this.lineColor = color;
    if (this.drawingVML) {
        if (this.useTextpath) {
            this._vmlRendererHandle.setProperty("fillColor", color).flush();
        } else {
            var nobrElement = this._getVMLHandle().firstChild;
            while (nobrElement != null && nobrElement.tagName != this._$NOBR) {
                nobrElement = nobrElement.firstChild;
            }
            
            if (nobrElement != null) nobrElement.style.color = color;
        }
    } else if (this.drawingSVG) {
        this._svgHandle.setAttributeNS(null, "fill", color);
    } else if (this.drawingBitmap) {
        if (this._useHTML()) {
            if (this._htmlText != null) {
                this._htmlText.setTextColor(color);
            }
        } else {
            this.drawPane.redrawBitmap();
        }
    }
},

//> @method drawLabel.setContents()
// Sets this DrawLabel's +link{contents,contents}.
// @param contents (String) the new contents.
// @visibility drawing
//<
_$NOBR: "NOBR",
setContents : function (contents, fromUpdateTitleLabelAndBackground) {
    if (this.contents == contents) return;
    this.contents = contents;
    this._setContentLines();
    if (this.drawingVML) {
        if (this.useTextpath) {
            this._vmlRendererHandle.setProperty("contents", contents).flush();
        } else {
            var nobrElement = this._getVMLHandle().firstChild;
            while (nobrElement != null && nobrElement.tagName != this._$NOBR) {
                nobrElement = nobrElement.firstChild;
            }
            
            if (nobrElement != null) nobrElement.innerText = contents;
        }

    } else if (this.drawingSVG) {
        var svgHandle = this._svgHandle;
        if (this.alignment === "center") {
            var left = this.left;
            left += this.getTextWidth() / 2;
            svgHandle.setAttributeNS(null, "x", left);
        } else if (this.alignment === "end") {
            var left = this.left;
            left += this.getTextWidth();
            svgHandle.setAttributeNS(null, "x", left);
        }

        var contentLines = this._contentLines,
            numContentLines;
        if (contentLines == null || (numContentLines = contentLines.length) <= 1) {
            svgHandle.textContent = (contents == null ? "" : String(contents));
        } else {
            // Clear the existing content.
            var lastChild;
            while ((lastChild = svgHandle.lastChild) != null) {
                svgHandle.removeChild(lastChild);
            }

            if (svgHandle.insertAdjacentHTML != null) {
                svgHandle.insertAdjacentHTML("beforeend", this._getInnerSvgString(left));
            } else {
                var doc = this.drawPane._getSvgDocument(),
                    lineHeight = this._lineHeight;
                for (var i = 0, j = 1; i < numContentLines; ++i) {
                    var line = contentLines[i];
                    if (line == "") {
                        // Increment the count of consecutive newlines.
                        ++j;
                    } else {
                        var tspanElem = doc.createElementNS(isc._$svgNS, "tspan");
                        tspanElem.setAttributeNS(null, "x", left);
                        tspanElem.setAttributeNS(null, "dy", lineHeight * j);
                        tspanElem.textContent = line;
                        svgHandle.appendChild(tspanElem);

                        j = 1;
                    }
                }
            }
        }

    } else if (this.drawingBitmap) {
        if (this._useHTML()) {
            if (this._htmlText != null) {
                this._htmlText.setContents(this._getHtmlTextContents());
            }
        } else {
            this.drawPane.redrawBitmap();
        }
    }

    this._reshaped();
},

_setContentLines : function () {
    this._contentLines = null;
    var contents = this.contents;
    if (!isc.isA.String(contents)) {
        return;
    }

    
    if (!this.drawingVML &&
        (this.drawingBitmap || this.drawingSVG ||
            (this.drawPane && (
                this.drawPane.drawingType == "bitmap" ||
                this.drawPane.drawingType == "svg"))) &&
        !this._useHTML())
    {
        var newlineRegExp = isc.DrawLabel._newlineRegExp;
        newlineRegExp.lastIndex = 0;
        if (newlineRegExp.test(contents)) {
            this._contentLines = contents.split(newlineRegExp);
        }
    }
},

_getTextMeasurementsOutput: { width: 0, height: 0 },
_getTextMeasurements : function (wantWidth, wantHeight) {
    
    var output = this._getTextMeasurementsOutput;

    if (!(this.drawingVML || this.drawingSVG || this.drawingBitmap)) {
        var m = this.drawPane.measureLabel(this.contents, this);
        if (wantWidth) output.width = m.width;
        if (wantHeight) output.height = m.height;
        return output;
    }

    if (this.drawingVML) {
        if (this.useTextpath) {
            var m = this.drawPane.measureLabel(this.contents, this);
            if (wantWidth) output.width = m.width;
            if (wantHeight) output.height = m.height;
        } else {
            var vmlHandle = this._getVMLHandle();
            if (this.synchTextMove) {
                // VML TEXTBOX
                var firstChild = vmlHandle.firstChild;
                if (wantWidth) output.width = firstChild.scrollWidth;
                if (wantHeight) output.height = firstChild.scrollHeight;
            } else {
                // external DIV
                var rotation = (((this.rotation || 0) % 360) + 360) % 360;
                if (rotation != 0) {
                    // The scrollWidth and scrollHeight are apparently swapped for 90-degree-
                    // rotated labels.
                    if (wantWidth) output.width = vmlHandle.scrollHeight;
                    if (wantHeight) output.height = vmlHandle.scrollWidth;
                } else {
                    if (wantWidth) output.width = vmlHandle.scrollWidth;
                    if (wantHeight) output.height = vmlHandle.scrollHeight;
                }
            }
        }
    } else if (this.drawingSVG && (this.drawPane._drawBitmapState == null ||
                                   this.drawPane._drawBitmapState._tmpBitmapContext == null))
    {
        if (this._svgHandle) {
            // Prefer to use getComputedTextLength(). We could use `getBBox().width', but getBBox()
            // also gets the height - guessing getComputedTextLength() is faster.
            // Note that getComputedTextLength() computes the total X advance, so in cases where
            // there are multiple lines, we need to use `getBBox().width'.
            if (wantWidth && !wantHeight && (this._contentLines == null || this._contentLines.length <= 1)) {
                output.width = this._svgHandle.getComputedTextLength();
            } else {
                var bbox = this._svgHandle.getBBox();
                if (wantWidth) output.width = bbox.width;
                if (wantHeight) output.height = bbox.height;
            }
        } else {
            var m = this.drawPane.measureLabel(this.contents, this);
            if (wantWidth) output.width = m.width;
            if (wantHeight) output.height = m.height;
        }
    } else {
        var drawBitmapState = this.drawPane._drawBitmapState;
        
        if (this._useHTML() && drawBitmapState == null) {
            if (this._htmlText == null) this.makeHTMLText();
            if (wantWidth) output.width = this._htmlText.getScrollWidth();
            if (wantHeight) output.height = this._htmlText.getScrollHeight();
        } else {
            var context,
                saved;
            if (drawBitmapState != null &&
                (context = drawBitmapState._tmpBitmapContext) != null)
            {
                saved = false;
            } else {
                context = this.drawPane.getBitmapContext(true);
                saved = true;
            }
            var font = this.getFontString();
            context.font = font;

            
            var contentLines = this._contentLines,
                textWidth = 0, textHeight = 0;
            if (contentLines == null) {
                var m = context.measureText(this.contents);
                textWidth = m.width;
                textHeight = m.height;
            } else {
                var lineHeight = this._lineHeight,
                    numLines = contentLines.length;
                if (wantWidth) {
                    for (var i = 0; i < numLines; ++i) {
                        var m = context.measureText(contentLines[i]),
                            width = m.width,
                            height = m.height;
                        if (i == 0 || width > textWidth) {
                            textWidth = width;
                        }
                    }
                }
                textHeight = numLines * lineHeight;
            }

            if (saved) {
                context.restore();
            }
            output.width = textWidth;
            output.height = textHeight || this.fontSize;
        }
    }
    return output;
},
getTextWidth : function () {
    return this._getTextMeasurements(true, false).width;
},
getTextHeight : function () {
    return this._getTextMeasurements(false, true).height;
},

//> @method drawLabel.setFontSize()
// Sets this DrawLabel's +link{fontSize,fontSize}.
// @param size (int) the new font size in pixels.
// @visibility drawing
//<
setFontSize: function (size) {
    if (size == null) {
        return;
    }
    this.fontSize = size;
    this._setLineHeight();

    if (this.drawingVML) {
        if (this.useTextpath) {
            this._vmlRendererHandle.setProperty("fontSize", size + "px").flush();
        } else {
            this._getVMLTextHandle().fontSize = (size * this.drawPane.zoomLevel) + "px";
        }
    } else if (this.drawingSVG) {
        this._svgHandle.setAttributeNS(null, "font-size", size + "px");
    } else if (this.drawingBitmap) {
        if (this._useHTML()) {
            if (this._htmlText != null) {
                this._htmlText.setContents(this._getHtmlTextContents());
            }
        } else {
            this.drawPane.redrawBitmap();
        }
    }

    this._reshaped();
},

_setLineWidthVML : isc.Class.NO_OP

}); // end DrawLabel.addProperties

// Mark the DrawItem line and fill attribute setters as unsupported. Same reason as with DrawGroup:
// If someone blindly called the superclass setters, they would error on DrawLabels because e.g.
// there are no VML stroke and fill subelements.
// TODO expand this list as additional setters are added to DrawItem
isc.DrawLabel.markUnsupportedMethods(null, ["setLineWidth", "setLineOpacity",
        "setLinePattern", "setLineCap", "setFillColor", "setFillGradient", "setFillOpacity"]);
isc.DrawLabel.markUnsupportedMethods(null, ["setStartArrow", "setEndArrow"]);








//------------------------------------------------------------------------------------------
//> @class DrawImage
//
//  DrawItem subclass to render embedded images.
//
//  @inheritsFrom DrawItem
//  @treeLocation Client Reference/Drawing/DrawItem
//  @visibility drawing
//<
//------------------------------------------------------------------------------------------

isc.defineClass("DrawImage", "DrawItem").addProperties({
    //> @attr drawImage.left (int : 0 : IRW)
    // @include drawRect.left
    //<
    left:0, 

    //> @attr drawImage.top (int : 0 : IRW)
    // @include drawRect.top
    //<
    top:0, 

    //> @attr drawImage.width    (int : 16 : IRW)
    // @include drawRect.width
    //<
    width:16, 

    //> @attr drawImage.height   (int : 16 : IRW)
    // @include drawRect.height
    //<
    height:16,

    //> @attr drawImage.title  (String : null : IR)
    // Title (tooltip hover text) for this image.
    // @visibility drawing
    //<
    //title:"Untitled Image",

    //> @attr drawImage.src    (URL : "blank.png" : IRW)
    // URL to the image file.
    // @visibility drawing
    //<
    src:"blank.png",

    //> @attr drawImage.useMatrixFilter (Boolean : null : IR)
    // Configures whether a Matrix filter is used to render this DrawImage in Internet
    // Explorer 6-8.
    // <p>
    // With the default of <code>null</code>, matrix filters may be used unless
    // +link{classAttr:Canvas.neverUseFilters} has been set.  When set explicitly to
    // <code>true</code>, matrix filters may be used even if <code>neverUseFilters</code> is
    // <code>true</code>.
    // <p>
    // This setting exists due to platform limitations in Internet Explorer where it is
    // impossible to implement shearing transforms on an image without a matrix filter.  Shear
    // can arise by:
    // <ul>
    // <li>specifying a nonzero +link{drawItem.xShearFactor,xShearFactor} or
    // +link{drawItem.yShearFactor,yShearFactor},</li>
    // <li>specifying a nonuniform +link{drawItem.scale,scale} (where the scale values along
    // the x- and y-dimensions are not equal) and a nonzero +link{drawItem.rotation,rotation},
    // or</li>
    // <li>using <smartclient>"resize"</smartclient>
    // <smartgwt>{@link com.smartgwt.client.types.KnobType#RESIZE}</smartgwt>
    // +link{drawItem.knobs,control knobs} on a rotated DrawImage.
    // </ul>
    // When prohibited from using a matrix filter, DrawImage will ignore the shearing
    // components of its local transform.  If any of the above conditions are met then the
    // DrawImage might not be drawn correctly.  Setting <code>useMatrixFilter</code> to
    // <code>true</code> avoids this possibility but it also suffers from a range of
    // side-effects mentioned +link{group:IEFilters,here}.
    // @group IEFilters
    // @visibility drawing
    //<

    _getUseMatrixFilter : function () {
        
        return (
            this.useMatrixFilter === true ||
            (this.useMatrixFilter !== false &&
             // Save the return value to detect when `isc.Canvas.setNeverUseFilters()` has
             // been called.
             (this._useMatrixFilter = !isc.Canvas.neverUseFilters)));
    },

    _checkUseMatrixFilter : function () {
        
        if (!(this.useMatrixFilter === true || this.useMatrixFilter === false)) {
            var prevUseMatrixFilter = this._useMatrixFilter;
            if (prevUseMatrixFilter != this._getUseMatrixFilter()) {
                this._vmlRendererHandle.setProperty("useMatrixFilter", !prevUseMatrixFilter);
            }
        }
    },


//> @method drawImage.getBoundingBox()
// Returns the top, left, top+width, left+height
//
// @return (Array[] of double) x1, y1, x2, y2 coordinates
// @visibility drawing
//<
getBoundingBox : function (includeStroke, outputBox) {
    
    var box = (outputBox || new Array(4));
    box[0] = this.left;
    box[1] = this.top;
    box[2] = this.left + this.width;
    box[3] = this.top + this.height;
    return box;
},

isPointInPath : isc.DrawItem.getInstanceProperty("isInBounds"),

// TODO copy w/h to internal properties so we can apply scaling without changing
//  the user properties
init : function () {
    // `exemptFromGlobalTransform` is not implemented for DrawImage.
    this.exemptFromGlobalTransform = false;

    this.Super("init", arguments);
    this.initImage(this.src);
},

initImage : function (src) {
    if (src) {
        // Support relative image-paths.
        src = this.getSrcURL(src);

        // Create the Image object if not already created.
        var image = this.image;
        if (image == null) {
            image = this.image = new Image();

            var self = this;
            image.onload = function () {
                if (self.drawingBitmap) {
                    self.drawPane.redrawBitmap();
                }
            };
        }

        // Set the image's src to load the image.
        image.src = src;
    }
},

//----------------------------------------
//  DrawImage renderers
//----------------------------------------

getSrcURL : function (src) {
    var result = isc.Canvas.getImgURL(src);
    return result;
},

_vmlImageConfigAdapter: new isc.VMLRenderer.shapeConfig(),
_renderVML : function (vmlRenderer, id, conversionContext) {
    var config = this._vmlImageConfigAdapter;
    isc.VMLRenderer.drawItemToShapeConfig(this, config);
    config.alt = this.title;
    config.useMatrixFilter = this._getUseMatrixFilter();
    this._vmlRendererHandle = vmlRenderer.image(id, config);
},

getSvgString : function (conversionContext) {
    var center;
    var svgString = "<image";
    var svgHandleID = this._getSvgHandleID();
    if (svgHandleID != null) svgString += " id='" + svgHandleID + "'";
    var t = this._getLocalTransform();
    svgString += (
        " transform='matrix(" + t.m00 + " " + t.m10 + " " +
                                t.m01 + " " + t.m11 + " " +
                                t.m02 + " " + t.m12 + ")" +
        "' x='" + this.left +
        "' y='" + this.top +
        "' width='" + this.width +
        "px' height='" + this.height +
        "px' " + (conversionContext ? conversionContext.xlinkPrefix||isc.SVGStringConversionContext._$xlink : isc.SVGStringConversionContext._$xlink) + ":href='" + this.getSrcURL(this.src) + "'"
        );
    var attributesSVG = this.getAttributesSVG();
    if (attributesSVG) svgString += " " + attributesSVG;
    svgString += ">";
    if (this.title) {
        svgString += "<title>" + isc.makeXMLSafe(this.title) + "</title>";
    }
    svgString += "</image>";
    return svgString;
},

getAttributesSVG : function () {
    // Override SVG's default preserveAspectRatio="xMidYMid" setting so that the image is
    // non-uniformly scaled to fit the viewport rectangle.
    // http://www.w3.org/TR/SVG11/coords.html#PreserveAspectRatioAttribute
    return this.Super("getAttributesSVG", arguments) + " preserveAspectRatio='none'";
},

drawBitmapPath : function(context) {
    context.drawImage(this.image,this.left,this.top,this.width,this.height);
},

//> @method drawImage.getCenter()
// Get the center point of the image.
// @return (Point) the center point
// @visibility drawing
//<
getCenter : function () {
    return [this.left + isc.DrawItem._makeCoordinate(this.width/2), this.top + isc.DrawItem._makeCoordinate(this.height/2)];
},

//----------------------------------------
//  DrawImage attribute setters
//----------------------------------------

//> @method drawImage.setSrc()
// Change the URL of the image displayed.
// @param src (URL) new URL
//
// @visibility drawing
//<
setSrc: function (src) {
    this.initImage(src);

    if (this.drawingVML) {
        this._vmlRendererHandle.setProperty("src", this.src);
        this._checkUseMatrixFilter();
        this._vmlRendererHandle.flush();
    } else if (this.drawingSVG) {
        this._svgHandle.setAttributeNS(isc._$xlinkNS, "href", this.src);
    } else if (this.drawingBitmap) {
        this.drawPane.redrawBitmap();
    }
},

//> @method drawImage.setLeft()
// Set the left coordinate of the drawImage.
// @param left (int) new left coordinate
// @visibility drawing
//<
setLeft : function (left) {
    this.setRect(left, this.top, this.width, this.height);
},

//> @method drawImage.setTop()
// Set the top coordinate of the drawImage.
// @param top (int) new top coordinate
// @visibility drawing
//<
setTop : function (top) {
    this.setRect(this.left, top, this.width, this.height);
},

//> @method drawImage.setWidth()
// Set the width of the drawImage.
// @param width (int) new width
// @visibility drawing
//<
setWidth : function (width) {
    this.setRect(this.left, this.top, width, this.height);
},

//> @method drawImage.setHeight()
// Set the height of the drawImage.
// @param height (int) new height
// @visibility drawing
//<
setHeight : function (height) {
    this.setRect(this.left, this.top, this.width, height);
},

//> @method drawImage.setRect()
// Updates the drawImage to match the specified coordinates and size in 
// +link{DrawPane,local coordinates}.
// @param left (integer) new left coordinate
// @param top (integer) new top coordinate
// @param width (integer) new width
// @param height (integer) new height
// @visibility drawing
//<
setRect : function (left, top, width, height, cx0, cy0) {
    if (left == null) left = this.left;
    if (top == null) top = this.top;
    if (width == null) width = this.width;
    if (height == null) height = this.height;

    

    var sameLTWH = false;
    if (this.left == left &&
        this.top == top &&
        this.width == width &&
        this.height == height)
    {
        sameLTWH = true;
        if (cx0 == null && cy0 == null) return;
    }

    if (cx0 == null || cy0 == null) {
        var center0 = this._getRotationCenter();
        if (sameLTWH &&
            (cx0 == null || cx0 == center0.cx) &&
            (cy0 == null || cy0 == center0.cy))
        {
            return;
        }
        cx0 = center0.cx;
        cy0 = center0.cy;
    }

    this.left = left;
    this.top = top;
    this.width = width;
    this.height = height;

    var center = this._getRotationCenter();
    if (sameLTWH &&
        cx0 == center.cx &&
        cy0 == center.cy)
    {
        return;
    }
    this._updateRotationCenter(cx0, cy0, center.cx, center.cy);

    if (this.drawingVML) {
        var vmlRendererHandle = this._vmlRendererHandle;
        if (cx0 != center.cx || cy0 != center.cy) {
            this._transform = null;
            vmlRendererHandle.setProperty("transform", this._getLocalTransform());
        }
        vmlRendererHandle.setProperty("left", left).setProperty("top", top)
            .setProperty("width", width).setProperty("height", height);
        this._checkUseMatrixFilter();
        vmlRendererHandle.flush();

    } else if (this.drawingSVG) {
        var svgHandle = this._svgHandle;
        if (cx0 != center.cx || cy0 != center.cy) {
            this._transform = null;
            var t = this._getLocalTransform();
            svgHandle.setAttributeNS(null, "transform", "matrix(" + t.m00 + " " + t.m10 + " " +
                                                                    t.m01 + " " + t.m11 + " " +
                                                                    t.m02 + " " + t.m12 + ")");
        }
        svgHandle.setAttributeNS(null, "x", left);
        svgHandle.setAttributeNS(null, "y", top);
        svgHandle.setAttributeNS(null, "width", width);
        svgHandle.setAttributeNS(null, "height", height);

    } else if (this.drawingBitmap) {
        this.drawPane.redrawBitmap();
    }

    this._reshaped();
},

_setLineWidthVML : isc.Class.NO_OP,

//> @method drawImage.moveBy()
// Move the drawImage by the specified delta
// @param dX (int) number of pixels to move horizontally
// @param dY (int) number of pixels to move vertically
// @visibility drawing
//<

//> @method drawImage.moveTo()
// Move the drawImage to the specified position
// @param left (integer) new left coordinate
// @param top (integer) new top coordinate
// @visibility drawing
//<

_updateLocalTransform : function (transform, cx, cy, initialShape, fireReshaped, viaLocalTransformOnly) {
    
    if (!viaLocalTransformOnly) {
        if (initialShape == null) {
            initialShape = this;
        }
        var info = isc.DrawItem._fitBestRect(
                transform, cx, cy,
                initialShape.left, initialShape.top, initialShape.width, initialShape.height, 0);
        if (info.success) {
            this.Super("_updateLocalTransform", [transform, cx, cy, initialShape, false, true], arguments);
            this.setRect(isc.DrawItem._makeCoordinate(info.left),
                         isc.DrawItem._makeCoordinate(info.top),
                         isc.DrawItem._makeCoordinate(info.width),
                         isc.DrawItem._makeCoordinate(info.height),
                         isc.DrawItem._makeCoordinate(cx),
                         isc.DrawItem._makeCoordinate(cy));
            return;
        }
    }
    this.Super("_updateLocalTransform", [transform, cx, cy, initialShape, fireReshaped, true], arguments);
},

_saveShape : function () {
    return {
        left: this.left,
        top: this.top,
        width: this.width,
        height: this.height
    };
}

}); // end DrawImage.addProperties

isc.DrawImage.markUnsupportedMethods(null, ["setLineWidth", "setLineColor", "setLineOpacity",
        "setLinePattern", "setLineCap", "setFillColor", "setFillGradient", "setFillOpacity"]);
isc.DrawImage.markUnsupportedMethods(null, ["setStartArrow", "setEndArrow"]);








//------------------------------------------------------------------------------------------
//> @class DrawCurve
//
//  DrawItem that renders cubic bezier curves.
//
//  @inheritsFrom DrawItem
//  @treeLocation Client Reference/Drawing/DrawItem
//  @visibility drawing
//<
//------------------------------------------------------------------------------------------

// TODO consider whether this should be a subclass of DrawPath instead
isc.defineClass("DrawCurve", "DrawItem").addProperties({
    //> @attr drawCurve.knobs
    // Array of control knobs to display for this item. Each +link{knobType} specified in this
    // will turn on UI element(s) allowing the user to manipulate this DrawCurve.  To update the
    // set of knobs at runtime use +link{drawItem.showKnobs()} and +link{drawItem.hideKnobs()}.
    // <p>
    // DrawCurve supports the
    // <smartclient>"startPoint", "endPoint", "controlPoint1", and "controlPoint2"</smartclient>
    // <smartgwt>{@link com.smartgwt.client.types.KnobType#STARTPOINT}, {@link com.smartgwt.client.types.KnobType#ENDPOINT},
    // {@link com.smartgwt.client.types.KnobType#CONTROLPOINT1}, and {@link com.smartgwt.client.types.KnobType#CONTROLPOINT2}</smartgwt>
    // knob types.
    // @see DrawItem.knobs
    // @include DrawItem.knobs
    //<

    //> @attr drawCurve.showTitleLabelBackground (boolean : true : IRA)
    // @include DrawItem.showTitleLabelBackground
    //<
    showTitleLabelBackground: true,

    //> @attr drawCurve.startPoint     (Point : [0,0] : IRW)
    // Start point of the curve
    // @visibility drawing
    //<
    startPoint: [0,0],

    //> @attr drawCurve.endPoint       (Point : [100,100] : IRW)
    // End point of the curve
    // @visibility drawing
    //<
    endPoint: [100,100],

    //> @attr drawCurve.controlPoint1  (Point : [100,0] : IRW)
    // First cubic bezier control point.
    // @visibility drawing
    //<
    controlPoint1: [100,0],

    //> @attr drawCurve.controlPoint2  (Point : [0,100] : IRW)
    // Second cubic bezier control point.
    // @visibility drawing
    //<
    controlPoint2: [0,100],

    svgElementName: "path",

    //> @attr drawCurve.lineCap     (LineCap : "butt" : IRW)
    // Style of drawing the endpoints of a line.
    // <P>
    // Note that for dashed and dotted lines, the lineCap style affects each dash or dot.
    //
    // @group line
    // @visibility drawing
    //<
    lineCap: "butt",

init : function () {
    this.Super("init", arguments);
    this.startPoint = this.startPoint.duplicate();
    this.endPoint = this.endPoint.duplicate();
    this.controlPoint1 = this.controlPoint1.duplicate();
    this.controlPoint2 = this.controlPoint2.duplicate();
    if (!isc.Browser._supportsCanvasIsPointInStroke) {
        this._calculateOffsetCurvePath();
    }
},

//> @method drawCurve.getCenter()
// Get the center point of the rectangle from the curve's +link{DrawCurve.startPoint,startPoint}
// to the +link{DrawCurve.endPoint,endPoint}.
// @return (Point) the center point
// @visibility drawing
//<
getCenter : function () {
    return [this.startPoint[0] + isc.DrawItem._makeCoordinate((this.endPoint[0] - this.startPoint[0])/2), this.startPoint[1] + isc.DrawItem._makeCoordinate((this.endPoint[1] - this.startPoint[1])/2)]; 
},



//----------------------------------------
//  DrawCurve renderers
//----------------------------------------

_vmlCurveConfigAdapter: new isc.VMLRenderer.shapeConfig(),
_renderVML : function (vmlRenderer, id, conversionContext) {
    var config = this._vmlCurveConfigAdapter;
    isc.VMLRenderer.drawItemToShapeConfig(this, config);
    config.fromX = this.startPoint[0];
    config.fromY = this.startPoint[1];
    config.control1X = this.controlPoint1[0];
    config.control1Y = this.controlPoint1[1];
    config.control2X = this.controlPoint2[0];
    config.control2Y = this.controlPoint2[1];
    config.toX = this.endPoint[0];
    config.toY = this.endPoint[1];
    this._vmlRendererHandle = vmlRenderer.curve(id, config);
},

getAttributesSVG : function () {
    return  "d='" + this.getPathSVG() + "'"
},

getPathSVG : function () {
    return  "M" + this.startPoint[0] + " " + this.startPoint[1] +
            "C" + this.controlPoint1[0] + " " + this.controlPoint1[1] +
            " " + this.controlPoint2[0] + " " + this.controlPoint2[1] +
            " " + this.endPoint[0] + " " + this.endPoint[1]
},

computeBezierPoint : function (t) {
    var x = isc.DrawPane.bezier(
        this.startPoint[0], this.controlPoint1[0], this.controlPoint2[0], this.endPoint[0], t);
    var y = isc.DrawPane.bezier(
        this.startPoint[1], this.controlPoint1[1], this.controlPoint2[1], this.endPoint[1], t);
    return [x, y];
},

drawBitmapPath : function (context, drawFlag) {
    var hasArrow = false;
    if (this.startArrow == "open" || this.startArrow == "block") {
        hasArrow = true;
        context.save();
        this._drawBitmapStartArrow(context, this.lineWidth, (drawFlag !== false));
        context.restore();
    }
    if (this.endArrow == "open" || this.endArrow == "block") {
        hasArrow = true;
        context.save();
        this._drawBitmapEndArrow(context, this.lineWidth, (drawFlag !== false));
        context.restore();
    }
    if (hasArrow) {
        // Clear out the current path, which now contains the start or end arrow, so that the
        // arrow is not stroked and/or filled twice.
        context.beginPath();
    }
    context.moveTo(
        this.startPoint[0], this.startPoint[1]
    );
    context.bezierCurveTo(
        this.controlPoint1[0], this.controlPoint1[1],
        this.controlPoint2[0], this.controlPoint2[1],
        this.endPoint[0], this.endPoint[1]
    );
},

_drawBitmapStartArrow : function (context, lineWidth, draw) {
    
    var arrowDelta = 10,
        originX = this.startPoint[0],
        originY = this.startPoint[1],
        point = this.computeBezierPoint(0.01),
        angle = this.computeAngle(originX, originY, point[0], point[1]);

    context.beginPath();
    if (draw) {
        context.fillStyle = this.lineColor;
        context.strokeStyle = this.lineColor;
    }
    context.lineWidth = lineWidth;
    context.lineCap = this.lineCap;
    context.translate(originX, originY);
    context.rotate(angle * this._radPerDeg);
    this.bmMoveTo(arrowDelta, -arrowDelta, context);
    this.bmLineTo(0, 0, context);
    this.bmLineTo(arrowDelta, arrowDelta, context);
    if (this.startArrow == "block") {
        context.closePath();
    }
    if (draw) {
        if (this.startArrow == "block") {
            context.fill();
        }
        context.stroke();
    }
},

_drawBitmapEndArrow : function (context, lineWidth, draw) {
    
    var arrowDelta = 10,
        originX = this.endPoint[0],
        originY = this.endPoint[1],
        point = this.computeBezierPoint(0.99),
        angle = this.computeAngle(point[0], point[1], originX, originY);

    context.beginPath();
    if (draw) {
        context.fillStyle = this.lineColor;
        context.strokeStyle = this.lineColor;
    }
    context.lineWidth = lineWidth;
    context.lineCap = this.lineCap;
    context.translate(originX, originY);
    context.rotate(angle * this._radPerDeg);
    this.bmMoveTo(-arrowDelta, arrowDelta, context);
    this.bmLineTo(0, 0, context);
    this.bmLineTo(-arrowDelta, -arrowDelta, context);
    if (this.endArrow == "block") {
        context.closePath();
    }
    if (draw) {
        if (this.endArrow == "block") {
            context.fill();
        }
        context.stroke();
    }
},


_bezierCurve : function (p1x, p1y, cp1x, cp1y, cp2x, cp2y, p2x, p2y) {
    this.p1x = p1x;
    this.p1y = p1y;
    this.cp1x = cp1x;
    this.cp1y = cp1y;
    this.cp2x = cp2x;
    this.cp2y = cp2y;
    this.p2x = p2x;
    this.p2y = p2y;
},

_bezierCurveZero : function () {
    return new this._bezierCurve(0, 0, 0, 0, 0, 0, 0, 0);
},

_bezierCurveCopy : function (curve, output) {
    output.p1x = curve.p1x;
    output.p1y = curve.p1y;
    output.cp1x = curve.cp1x;
    output.cp1y = curve.cp1y;
    output.cp2x = curve.cp2x;
    output.cp2y = curve.cp2y;
    output.p2x = curve.p2x;
    output.p2y = curve.p2y;
    return output;
},

_drawCurveToBezierCurve : function (drawCurve) {
    return new this._bezierCurve(
        drawCurve.startPoint[0], drawCurve.startPoint[1],
        drawCurve.controlPoint1[0], drawCurve.controlPoint1[1],
        drawCurve.controlPoint2[0], drawCurve.controlPoint2[1],
        drawCurve.endPoint[0], drawCurve.endPoint[1]);
},

_bezierCurveReverse : function (curve, output) {
    var swap = curve.p1x;
    output.p1x = curve.p2x;
    output.p2x = swap;
    swap = curve.p1y;
    output.p1y = curve.p2y;
    output.p2y = swap;
    swap = curve.cp1x;
    output.cp1x = curve.cp2x;
    output.cp2x = swap;
    swap = curve.cp1y;
    output.cp1y = curve.cp2y;
    output.cp2y = swap;
    return output;
},


_bezierCurveDeCasteljau : function (curve, t, leftOutputCurve, rightOutputCurve) {
    
    var w = 1 - t,
        p1x = curve.p1x,
        p1y = curve.p1y,
        p2x = curve.p2x,
        p2y = curve.p2y,
        ax = w * p1x + t * curve.cp1x,
        ay = w * p1y + t * curve.cp1y,
        bx = w * curve.cp1x + t * curve.cp2x,
        by = w * curve.cp1y + t * curve.cp2y,
        cx = w * curve.cp2x + t * p2x,
        cy = w * curve.cp2y + t * p2y,
        dx = w * ax + t * bx,
        dy = w * ay + t * by,
        ex = w * bx + t * cx,
        ey = w * by + t * cy,
        fx = w * dx + t * ex,
        fy = w * dy + t * ey;

    if (leftOutputCurve != null) {
        leftOutputCurve.p1x = p1x;
        leftOutputCurve.p1y = p1y;
        leftOutputCurve.cp1x = ax;
        leftOutputCurve.cp1y = ay;
        leftOutputCurve.cp2x = dx;
        leftOutputCurve.cp2y = dy;
        leftOutputCurve.p2x = fx;
        leftOutputCurve.p2y = fy;
    }

    if (rightOutputCurve != null) {
        rightOutputCurve.p1x = fx;
        rightOutputCurve.p1y = fy;
        rightOutputCurve.cp1x = ex;
        rightOutputCurve.cp1y = ey;
        rightOutputCurve.cp2x = cx;
        rightOutputCurve.cp2y = cy;
        rightOutputCurve.p2x = p2x;
        rightOutputCurve.p2y = p2y;
    }
},


_calculateOffsetCurvePath : function () {
    
    var epsilon = 1e-6,
        
        flatness = 0.01,
        lowFlatness = 0.001,
        // Use the same line width set by DrawItem.isPointInPath() before calling the native
        // isPointInStroke():
        lineWidth = (
            (this.lineWidth == null ? 1 : Math.max(1, this.lineWidth)) +
            2 * this.hitTolerance),
        curve = this._drawCurveToBezierCurve(this);

    var p1x = curve.p1x,
        p1y = curve.p1y,
        cp1x = curve.cp1x,
        cp1y = curve.cp1y,
        cp2x = curve.cp2x,
        cp2y = curve.cp2y,
        p2x = curve.p2x,
        p2y = curve.p2y;

    // Detect the cases where the Bezier curve is a single point (all control points are
    // equal) or has collinear control points.
    var pi2 = 2 * Math.PI,
        irregularStartPoint = (
            Math.abs(p1x - cp1x) < epsilon && Math.abs(p1y - cp1y) < epsilon),
        irregularEndPoint = (
            Math.abs(p2x - cp2x) < epsilon && Math.abs(p2y - cp2y) < epsilon),
        midpointX = (p1x + cp1x + cp2x + p2x) / 4,
        midpointY = (p1y + cp1y + cp2y + p2y) / 4,
        meanX = (p1x + cp1x + cp2x + p2x) / 4,
        meanY = (p1y + cp1y + cp2y + p2y) / 4,
        sxx = 0, sxy = 0, syy = 0,
        singlePoint = true;
    for (var i = 4; i--; ) {
        var x = 0, y = 0;
        if (i == 3) {
            x = p1x;
            y = p1y;
        } else if (i == 2) {
            x = cp1x;
            y = cp1y;
        } else if (i == 1) {
            x = cp2x;
            y = cp2y;
        } else {
            x = p2x;
            y = p2y;
        }
        var dx = x - meanX, dy = y - meanY;
        sxx += dx * dx;
        sxy += dx * dy;
        syy += dy * dy;

        if (isc.Math._hypot(x - midpointX, y - midpointY) >= epsilon) {
            singlePoint = false;
        }
    }

    
    var collinear = !singlePoint,
        phi = 0;
    if (collinear) {
        
        var numer = (syy - sxx + isc.Math._hypot(syy - sxx, 2 * sxy)),
            denom = 2 * sxy;
        phi = Math.atan2(numer, denom);

        
        var c = Math.cos(phi), s = Math.sin(phi),
            rho = meanX * s - meanY * c;
        for (var i = 0; collinear && i < 4; ++i) {
            var x = 0, y = 0;
            if (i == 3) {
                x = p1x; y = p1y;
            } else if (i == 2) {
                x = cp1x; y = cp1y;
            } else if (i == 1) {
                x = cp2x; y = cp2y;
            } else {
                x = p2x; y = p2y;
            }

            var sigma = x * c + y * s,
                dx = rho * s + sigma * c - x,
                dy = rho * -c + sigma * s - y,
                distance = isc.Math._hypot(dx, dy);

            collinear = (distance < 2 * Math.SQRT2);
        }
    }

    var halfLineWidth = lineWidth / 2;
    if (singlePoint) {
        var angle1 = 0, angle2 = 0;
        if (!(p1x == cp1x && p1y == cp1y)) {
            angle1 = Math.atan2(cp1y - p1y, cp1x - p1x);
        } else if (!(p1x == cp2x && p1y == cp2y)) {
            angle1 = Math.atan2(cp2y - p1y, cp2x - p1x);
        } else if (!(p1x == p2x && p1y == p2y)) {
            angle1 = Math.atan2(p2y - p1y, p2x - p1x);
        }
        if (!(p2x == cp2x && p2y == cp2y)) {
            angle2 = Math.atan2(cp2y - p2y, cp2x - p2x);
        } else if (!(p2x == cp1x && p2y == cp1y)) {
            angle2 = Math.atan2(cp1y - p2y, cp1x - p2x);
        } else if (!(p2x == p1x && p2y == p1y)) {
            angle2 = Math.atan2(p1y - p2y, p1x - p2x);
        }
        this._offsetCurveInfo = {
            halfLineWidth: halfLineWidth,
            singlePoint: true,
            collinear: false,
            midpointX: midpointX,
            midpointY: midpointY,
            angle1: angle1,
            angle2: angle2
        };
        return;
    }

    collinear = collinear || (irregularStartPoint && irregularEndPoint);
    if (collinear) {
        var xExtrema = isc.DrawPane.bezierExtrema(p1x, cp1x, cp2x, p2x),
            minX = xExtrema[0], maxX = xExtrema[1],
            yExtrema = isc.DrawPane.bezierExtrema(p1y, cp1y, cp2y, p2y),
            minY = yExtrema[0], maxY = yExtrema[1];

        // Determine if the line is from (minX, minY) to (maxX, maxY) or from (minX, maxY) to
        // (maxX, minY).
        var ux = Math.cos(phi), uy = -Math.sin(phi),
            x1 = 0, y1 = 0, x2 = 0, y2 = 0;
        if (Math.abs(ux * (maxY - minY) + uy * (maxX - minX))
                < Math.abs(ux * (minY - maxY) + uy * (maxX - minX)))
        {
            x1 = minX;
            y1 = minY;
            x2 = maxX;
            y2 = maxY;
        } else {
            x1 = minX;
            y1 = maxY;
            x2 = maxX;
            y2 = minY;
        }

        var pi2 = 2 * Math.PI,
            angle = (pi2 + Math.atan2(y2 - y1, x2 - x1)) % pi2;
        this._offsetCurveInfo = {
            halfLineWidth: halfLineWidth,
            singlePoint: false,
            collinear: true,

            
            roundPoint1: !(x1 == p1x && y1 == p1y) && !(x1 == p2x && y1 == p2y),
            roundPoint2: !(x2 == p1x && y2 == p1y) && !(x2 == p2x && y2 == p2y),

            x1: x1,
            y1: y1,
            x1Left: x1 - halfLineWidth * Math.cos(Math.PI / 2 - angle),
            y1Left: y1 + halfLineWidth * Math.sin(Math.PI / 2 - angle),
            x1Right: x1 + halfLineWidth * Math.cos(Math.PI / 2 - angle),
            y1Right: y1 - halfLineWidth * Math.sin(Math.PI / 2 - angle),
            startAngle1: angle + Math.PI / 2,
            endAngle1: angle + 3 * Math.PI / 2,
            x2: x2,
            y2: y2,
            x2Left: x2 - halfLineWidth * Math.cos(Math.PI / 2 - angle),
            y2Left: y2 + halfLineWidth * Math.sin(Math.PI / 2 - angle),
            x2Right: x2 + halfLineWidth * Math.cos(Math.PI / 2 - angle),
            y2Right: y2 - halfLineWidth * Math.sin(Math.PI / 2 - angle),
            startAngle2: angle + Math.PI / 2,
            endAngle2: angle - Math.PI / 2,
            // The angle at which the "square" lineCap should start:
            angle: 3 * Math.PI / 4 - angle
        };
        return;
    }

    
    var ax = -p1x + 3 * (cp1x - cp2x) + p2x,
        ay = -p1y + 3 * (cp1y - cp2y) + p2y,
        bx = 3 * (p1x - cp1x - cp1x + cp2x),
        by = 3 * (p1y - cp1y - cp1y + cp2y),
        cx = 3 * (-p1x + cp1x),
        cy = 3 * (-p1y + cp1y),
        dx = p1x,
        dy = p1y;

    
    var xRoots = isc.Math._rpoly([3 * ax, 2 * bx, cx], 2).zeros,
        hasXRoot1 = (
            xRoots.length > 0 &&
            Math.abs(xRoots[0].imag) < epsilon),
        hasXRoot2 = (
            xRoots.length > 1 &&
            Math.abs(xRoots[1].imag) < epsilon),
        yRoots = null,
        hasYRoot1 = false, hasYRoot2 = false;

    if (hasXRoot1 || hasXRoot2) {
        yRoots = isc.Math._rpoly([3 * ay, 2 * by, cy], 2).zeros;
        hasYRoot1 = (
            yRoots.length > 0 &&
            Math.abs(yRoots[0].imag) < epsilon &&
            epsilon < yRoots[0].real && yRoots[0].real < 1 - epsilon);
        hasYRoot2 = (
            yRoots.length > 1 &&
            Math.abs(yRoots[1].imag) < epsilon &&
            epsilon < yRoots[1].real && yRoots[1].real < 1 - epsilon);
    }

    var roots = [];
    if (hasXRoot1 && hasYRoot1 && Math.abs(xRoots[0].real, yRoots[0].real) < epsilon) {
        roots.push((xRoots[0].real + yRoots[0].real) / 2);
    }
    if (hasXRoot1 && hasYRoot2 && Math.abs(xRoots[0].real, yRoots[1].real) < epsilon) {
        roots.push((xRoots[0].real + yRoots[1].real) / 2);
    }
    if (hasXRoot2 && hasYRoot1 && Math.abs(xRoots[1].real, yRoots[0].real) < epsilon) {
        roots.push((xRoots[1].real + yRoots[0].real) / 2);
    }
    if (hasXRoot2 && hasYRoot2 && Math.abs(xRoots[1].real, yRoots[1].real) < epsilon) {
        roots.push((xRoots[1].real + yRoots[1].real) / 2);
    }
    for (var i = roots.length; i--; ) {
        var root = roots[i];
        if (root < -epsilon || root > 1 + epsilon) {
            roots.splice(i, 1);
        } else if (root < epsilon) {
            irregularStartPoint = true;
            roots.splice(i, 1);
        } else if (root > 1 - epsilon) {
            irregularEndPoint = true;
            roots.splice(i, 1);
        }
    }
    // Sort the roots in ascending order.
    for (var i = 1; i < roots.length; ++i) {
        for (var j = i; roots[j - 1] > roots[j]; --j) {
            var swap = roots[j - 1];
            roots[j - 1] = roots[j];
            roots[j] = swap;
        }
    }

    
    var leftPath = [],
        rightPath = [];

    // Calculate the first point of the left path and the first point of the right path.
    var halfLineWidth = lineWidth / 2,
        cp1xmp1x = cp1x - p1x,
        cp1ymp1y = cp1y - p1y,
        sDenom = isc.Math._hypot(cp1xmp1x, cp1ymp1y),
        ux = cp1ymp1y / sDenom,
        uy = -cp1xmp1x / sDenom;
    leftPath.push(p1x + halfLineWidth * ux, p1y + halfLineWidth * uy);
    var cp2xmp2x = cp2x - p2x,
        cp2ymp2y = cp2y - p2y,
        z = isc.Math._hypot(cp2xmp2x, cp2ymp2y),
        vx = cp2ymp2y / z,
        vy = -cp2xmp2x / z;

    
    var head = this._bezierCurveZero(),
        tail = this._bezierCurveZero(),
        work = this._bezierCurveZero(),
        temp = this._bezierCurveZero();

    roots.unshift(0);
    roots.push(1);
    var numRoots = roots.length;
    for (var i = 1; i < numRoots; ++i) {
        var rleft = roots[i - 1], rright = roots[i];

        // Copy the subcurve of `curve` from `t = rleft` to `t = rright` into the `temp` curve.
        if (numRoots == 2) {
            this._bezierCurveCopy(curve, temp);
        } else {
            if (1 < i) {
                this._bezierCurveDeCasteljau(curve, rleft, null, temp);
            }
            if (i < numRoots - 1) {
                this._bezierCurveDeCasteljau(
                    (1 < i ? temp : curve), (rright - rleft) / (1 - rleft), temp, null);
            }
        }

        var leftFlag = (
                (1 < i &&
                    isc.Math._hypot(temp.cp1x - temp.p1x, temp.cp2x - temp.p2x) < epsilon) ||
                irregularStartPoint),
            rightFlag = (
                (i < numRoots - 1 &&
                    isc.Math._hypot(temp.cp2x - temp.p2x, temp.cp2y - temp.p2y) < epsilon) ||
                irregularEndPoint);
        if (leftFlag && rightFlag) {
            
            this._addOffsetLineToPath(lineWidth, temp, leftPath, rightPath);
        } else {
            this._addOffsetCurveToPath(
                lineWidth, flatness, lowFlatness, temp, leftFlag, rightFlag, leftPath,
                rightPath, head, tail, work);
        }
    }

    // Reverse the right path so that the paths can be concatenated into a closed path.
    rightPath.push(p2y + halfLineWidth * vy, p2x + halfLineWidth * vx);
    rightPath = rightPath.reverse();

    

    
    var pi2 = 2 * Math.PI,
        x1Left = leftPath[0],
        y1Left = leftPath[1],
        x1Right = rightPath[rightPath.length - 2],
        y1Right = rightPath[rightPath.length - 1],
        x2Left = leftPath[leftPath.length - 2],
        y2Left = leftPath[leftPath.length - 1],
        x2Right = rightPath[0],
        y2Right = rightPath[1],
        // The angles for `lineCap: "round"`:
        startAngle1 = (Math.PI + Math.atan2(y1Left - y1Right, x1Left - x1Right)) % pi2,
        endAngle1 = startAngle1 + Math.PI,
        endAngle2 = (pi2 + Math.PI + Math.atan2(y2Left - y2Right, x2Left - x2Right)) % pi2,
        startAngle2 = endAngle2 - Math.PI,
        // The corner points for `lineCap: "square"`:
        theta1 = 3 * Math.PI / 2 - startAngle1,
        leftCorner1x = x1Right + halfLineWidth * Math.cos(theta1),
        leftCorner1y = y1Right - halfLineWidth * Math.sin(theta1),
        rightCorner1x = x1Left + halfLineWidth * Math.cos(theta1),
        rightCorner1y = y1Left - halfLineWidth * Math.sin(theta1),
        theta2 = 3 * Math.PI / 2 - startAngle2,
        leftCorner2x = x2Left + halfLineWidth * Math.cos(theta2),
        leftCorner2y = y2Left - halfLineWidth * Math.sin(theta2),
        rightCorner2x = x2Right + halfLineWidth * Math.cos(theta2),
        rightCorner2y = y2Right - halfLineWidth * Math.sin(theta2);

    this._offsetCurveInfo = {
        halfLineWidth: halfLineWidth,
        singlePoint: false,
        collinear: false,

        
        leftPath: leftPath,
        rightPath: rightPath,

        // Precalculated arguments for the lineCap near the start point:
        x1: (x1Left + x1Right) / 2,
        y1: (y1Left + y1Right) / 2,
        radius1: isc.Math._hypot(x1Left - x1Right, y1Left - y1Right) / 2,
        startAngle1: startAngle1,
        endAngle1: endAngle1,
        leftCorner1x: leftCorner1x,
        leftCorner1y: leftCorner1y,
        rightCorner1x: rightCorner1x,
        rightCorner1y: rightCorner1y,

        // Precalculated arguments for the lineCap near the end point:
        x2: (x2Left + x2Right) / 2,
        y2: (y2Left + y2Right) / 2,
        radius2: isc.Math._hypot(x2Left - x2Right, y2Left - y2Right) / 2,
        startAngle2: startAngle2,
        endAngle2: endAngle2,
        leftCorner2x: leftCorner2x,
        leftCorner2y: leftCorner2y,
        rightCorner2x: rightCorner2x,
        rightCorner2y: rightCorner2y
    };
},


_addOffsetCurveToPath : function (
    lineWidth, flatness, lowFlatness, curve, irregularStartPoint, irregularEndPoint, leftPath,
    rightPath, head, tail, work)
{
    
    var epsilon = 1e-6,
        p1x = curve.p1x, p1y = curve.p1y,
        cp1x = curve.cp1x, cp1y = curve.cp1y,
        cp2x = curve.cp2x, cp2y = curve.cp2y,
        p2x = curve.p2x, p2y = curve.p2y,
        ax = -p1x + 3 * (cp1x - cp2x) + p2x, ay = -p1y + 3 * (cp1y - cp2y) + p2y,
        bx = 3 * (p1x - cp1x - cp1x + cp2x), by = 3 * (p1y - cp1y - cp1y + cp2y),
        cx = 3 * (-p1x + cp1x), cy = 3 * (-p1y + cp1y),
        dx = p1x, dy = p1y;

    var denom = (ay * bx - ax * by),
        tCusp = 0,
        t1 = 0,
        t2 = 0,
        noInflectionPoints = false;
    if (Math.abs(denom) < epsilon) {
        var denom2 = (ay * cx - ax * cy);
        if (Math.abs(denom2) < epsilon) {
            // Move the (nonexistent) inflection points out of the [0, 1] interval.
            tCusp = t1 = t2 = -1;
            noInflectionPoints = true;
        } else {
            tCusp = t1 = t2 = -(by * cx - bx * cy) / (3 * denom2);
        }
    } else {
        tCusp = -(ay * cx - ax * cy) / (2 * denom);
        var discriminant = tCusp * tCusp - (by * cx - bx * cy) / (3 * denom);
        if (discriminant <= 0) {
            t1 = t2 = tCusp;
        } else {
            var sqrtDiscriminant = Math.sqrt(discriminant);
            t1 = tCusp - sqrtDiscriminant;
            t2 = tCusp + sqrtDiscriminant;
        }
    }
    
    if (t2 - t1 < epsilon) {
        tCusp = t1 = t2 = (t1 + t2) / 2;
    }

    var tf1 = 0;
    if (-epsilon < t1 && t1 < 1 + epsilon) {
        if (t1 > 0.5) {
            this._bezierCurveDeCasteljau(curve, t1, tail, null);
            this._bezierCurveReverse(tail, tail);
        } else {
            this._bezierCurveDeCasteljau(curve, t1, null, tail);
        }
        var rs = this._xyToRS(tail);
        if (rs == null) {
            t1 = -1;
        } else {
            tf1 = Math.pow(Math.abs(lowFlatness / rs.s3), 1 / 3);
        }
    }
    var tf2 = 0;
    if (t1 == t2) {
        tf2 = tf1;
    } else if (t2 < 1 + epsilon) {
        if (t2 > 0.5) {
            this._bezierCurveDeCasteljau(curve, t2, tail, null);
            this._bezierCurveReverse(tail, tail);
        } else {
            this._bezierCurveDeCasteljau(curve, t2, null, tail);
        }
        var rs = this._xyToRS(tail);
        if (rs == null) {
            t2 = t1;
        } else {
            tf2 = Math.pow(Math.abs(lowFlatness / rs.s3), 1 / 3);
        }
    }

    var epsilon2 = 1000 * epsilon,
        t1Minus = Math.min(t1 - epsilon2, t1 - tf1 * (1 - t1)),
        t1Plus = Math.max(t1 + epsilon2, t1 + tf1 * (1 - t1)),
        t2Minus = Math.min(t2 - epsilon2, t2 - tf2 * (1 - t2)),
        t2Plus = Math.max(t2 + epsilon2, t2 + tf2 * (1 - t2)),
        tValues = (t1 == t2 ? [t1] : [t1, t2]),
        tMinusValues = (t1 == t2 ? [t1Minus] : [t1Minus, t2Minus]),
        tPlusValues = (t1 == t2 ? [t1Plus] : [t1Plus, t2Plus]),
        t1Index = 0,
        t2Index = (t1 == t2 ? t1Index : 1);

    var ax2 = ax * ax, ax3 = ax * ax2,
        ay2 = ay * ay, ay3 = ay * ay2,
        bx2 = bx * bx, bx3 = bx * bx2,
        by2 = by * by, by3 = by * by2,
        cx2 = cx * cx, cx3 = cx * cx2,
        cy2 = cy * cy, cy3 = cy * cy2;

    
    var a5 = 36 * (ax2 + ay2) * (ax * by - bx * ay),
        a4 = 15 * (
            3 * (ax2 + ay2) * (ax * cy - cx * ay) - 2 * ax * ay * (bx2 - by2) +
            2 * (ax2 - ay2) * bx * by),
        a3 = 4 * (
            3 * (4 * ax * ay * by + ay2 * bx + 5 * ax2 * bx) * cy -
            3 * (5 * ay2 * by + ax2 * by + 4 * ax * ay * bx) * cx +
            (ax * by - bx * ay) * (bx2 + by2)),
        a2 = 2 * (
            -6 * ax * ay * (cx2 - cy2) + 6 * (ax2 - ay2) * cx * cy +
            (3 * ax * by2 + 10 * ay * bx * by + 13 * ax * bx2) * cy -
            (13 * ay * by2 + 10 * ax * bx * by + 3 * ay * bx2) * cx),
        a1 = 4 * (2 * (ax * cx + ay * cy) + bx2 + by2) * (bx * cy - cx * by),
        a0 = (
            -ax * cy3 + ay * cx3 + cx * cy * (ay * cy - ax * cx) + 2 * (bx2 - by2) * cx * cy -
            2 * bx * by * (cx2 - cy2));

    
    var epsilon3 = 10 * epsilon2,
        zeros = isc.Math._rpoly([a5, a4, a3, a2, a1, a0], 5).zeros;
    for (var i = zeros.length; i--; ) {
        var z = zeros[i];
        if (-epsilon < zeros[i].real && zeros[i].real < 1 + epsilon && Math.abs(z.imag) < epsilon) {
            var root = zeros[i].real,
                unique = true;
            for (var j = tValues.length; unique && j--; ) {
                unique = (Math.abs(root - tValues[j]) >= epsilon);
            }
            if (!unique) {
                continue;
            }

            tValues.push(root);
            this._bezierCurveDeCasteljau(curve, root, null, work);
            var rs = this._xyToRS(work),
                r1 = rs.r1,
                s2 = rs.s2,
                absS2 = Math.abs(s2),
                ratio = lineWidth * s2 / (3 * r1 * r1),
                halfT2 = 0;
            if (ratio < 1 + epsilon) {
                halfT2 = flatness / (3 * absS2 * (1 - ratio));
            }
            if (ratio > -1 + epsilon) {
                halfT2 = Math.max(halfT2, flatness / (3 * absS2 * (1 + ratio)));
            }
            var t = 2 * Math.sqrt(halfT2);
            tMinusValues.push(Math.min(root - epsilon3, root - t * (1 - root)));
            tPlusValues.push(Math.max(root + epsilon3, root + t * (1 - root)));
        }
    }

    // Sort tValues in ascending order.  Apply the sort order to tMinusValues/tPlusValues.
    for (var i = 1; false && i < tValues.length; ++i) {
        for (var j = i; tValues[j - 1] > tValues[j]; --j) {
            var swap = tValues[j - 1];
            tValues[j - 1] = tValues[j];
            tValues[j] = swap;

            swap = tMinusValues[j - 1];
            tMinusValues[j - 1] = tMinusValues[j];
            tMinusValues[j] = swap;

            swap = tPlusValues[j - 1];
            tPlusValues[j - 1] = tPlusValues[j];
            tPlusValues[j] = swap;

            if (j == t1Index) {
                t1Index = j - 1;
            } else if (j - 1 == t1Index) {
                t1Index = j;
            } else if (j == t2Index) {
                t2Index = j - 1;
            } else if (j - 1 == t2Index) {
                t2Index = j;
            }
        }
    }
    

    // Create a list of the t-values at which to segment the curve and for each segment mark
    // which helper method to use to generate the offset curves over the segment.
    var useLine = 0, useRegularFlatnessCurve = 1, useLowFlatnessCurve = 2,
        t1t2IntervalsOverlap = (t1 != t2 && t2Minus <= t1Plus),
        ts = [0],
        which = [];
    for (var t = 0; t < 1; ) {
        var tInT1Interval = (t1Minus <= t && t < t1Plus),
            tInT2Interval = (t1 != t2 && t2Minus <= t && t < t2Plus),
            flag = useRegularFlatnessCurve,
            nextT = t,
            tInHighCurvatureInterval = false;
        for (var i = 0; !tInHighCurvatureInterval && i < tValues.length; ++i) {
            tInHighCurvatureInterval = (
                i != t1Index && i != t2Index &&
                tMinusValues[i] <= t && t < tPlusValues[i]);
        }

        if (tInT1Interval || tInT2Interval) {
            flag = useLine;
        } else if (tInHighCurvatureInterval) {
            flag = useLowFlatnessCurve;
        } else {
            flag = useRegularFlatnessCurve;
        }
        if (t1t2IntervalsOverlap && t1Minus <= t && t < tCusp) {
            nextT = tCusp;
        } else if (t1t2IntervalsOverlap && tCusp <= t && t < t2Plus) {
            nextT = t2Plus;
        } else if (tInT1Interval) {
            nextT = t1Plus;
        } else if (tInT2Interval) {
            nextT = t2Plus;
        } else if (tInHighCurvatureInterval) {
            nextT = 2;
            for (var i = 0; i < tValues.length; ++i) {
                if (i != t1Index && i != t2Index && t < tValues[i]) {
                    nextT = Math.min(nextT, tValues[i]);
                }
                if (tMinusValues[i] <= t && t < tPlusValues[i]) {
                    nextT = Math.min(nextT, tPlusValues[i]);
                }
            }
        } else {
            nextT = 2;
            for (var i = 0; i < tValues.length; ++i) {
                if (t < tMinusValues[i]) {
                    nextT = Math.min(nextT, tMinusValues[i]);
                }
            }
        }

        nextT = Math.min(nextT, 1);
        ts.push(nextT);
        which.push(flag);
        t = nextT;
    }

    for (var i = 0; i < which.length; ++i) {
        var tl = ts[i], th = ts[i + 1], flag = which[i], last = (i == which.length - 1);

        // Subdivide the original curve from `tl` to `th`.
        if (last) {
            head = curve;
        } else {
            this._bezierCurveDeCasteljau(curve, (th - tl) / (1 - tl), head, curve);
        }

        // Approximate the offset curves over the subcurve.
        if (flag == useLine) {
            this._addOffsetLineToPath(lineWidth, head, leftPath, rightPath);
        } else {
            var f = (flag == useRegularFlatnessCurve ? flatness : lowFlatness);
            this._addOffsetSimpleCurveToPath(f, lineWidth, head, leftPath, rightPath, work);
        }
    }
},


_xyToRS : function (curve) {
    var epsilon = 1e-6,
        cp1xmp1x = curve.cp1x - curve.p1x,
        cp1ymp1y = curve.cp1y - curve.p1y,
        distCp1P1 = isc.Math._hypot(cp1xmp1x, cp1ymp1y);

    var dx = 0, dy = 0, dist = 0;
    if (distCp1P1 < epsilon) {
        var cp2xmcp1x = curve.cp2x - curve.cp1x,
            cp2ymcp1y = curve.cp2y - curve.cp1y,
            distCp2Cp1 = isc.Math._hypot(cp2xmcp1x, cp2ymcp1y);
        if (distCp2Cp1 < epsilon) {
            var p2xmp1x = curve.p2x - curve.p1x,
                p2ymp1y = curve.p2y - curve.p1y,
                distP2P1 = isc.Math._hypot(p2xmp1x, p2ymp1y);
            if (distP2P1 < epsilon) {
                return null;
            } else {
                // Let the `r` axis run along the line from the start point to the end point.
                dx = p2xmp1x;
                dy = p2ymp1y;
                dist = distP2P1;
            }
        } else {
            // Let the `r` axis be oriented along the acceleration vector of the curve
            // at t = 0:
            dx = cp2xmcp1x;
            dy = cp2ymcp1y;
            dist = distCp2Cp1;
        }
    } else {
        
        dx = cp1xmp1x;
        dy = cp1ymp1y;
        dist = distCp1P1;
    }

    var x0 = curve.p1x, y0 = curve.p1y,
        dx0 = curve.p1x - x0,
        dy0 = curve.p1y - y0,
        dx1 = curve.cp1x - x0,
        dy1 = curve.cp1y - y0,
        dx2 = curve.cp2x - x0,
        dy2 = curve.cp2y - y0,
        dx3 = curve.p2x - x0,
        dy3 = curve.p2y - y0;
    return {
        r0: (dx0 * dx + dy0 * dy) / dist,
        s0: (dx0 * dy - dy0 * dx) / dist,
        r1: (dx1 * dx + dy1 * dy) / dist,
        s1: (dx1 * dy - dy1 * dx) / dist,
        r2: (dx2 * dx + dy2 * dy) / dist,
        s2: (dx2 * dx - dy2 * dx) / dist,
        r3: (dx3 * dx + dy3 * dy) / dist,
        s3: (dx3 * dy - dy3 * dx) / dist
    };
},

_addOffsetLineToPath : function (lineWidth, curve, leftPath, rightPath) {
    var epsilon = 1e-6,
        x0 = curve.p1x,
        y0 = curve.p1y,
        x1 = curve.p2x,
        y1 = curve.p2y,
        dx = x1 - x0,
        dy = y1 - y0,
        dist = isc.Math._hypot(dx, dy);
    if (!(dist < epsilon)) {
        var scale = lineWidth / (2 * dist),
            ux = dy * scale,
            uy = -dx * scale;
        leftPath.push(x1 + ux, y1 + uy);
        // The right path will be reverse()d later.
        rightPath.push(y0 - uy, x0 - ux);
    }
},

_addOffsetSimpleCurveToPath : function (flatness, lineWidth, curve, leftPath, rightPath, workCurve) {

    var epsilon = 1e-6,
        origCurve = curve;

    for (var dir = 0; dir < 2; ++dir) {
        var left = true,
            rightPathStartIndex = 0;
        if (dir == 0) {
            left = true;
        } else if (dir == 1) {
            left = false;
            this._bezierCurveReverse(origCurve, origCurve);
            rightPathStartIndex = rightPath.length;
        }
        curve = this._bezierCurveCopy(origCurve, workCurve);

        var globalT = 0;
        for (var notDone = true; notDone; ) {
            var p1x = curve.p1x,
                p1y = curve.p1y,
                cp1x = curve.cp1x,
                cp1y = curve.cp1y,
                cp2x = curve.cp2x,
                cp2y = curve.cp2y,
                p2x = curve.p2x,
                p2y = curve.p2y;

            var cp1xmp1x = cp1x - p1x,
                cp1ymp1y = cp1y - p1y,
                rsDenom = isc.Math._hypot(cp1xmp1x, cp1ymp1y),
                r1 = ((cp1x - p1x) * cp1xmp1x + (cp1y - p1y) * cp1ymp1y) / rsDenom,
                s2 = ((cp2x - p1x) * cp1ymp1y - (cp2y - p1y) * cp1xmp1x) / rsDenom,
                radius = 3 * r1 * r1 / 2 / s2,
                diameter = 2 * radius,
                ds2d3r12 = lineWidth / diameter,
                outside = (diameter >= (left ? lineWidth : -lineWidth)),
                absS2 = Math.abs(s2),
                tDenom = 3 * absS2 * (left ? (1 - ds2d3r12) : (1 + ds2d3r12)),
                addLineSegment = (tDenom > 0);
            if (!left) {
                var rs = this._xyToRS(curve);
            }
            if (!addLineSegment) {
                tDenom = -tDenom;
            }

            var t = 2 * Math.sqrt(flatness / tDenom);
            globalT = globalT + t * (1 - globalT);
            notDone = (globalT < 1 - epsilon);
            if (notDone) {
                this._bezierCurveDeCasteljau(origCurve, globalT, null, curve);
            } else {
                t = globalT = 1;
            }

            if (notDone) {
                var vx = 3 * (-curve.cp1x + curve.p1x),
                    vy = 3 * (-curve.cp1y + curve.p1y),
                    scale = lineWidth / (2 * isc.Math._hypot(vx, vy)),
                    ux = -vy * scale,
                    uy = vx * scale;
                if (left) {
                    leftPath.push(curve.p1x + ux, curve.p1y + uy);
                } else {
                    // The right path will be reverse()d later.
                    rightPath.push(curve.p1y + uy, curve.p1x + ux);
                }
            } else {
                var vx = -3 * (-cp2x + curve.p2x),
                    vy = -3 * (-cp2y + p2y),
                    scale = lineWidth / (2 * isc.Math._hypot(vx, vy)),
                    ux = -vy * scale,
                    uy = vx * scale;
                if (left) {
                    leftPath.push(curve.p2x + ux, curve.p2y + uy);
                } else {
                    // The right path will be reverse()d later.
                    rightPath.push(curve.p2y + uy, curve.p2x + ux);
                }
            }
        }

        if (!left) {
            var rightPathLen = rightPath.length;
            for (var i = rightPathStartIndex, j = rightPathLen - 2; i < j; i += 2, j -= 2) {
                var swap = rightPath[i];
                rightPath[i] = rightPath[j];
                rightPath[j] = swap;
                swap = rightPath[i + 1];
                rightPath[i + 1] = rightPath[j + 1];
                rightPath[j + 1] = swap;
            }
        }
    }

    this._bezierCurveReverse(origCurve, origCurve);
},


_isPointInStroke : function (context, x, y) {
    
    // If either argument is infinite or NaN then isPointInStroke() should return false.
    if (!(isc.isA.Number(x) && isc.isA.Number(y))) {
        return false;
    }

    context.save();
    var info = this._offsetCurveInfo,
        halfLineWidth = info.halfLineWidth;
    

    var isPointInStroke = false;
    if (info.singlePoint) {
        var x0 = info.midpointX, y0 = info.midpointY;
        if (this.lineCap == "square") {
            var radius = halfLineWidth * Math.SQRT2,
                c = Math.cos(info.angle1),
                s = Math.sin(info.angle1),
                startX = x0 + radius * c,
                startY = y0 - radius * s;
            context.beginPath();
            context.moveTo(startX, startY);
            context.lineTo(x0 + radius * s, y0 + radius * c);
            context.lineTo(x0 - radius * c, y0 + radius * s);
            context.lineTo(x0 - radius * s, y0 - radius * c);
            context.lineTo(startX, startY);
            context.closePath();
            isPointInStroke = context.isPointInPath(x, y);

            if (!isPointInStroke && info.angle2 != info.angle1) {
                c = Math.cos(info.angle2);
                s = Math.sin(info.angle2);
                context.beginPath();
                context.moveTo(x0 + radius * c, y0 - radius * s);
                context.lineTo(x0 + radius * s, y0 + radius * c);
                context.lineTo(x0 - radius * c, y0 + radius * s);
                context.lineTo(x0 - radius * s, y0 - radius * c);
                context.lineTo(x0 + radius * c, y0 - radius * s);
                context.moveTo(startX, startY);
                context.closePath();
            }
            context.closePath();
        } else if (this.lineCap == "round") {
            context.beginPath();
            context.moveTo(x0 + halfLineWidth, y0);
            context.arc(x0, y0, halfLineWidth, 0, 360, false);
            context.closePath();
            isPointInStroke = context.isPointInPath(x, y);
        }
        // Nothing is drawn for `lineCap: "butt"`.

    } else if (info.collinear) {
        context.beginPath();
        context.moveTo(info.x1Left, info.y1Left);
        context.lineTo(info.x2Left, info.y2Left);
        if (info.roundPoint2 || this.lineCap == "round") {
            context.arc(
                info.x2, info.y2, halfLineWidth, info.startAngle2, info.endAngle2);
        } else {
            context.lineTo(info.x2Right, info.y2Right);
        }
        context.lineTo(info.x1Right, info.y1Right);
        if (info.roundPoint1 || this.lineCap == "round") {
            context.arc(
                info.x1, info.y1, halfLineWidth, info.startAngle1, info.endAngle1);
        }
        context.lineTo(info.x1Left, info.y1Left);
        context.closePath();
        isPointInStroke = context.isPointInPath(x, y);

        // Draw the square lineCaps separately.
        if (!isPointInStroke && this.lineCap == "square") {
            var radius = halfLineWidth * Math.SQRT2,
                c = Math.cos(info.angle),
                s = Math.sin(info.angle);

            if (!info.roundPoint1) {
                var x0 = info.x1, y0 = info.y1;
                context.beginPath();
                context.moveTo(x0 + radius * c, y0 - radius * s);
                context.lineTo(x0 + radius * s, y0 + radius * c);
                context.lineTo(x0 - radius * c, y0 + radius * s);
                context.lineTo(x0 - radius * s, y0 - radius * c);
                context.lineTo(x0 + radius * c, y0 - radius * s);
                context.closePath();
                isPointInStroke = context.isPointInPath(x, y);
            }
            if (!isPointInStroke && !info.roundPoint2) {
                var x0 = info.x2, y0 = info.y2;
                context.beginPath();
                context.moveTo(x0 + radius * c, y0 - radius * s);
                context.lineTo(x0 + radius * s, y0 + radius * c);
                context.lineTo(x0 - radius * c, y0 + radius * s);
                context.lineTo(x0 - radius * s, y0 - radius * c);
                context.lineTo(x0 + radius * c, y0 - radius * s);
                context.closePath();
                isPointInStroke = context.isPointInPath(x, y);
            }
        }

    } else {
        var leftPath = info.leftPath,
            rightPath = info.rightPath;
        context.beginPath();
        context.moveTo(leftPath[0], leftPath[1]);
        for (var i = 2, leftPathLen = leftPath.length; i < leftPathLen; i += 2) {
            context.lineTo(leftPath[i], leftPath[i + 1]);
        }
        if (this.lineCap == "round") {
            context.arc(info.x2, info.y2, info.radius2, info.startAngle2, info.endAngle2, false);
        } else if (this.lineCap == "square") {
            context.lineTo(info.leftCorner2x, info.leftCorner2y);
            context.lineTo(info.rightCorner2x, info.rightCorner2y);
        } else { // this.lineCap == "butt", or the default case
            context.lineTo(rightPath[0], rightPath[1]);
        }
        for (var i = 2, rightPathLen = rightPath.length; i < rightPathLen; i += 2) {
            context.lineTo(rightPath[i], rightPath[i + 1]);
        }
        if (this.lineCap == "round") {
            context.arc(info.x1, info.y1, info.radius1, info.startAngle1, info.endAngle1, false);
        } else if (this.lineCap == "square") {
            context.lineTo(info.leftCorner1x, info.leftCorner1y);
            context.lineTo(info.rightCorner1x, info.rightCorner1y);
        }
        context.closePath();
        isPointInStroke = context.isPointInPath(x, y);
    }

    
    var arrowDelta = 10,
        lineWidth = 2 * halfLineWidth;
    if (!isPointInStroke && (this.startArrow == "open" || this.startArrow == "block")) {
        var originX = this.startPoint[0],
            originY = this.startPoint[1],
            point = this.computeBezierPoint(0.01),
            angle = this.computeAngle(originX, originY, point[0], point[1]) * this._radPerDeg,
            c = Math.cos(-angle), s = Math.sin(-angle),
            x1 = originX,
            y1 = originY,
            x0 = (c - s) * arrowDelta + x1,
            y0 = -(s + c) * arrowDelta + y1,
            x2 = (c + s) * arrowDelta + x1,
            y2 = (-s + c) * arrowDelta + y1;

        isPointInStroke = (
            isc.Math.euclideanDistanceToLine(
                x0, y0, x1, y1, x, y) <= lineWidth ||
            isc.Math.euclideanDistanceToLine(
                x1, y1, x2, y2, x, y) <= lineWidth ||
            (this.startArrow == "block" && isc.Math.euclideanDistanceToLine(
                x2, y2, x0, y0, x, y) <= lineWidth));
    }
    if (!isPointInStroke && (this.endArrow == "open" || this.endArrow == "block")) {
        var originX = this.endPoint[0],
            originY = this.endPoint[1],
            point = this.computeBezierPoint(0.99),
            angle = this.computeAngle(point[0], point[1], originX, originY) * this._radPerDeg,
            c = Math.cos(-angle), s = Math.sin(-angle),
            x1 = originX,
            y1 = originY,
            x0 = (-c + s) * arrowDelta + x1,
            y0 = (s + c) * arrowDelta + y1,
            x2 = -(c + s) * arrowDelta + x1,
            y2 = (s - c) * arrowDelta + y1;

        isPointInStroke = (
            isc.Math.euclideanDistanceToLine(
                x0, y0, x1, y1, x, y) <= lineWidth ||
            isc.Math.euclideanDistanceToLine(
                x1, y1, x2, y2, x, y) <= lineWidth ||
            (this.endArrow == "block" && isc.Math.euclideanDistanceToLine(
                x2, y2, x0, y0, x, y) <= lineWidth));
    }

    context.restore();

    return isPointInStroke;
},

//----------------------------------------
//  DrawCurve attribute setters
//----------------------------------------

//> @method drawCurve.setStartPoint()
// @include drawLine.setStartPoint
//<
setStartPoint : function (left, top) {
    if (isc.isAn.Array(left)) { // conversion needed for SGWT
        top = left[1];
        left = left[0];
    }

    

    this.startPoint[0] = left;
    this.startPoint[1] = top;

    if (!isc.Browser._supportsCanvasIsPointInStroke) {
        this._calculateOffsetCurvePath();
    }
    if (this.drawingVML) {
        this._vmlRendererHandle.setProperty("fromX", left).setProperty("fromY", top).flush();
    } else if (this.drawingSVG) {
        this._svgHandle.setAttributeNS(null, "d", this.getPathSVG());
    } else if (this.drawingBitmap) {
        this.drawPane.redrawBitmap();
    }

    this._reshaped();
},

//> @method drawCurve.setEndPoint()
// @include drawLine.setEndPoint
//<
setEndPoint : function (left, top) {
    if (isc.isAn.Array(left)) { // conversion needed for SGWT
        top = left[1];
        left = left[0];
    }

    

    this.endPoint[0] = left;
    this.endPoint[1] = top;

    if (!isc.Browser._supportsCanvasIsPointInStroke) {
        this._calculateOffsetCurvePath();
    }
    if (this.drawingVML) {
        this._vmlRendererHandle.setProperty("toX", left).setProperty("toY", top).flush();
    } else if (this.drawingSVG) {
        this._svgHandle.setAttributeNS(null, "d", this.getPathSVG());
    } else if (this.drawingBitmap) {
        this.drawPane.redrawBitmap();
    }

    this._reshaped();
},


//> @method drawCurve.setControlPoint1()
// Updates the first cubic B&#233;zier control point.
//
// @param left (int) left coordinate for control point, in pixels
// @param top (int) top coordinate for control point, in pixels
// @visibility drawing
//< 
setControlPoint1 : function (left, top) {
    if (isc.isAn.Array(left)) { // conversion needed for SGWT
        top = left[1];
        left = left[0];
    }

    

    this.controlPoint1[0] = left;
    this.controlPoint1[1] = top;

    if (!isc.Browser._supportsCanvasIsPointInStroke) {
        this._calculateOffsetCurvePath();
    }
    if (this.drawingVML) {
        this._vmlRendererHandle.setProperty("control1X", left)
            .setProperty("control1Y", top).flush();
    } else if (this.drawingSVG) {
        this._svgHandle.setAttributeNS(null, "d", this.getPathSVG());
    } else if (this.drawingBitmap) {
        this.drawPane.redrawBitmap();
    }

    this._reshaped();
},

//> @method drawCurve.setControlPoint2()
// Updates the second cubic B&#233;zier control point.
//
// @param left (int) left coordinate for control point, in pixels
// @param top (int) top coordinate for control point, in pixels
// @visibility drawing
//< 
setControlPoint2 : function (left, top) {
    if (isc.isAn.Array(left)) { // conversion needed for SGWT
        top = left[1];
        left = left[0];
    }

    

    this.controlPoint2[0] = left;
    this.controlPoint2[1] = top;

    if (!isc.Browser._supportsCanvasIsPointInStroke) {
        this._calculateOffsetCurvePath();
    }
    if (this.drawingVML) {
        this._vmlRendererHandle.setProperty("control2X", left)
            .setProperty("control2Y", top).flush();
    } else if (this.drawingSVG) {
        this._svgHandle.setAttributeNS(null, "d", this.getPathSVG());
    } else if (this.drawingBitmap) {
        this.drawPane.redrawBitmap();
    }

    this._reshaped();
},

_setCurve : function (p1x, p1y, cp1x, cp1y, cp2x, cp2y, p2x, p2y, cx0, cy0) {
    
    var flags = (
            (this.startPoint[0] != p1x ? 0x1 : 0) |
            (this.startPoint[1] != p1y ? 0x2 : 0) |
            (this.controlPoint1[0] != cp1x ? 0x4 : 0) |
            (this.controlPoint1[1] != cp1y ? 0x8 : 0) |
            (this.controlPoint2[0] != cp2x ? 0x10 : 0) |
            (this.controlPoint2[1] != cp2y ? 0x20 : 0) |
            (this.endPoint[0] != p2x ? 0x40 : 0) |
            (this.endPoint[1] != p2y ? 0x80 : 0));
    if (flags == 0) {
        return;
    }
    if (cx0 == null || cy0 == null) {
        var center0 = this._getRotationCenter();
        cx0 = center0.cx;
        cy0 = center0.cy;
    }

    this.startPoint[0] = p1x;
    this.startPoint[1] = p1y;
    this.controlPoint1[0] = cp1x;
    this.controlPoint1[1] = cp1y;
    this.controlPoint2[0] = cp2x;
    this.controlPoint2[1] = cp2y;
    this.endPoint[0] = p2x;
    this.endPoint[1] = p2y;

    var center = this._getRotationCenter();
    this._updateRotationCenter(cx0, cy0, center.cx, center.cy);

    if (!isc.Browser._supportsCanvasIsPointInStroke) {
        this._calculateOffsetCurvePath();
    }
    if (this.drawingVML) {
        var vmlRendererHandle = this._vmlRendererHandle;
        if (cx0 != center.cx || cy0 != center.cy) {
            this._transform = null;
            vmlRendererHandle.setProperty("transform", this._getLocalTransform());
        }
        if (flags & 0x1) vmlRendererHandle.setProperty("fromX", p1x);
        if (flags & 0x2) vmlRendererHandle.setProperty("fromY", p1y);
        if (flags & 0x4) vmlRendererHandle.setProperty("control1X", cp1x);
        if (flags & 0x8) vmlRendererHandle.setProperty("control1Y", cp1y);
        if (flags & 0x10) vmlRendererHandle.setProperty("control2X", cp2x);
        if (flags & 0x20) vmlRendererHandle.setProperty("control2Y", cp2y);
        if (flags & 0x40) vmlRendererHandle.setProperty("toX", p2x);
        if (flags & 0x80) vmlRendererHandle.setProperty("toY", p2y);
        vmlRendererHandle.flush();
    } else if (this.drawingSVG) {
        this._svgHandle.setAttributeNS(null, "d", this.getPathSVG());
    } else if (this.drawingBitmap) {
        this.drawPane.redrawBitmap();
    }

    this._reshaped();
},

// Override DrawItem.setLineWidth() so that the offset curves can be updated.
setLineWidth : function (width) {
    if (width != null) {
        var prevLineWidth = (this.lineWidth == null ? 1 : Math.max(1, this.lineWidth));
        this.lineWidth = width;
        if (!isc.Browser._supportsCanvasIsPointInStroke) {
            var newLineWidth = (this.lineWidth == null ? 1 : Math.max(1, this.lineWidth));
            if (prevLineWidth != newLineWidth) {
                this._calculateOffsetCurvePath();
            }
        }
    }
    return this.Super("setLineWidth", [width, true], arguments);
},

//> @method drawCurve.getBoundingBox()
// Returns the smallest box containing the entire curve.
// @return (Array[] of double) x1, y1, x2, y2 coordinates
// @visibility drawing
//<
getBoundingBox : function (includeStroke, outputBox) {
    
    var epsilon = 1e-6,
        box = (outputBox || new Array(4));
    for (var k = 0; k < 2; ++k) {
        var x = this.startPoint[k],
            y = this.controlPoint1[k],
            z = this.controlPoint2[k],
            w = this.endPoint[k],
            extrema = isc.DrawPane.bezierExtrema(x, y, z, w);

        box[k] = extrema[0];
        box[k + 2] = extrema[1];
    }
    
    return includeStroke != true ? box : this._adjustBoundingBox(true, false, box);
},

_getBoundingBoxOfTransformedShape : function (
    transform, includeStroke, includeHitTolerance, outputBox)
{
    
    var box = (outputBox || new Array(4));

    // Adjust the bounding box by the lineWidth and/or hitTolerance.
    var halfLineWidth = (includeStroke && this._hasStroke() ? (this.lineWidth / 2) : 0),
        hitTolerance = (includeHitTolerance ? this.hitTolerance : 0),
        radius = halfLineWidth + hitTolerance;

    var p1x = this.startPoint[0], p1y = this.startPoint[1],
        cp1x = this.controlPoint1[0], cp1y = this.controlPoint1[1],
        cp2x = this.controlPoint2[0], cp2y = this.controlPoint2[1],
        p2x = this.endPoint[0], p2y = this.endPoint[1];

    var m00 = transform.m00, m01 = transform.m01, m02 = transform.m02,
        m10 = transform.m10, m11 = transform.m11, m12 = transform.m12;

    // Find the min/max x/y coordinates on the transformed Bezier curve.
    var xExtrema = isc.DrawPane.bezierExtrema(
            m00 * p1x + m01 * p1y + m02,
            m00 * cp1x + m01 * cp1y + m02,
            m00 * cp2x + m01 * cp2y + m02,
            m00 * p2x + m01 * p2y + m02),
        minX = xExtrema[0],
        maxX = xExtrema[1],
        yExtrema = isc.DrawPane.bezierExtrema(
            m10 * p1x + m11 * p1y + m12,
            m10 * cp1x + m11 * cp1y + m12,
            m10 * cp2x + m11 * cp2y + m12,
            m10 * p2x + m11 * p2y + m12),
        minY = yExtrema[0],
        maxY = yExtrema[1];

    if (radius == 0) {
        box[0] = minX;
        box[1] = minY;
        box[2] = maxX;
        box[3] = maxY;
    } else {
        
        var halfOffsetWidth = radius * isc.Math._hypot(m00, m01),
            halfOffsetHeight = radius * isc.Math._hypot(m10, m11);

        box[0] = minX - halfOffsetWidth;
        box[1] = minY - halfOffsetHeight;
        box[2] = maxX + halfOffsetWidth;
        box[3] = maxY + halfOffsetHeight;
    }
    return box;
},

// Support control knobs for start point, end point and control points.
// (Documented under DrawKnobs type definition)

showResizeKnobs : null,
hideResizeKnobs : null,

showMoveKnobs : null,
hideMoveKnobs : null,

// Note: can't borrow from drawLine - we have startPoint (two element array) rather than
// startLeft / endLeft
showStartPointKnobs : function () {
    if (this._startKnob != null && !this._startKnob.destroyed) return;
    

    var v = this._normalize(this.startPoint[0], this.startPoint[1], "local", "global");
    this._startKnob = this.createAutoChild("startKnob", {
        _constructor: "DrawKnob",
        x: v[0],
        y: v[1],
        drawPane: this.drawPane,

        resetKnobPosition : function () {
            var drawItem = this.creator,
                v = drawItem._normalize(
                    drawItem.startPoint[0], drawItem.startPoint[1], "local", "global");
            this.setCenterPoint(v[0], v[1]);
        },

        updatePoints : function (x, y, dx, dy, state) {
            var drawItem = this.creator;
            if (state == "start") {
                drawItem._dragStartPointControlPoint1 = drawItem.controlPoint1.duplicate();
                drawItem._dragStartPointControlPoint2 = drawItem.controlPoint2.duplicate();
            }
            var controlPoint1 = drawItem._dragStartPointControlPoint1,
                controlPoint2 = drawItem._dragStartPointControlPoint2;

            if (drawItem.keepInParentRect) {
                var box = drawItem._getParentRect(),
                    curve = {
                        startPoint: [0, 0],
                        controlPoint1: controlPoint1.duplicate(),
                        controlPoint2: controlPoint2.duplicate(),
                        endPoint: drawItem.endPoint
                    };

                drawItem._normalizeCurve(curve, "local", "global");
                curve.startPoint[0] = x;
                curve.startPoint[1] = y;
                drawItem._intersectCurveBox(curve.endPoint, curve, box);
                drawItem._normalizeCurve(curve, "global", "local");

                var newStartPoint = curve.startPoint,
                    newControlPoint1 = curve.controlPoint1,
                    newControlPoint2 = curve.controlPoint2;

                drawItem._setCurve(
                    isc.DrawItem._makeCoordinate(newStartPoint[0]),
                    isc.DrawItem._makeCoordinate(newStartPoint[1]),
                    isc.DrawItem._makeCoordinate(newControlPoint1[0]),
                    isc.DrawItem._makeCoordinate(newControlPoint1[1]),
                    isc.DrawItem._makeCoordinate(newControlPoint2[0]),
                    isc.DrawItem._makeCoordinate(newControlPoint2[1]),
                    drawItem.endPoint[0], drawItem.endPoint[1],
                    null, null);
            } else {
                var v = drawItem._normalize(x, y, "global", "local");
                drawItem._setCurve(
                    isc.DrawItem._makeCoordinate(v[0]),
                    isc.DrawItem._makeCoordinate(v[1]),
                    controlPoint1[0], controlPoint1[1],
                    controlPoint2[0], controlPoint2[1],
                    drawItem.endPoint[0], drawItem.endPoint[1],
                    null, null);
            }

            if (state == "stop") {
                delete drawItem._dragStartPointControlPoint1;
                delete drawItem._dragStartPointControlPoint2;
            }
        }
    });
},

_normalizeCurve : function (curve, inputCoordinateSystem, outputCoordinateSystem) {
    
    var p1 = curve.startPoint,
        v = this._normalize(p1[0], p1[1], inputCoordinateSystem, outputCoordinateSystem);
    p1[0] = v[0];
    p1[1] = v[1];
    var cp1 = curve.controlPoint1;
    v = this._normalize(cp1[0], cp1[1], inputCoordinateSystem, outputCoordinateSystem);
    cp1[0] = v[0];
    cp1[1] = v[1];
    var cp2 = curve.controlPoint2;
    v = this._normalize(cp2[0], cp2[1], inputCoordinateSystem, outputCoordinateSystem);
    cp2[0] = v[0];
    cp2[1] = v[1];
    var p2 = curve.endPoint;
    v = this._normalize(p2[0], p2[1], inputCoordinateSystem, outputCoordinateSystem);
    p2[0] = v[0];
    p2[1] = v[1];
},


_intersectCurveBox : function (fixedPoint, curve, box) {
    
    var box0 = Math.min(box[0], box[2]),
        box1 = Math.min(box[1], box[3]),
        box2 = Math.max(box[0], box[2]),
        box3 = Math.max(box[1], box[3]);
    box[0] = box0;
    box[1] = box1;
    box[2] = box2;
    box[3] = box3;

    // Flip the curve so that the fixedPoint is the same as curve.startPoint.  The curve
    // will be flipped back at the end of this method.
    var backward = (fixedPoint == curve.endPoint);
    if (backward) {
        this._backward(curve);
    }
    

    var epsilon = 1e-6,
        startPoint = curve.startPoint,
        endPoint = curve.endPoint,
        startPointWithinBound = (
            box[0] <= startPoint[0] && startPoint[0] <= box[2] &&
            box[1] <= startPoint[1] && startPoint[1] <= box[3]),
        endPointWithinBound = (
            box[0] <= endPoint[0] && endPoint[0] <= box[2] &&
            box[1] <= endPoint[1] && endPoint[1] <= box[3]),

        wantMinRoot = startPointWithinBound,
        wantMinRootIntoBox = !wantMinRoot && endPointWithinBound,
        wantMaxRoot = !wantMinRoot && !wantMinRootIntoBox,
        root = null;

    // Loop over the four sides of the box to find the intersections of the
    // Bezier curve to the box.
    for (var m = 0; m < 4; ++m) {
        var k = m % 2, l = (k + 1) % 2, s = (m < 2 ? -1 : 1), bound = box[s + 1 + k];

        var x = startPoint[k],
            y = curve.controlPoint1[k],
            z = curve.controlPoint2[k],
            w = endPoint[k],

            a = (-x + 3 * (y - z) + w),
            b = 3 * (x - y - y + z),
            c = 3 * (-x + y),
            d = x - bound;

        // The startPoint is fixed so we only need to decide where to move the endPoint.
        // Nothing can be done if the Bezier curve is a constant polynomial for this
        // coordinate.  Otherwise determine the roots of the cubic polynomial in the Bezier
        // curve to try to move the endPoint onto the boundary (so that the curve lies within
        // the boundary).
        var aIsZero = (Math.abs(a) < epsilon),
            bIsZero = (Math.abs(b) < epsilon),
            cIsZero = (Math.abs(c) < epsilon);
        if (!(aIsZero && bIsZero && cIsZero)) {
            var roots;
            if (aIsZero && bIsZero) {
                // Linear case:  c * x + d = 0
                roots = [-d / c];
            } else if (aIsZero) {
                // Quadratic case:  b * x^2 + c * x + d = 0
                var discriminant = (c * c - 4 * b * d);
                if (discriminant < 0) {
                    roots = [];
                } else if (discriminant > 0) {
                    var sqrtDiscriminant = Math.sqrt(discriminant);
                    roots = [(-c + sqrtDiscriminant) / (2 * b), (-c - sqrtDiscriminant) / (2 * b)];
                } else {
                    roots = [-c / (2 * b)];
                }
            } else {
                // Cubic polynomial case:  a * x^3 + b * x^2 + c * x + d = 0
                // See:  http://en.wikipedia.org/wiki/Cubic_function

                // Precompute powers of a and b
                var a2 = a * a, a3 = a * a2,
                    b2 = b * b, b3 = b * b2,

                    // Substitute t = x + b / (3 * a) to get t^3 + p * t + q = 0
                    avgRoot = -b / (3 * a),
                    p = (3 * a * c - b2) / (3 * a2),
                    q = (2 * b3 - 9 * a * b * c + 27 * a2 * d) / (27 * a3),
                    discriminant = (q * q / 4 + p * p * p / 27);

                if (discriminant > 0) {
                    // one real root
                    var sqrtDiscriminant = Math.sqrt(discriminant),
                        u3 = (-q / 2 + sqrtDiscriminant),
                        v3 = (-q / 2 - sqrtDiscriminant),
                        // Math.pow() may return NaN if the base is negative,
                        // so take the absolute value of the base and multiply
                        // by the sign.
                        u = (u3 > 0 ? 1 : -1) * Math.pow(Math.abs(u3), 1 / 3),
                        v = (v3 > 0 ? 1 : -1) * Math.pow(Math.abs(v3), 1 / 3);
                        roots = [u + v + avgRoot];
                } else {
                    // three real roots
                    var r = Math.sqrt(-4 * p / 3),
                        cos3Alpha = -4 * q / (r * r * r),
                        // Limit to [-1, 1] to avoid:  Math.acos(1.000000001) = NaN
                        alpha = Math.acos(Math.max(-1, Math.min(1, cos3Alpha))) / 3;
                    roots = [
                        avgRoot + r * Math.cos(alpha),
                        avgRoot + r * Math.cos(alpha + 2 * Math.PI / 3),
                        avgRoot + r * Math.cos(alpha - 2 * Math.PI / 3)];
                }
            }

            for (var i = roots.length; i--; ) {
                var r = roots[i],
                    firstDerivative = 3 * a * r * r + 2 * b * r + c;

                // The root must be in the interval [0, 1] to lie on the Bezier curve.  The
                // root must not be a (local) minimum/maximum so that the curve crosses from
                // one side of the bound to the other.
                if (epsilon < r && r < 1 - epsilon && firstDerivative != 0) {
                    var mn1 = (m + 3) % 4,
                        mp1 = (m + 1) % 4,
                        sn1 = (mn1 < 2 ? -1 : 1),
                        sp1 = (mp1 < 2 ? -1 : 1),
                        kn1 = l,
                        kp1 = l,
                        boundn1 = box[sn1 + 1 + kn1],
                        boundp1 = box[sp1 + 1 + kp1],
                        coord = isc.DrawPane.bezier(
                            startPoint[l], curve.controlPoint1[l],
                            curve.controlPoint2[l], endPoint[l],
                            r);

                    if (!(sn1 * coord > sn1 * boundn1) &&
                        !(sp1 * coord > sp1 * boundp1) &&
                        (wantMinRootIntoBox ?
                            (s == (firstDerivative > 0 ? -1 : 1) && (root == null || r < root)) :
                            (root == null || (wantMinRoot ? (r < root) : (r > root)))))
                    {
                        root = r;
                    }
                }
            }
        }
    }

    // If a suitable root is found then move the endPoint, and trim the curve to the subcurve
    // between the startPoint and the new endPoint.
    if (root != null) {
        this._decasteljau(curve, root, true);
    }

    // Flip the curve back.
    if (backward) {
        this._backward(curve);
    }
},

// Swaps curve.{start,end}Point and swaps curve.controlPoint{1,2}.
_backward : function (curve) {
    var swap = curve.startPoint;
    curve.startPoint = curve.endPoint;
    curve.endPoint = swap;

    swap = curve.controlPoint1;
    curve.controlPoint1 = curve.controlPoint2;
    curve.controlPoint2 = swap;
},


_decasteljau : function (curve, t, which) {
    

    var w = 1 - t;
    for (var k = 0; k < 2; ++k) {
        // Note that this is the same formula as in isc.DrawPane.bezier():
        var a = w * curve.startPoint[k] + t * curve.controlPoint1[k],
            b = w * curve.controlPoint1[k] + t * curve.controlPoint2[k],
            c = w * curve.controlPoint2[k] + t * curve.endPoint[k],
            d = w * a + t * b,
            e = w * b + t * c,
            f = w * d + t * e;

        if (which) {
            curve.controlPoint1[k] = a;
            curve.controlPoint2[k] = d;
            curve.endPoint[k] = f;
        } else {
            curve.startPoint[k] = f;
            curve.controlPoint1[k] = e;
            curve.controlPoint2[k] = c;
        }
    }
    return curve;
},

hideStartPointKnobs : function () {
    if (this._startKnob) {
        this._startKnob.destroy();
        delete this._startKnob;
    }
},

showEndPointKnobs : function () {
    if (this._endKnob != null && !this._endKnob.destroyed) return;
    

    var v = this._normalize(this.endPoint[0], this.endPoint[1], "local", "global");
    this._endKnob = this.createAutoChild("endKnob", {
        _constructor: "DrawKnob",
        x: v[0],
        y: v[1],
        drawPane: this.drawPane,

        resetKnobPosition : function () {
            var drawItem = this.creator,
                v = drawItem._normalize(
                    drawItem.endPoint[0], drawItem.endPoint[1], "local", "global");
            this.setCenterPoint(v[0], v[1], false);
        },

        updatePoints : function (x, y, dx, dy, state) {
            var drawItem = this.creator;

            if (state == "start") {
                drawItem._dragEndPointControlPoint1 = drawItem.controlPoint1.duplicate();
                drawItem._dragEndPointControlPoint2 = drawItem.controlPoint2.duplicate();
            }
            var controlPoint1 = drawItem._dragEndPointControlPoint1,
                controlPoint2 = drawItem._dragEndPointControlPoint2;

            if (drawItem.keepInParentRect) {
                var box = drawItem._getParentRect(),
                    curve = {
                        startPoint: drawItem.startPoint,
                        controlPoint1: controlPoint1.duplicate(),
                        controlPoint2: controlPoint2.duplicate(),
                        endPoint: [0, 0]
                    };

                drawItem._normalizeCurve(curve, "local", "global");
                curve.endPoint[0] = x;
                curve.endPoint[1] = y;
                drawItem._intersectCurveBox(curve.startPoint, curve, box);
                drawItem._normalizeCurve(curve, "global", "local");

                var newEndPoint = curve.endPoint,
                    newControlPoint2 = curve.controlPoint2,
                    newControlPoint1 = curve.controlPoint1;

                drawItem._setCurve(
                    drawItem.startPoint[0], drawItem.startPoint[1],
                    isc.DrawItem._makeCoordinate(newControlPoint1[0]),
                    isc.DrawItem._makeCoordinate(newControlPoint1[1]),
                    isc.DrawItem._makeCoordinate(newControlPoint2[0]),
                    isc.DrawItem._makeCoordinate(newControlPoint2[1]),
                    isc.DrawItem._makeCoordinate(newEndPoint[0]),
                    isc.DrawItem._makeCoordinate(newEndPoint[1]),
                    null, null);
            } else {
                var v = drawItem._normalize(x, y, "global", "local");
                drawItem._setCurve(
                    drawItem.startPoint[0], drawItem.startPoint[1],
                    controlPoint1[0], controlPoint1[1],
                    controlPoint2[0], controlPoint2[1],
                    isc.DrawItem._makeCoordinate(v[0]),
                    isc.DrawItem._makeCoordinate(v[1]),
                    null, null);
            }

            if (state == "stop") {
                delete drawItem._dragEndPointControlPoint1,
                delete drawItem._dragEndPointControlPoint2;
            }
        }
    });
},

hideEndPointKnobs : function () {
    if (this._endKnob) {
        this._endKnob.destroy();
        delete this._endKnob;
    }
},

c1LineDefaults: {
    _constructor: "DrawLine",
    lineWidth: 1,
    lineColor: "#8000ff",
    linePattern: "shortdot"
},

//> @attr drawCurve.c1Knob (AutoChild DrawKnob : null : IR)
// If this item is showing <smartclient>"controlPoint1"</smartclient>
// <smartgwt>{@link com.smartgwt.client.types.KnobType#CONTROLPOINT1}</smartgwt>
// +link{drawItem.knobs,control knobs}, this attribute specifies the AutoChild for the
// +link{DrawKnob} for control point 1 of current drawCurve.
//
// @visibility drawing
//<
c1KnobDefaults: {
    cursor: "move",
    knobShapeProperties: {
        _constructor: "DrawOval",
        radius: 4.5,
        lineWidth: 1,
        lineOpacity: 1,
        fillOpacity: 1,
        lineColor: "#333333",
        fillGradient: {
            id: "isc_defaultDrawCurveC1KnobGradient",
            x1: "0%", y1: "0%",
            x2: "0%", y2: "100%",
            colorStops: [
                {color: "#ffffff", offset: 0.15},
                {color: "#d6c1ec", offset: 0.5},
                {color: "#ffffff", offset: 0.85}
            ]
        }
    }
},

c1KnobConstructor: "DrawKnob",

// Control point knobs - these include a line going back to the start or end point
showControlPoint1Knobs : function () {
    var createC1Knob = (this._c1Knob == null || this._c1Knob.destroyed),
        createC1Line = (this._c1Line == null || this._c1Line.destroyed),
        v = null;
    if (createC1Knob || createC1Line) {
        
        v = this._normalize(this.controlPoint1[0], this.controlPoint1[1], "local", "global");
    }

    if (createC1Line) {
        var w = this._normalize(this.startPoint[0], this.startPoint[1], "local", "global");
        this._c1Line = this.createAutoChild("c1Line", {
            _constructor: "DrawLine",
            startLeft: w[0], startTop: w[1],
            endLeft: v[0], endTop: v[1],
            drawPane: this.drawPane,
            autoDraw: true,
            _internal: true,
            exemptFromGlobalTransform: true,
            _globalTransformChanged : function () {
                
                var drawItem = this.creator,
                    v = drawItem._normalize(
                        drawItem.startPoint[0], drawItem.startPoint[1], "local", "global"),
                    w = drawItem._normalize(
                        drawItem.controlPoint1[0], drawItem.controlPoint1[1], "local", "global");
                this._setStartAndEndPoints(v[0], v[1], w[0], w[1], null, null);
            }
        });
    }

    if (createC1Knob) {
        this._c1Knob = this.createAutoChild("c1Knob", {
            x: v[0],
            y: v[1],
            drawPane: this.drawPane,

            resetKnobPosition : function () {
                var drawItem = this.creator,
                    v = drawItem._normalize(
                        drawItem.controlPoint1[0], drawItem.controlPoint1[1], "local", "global");
                this.setCenterPoint(v[0], v[1], false);
            },

            updatePoints : function (x, y, dx, dy, state) {
                var drawItem = this.creator;
                if (state == "start") {
                    drawItem._dragControlPoint1StartPoint = drawItem.startPoint.duplicate();
                    drawItem._dragControlPoint1ControlPoint2 = drawItem.controlPoint2.duplicate();
                    drawItem._dragControlPoint1EndPoint = drawItem.endPoint.duplicate();
                }
                var startPoint = drawItem._dragControlPoint1StartPoint,
                    controlPoint2 = drawItem._dragControlPoint1ControlPoint2,
                    endPoint = drawItem._dragControlPoint1EndPoint;

                if (drawItem.keepInParentRect) {
                    var box = drawItem._getParentRect(),
                        curve = {
                            startPoint: startPoint.duplicate(),
                            controlPoint1: [0, 0],
                            controlPoint2: controlPoint2.duplicate(),
                            endPoint: endPoint.duplicate()
                        };

                    drawItem._normalizeCurve(curve, "local", "global");
                    curve.controlPoint1[0] = x;
                    curve.controlPoint1[1] = y;
                    drawItem._intersectCurveBox(curve.endPoint, curve, box);
                    drawItem._normalizeCurve(curve, "global", "local");

                    var newStartPoint = curve.startPoint,
                        newControlPoint1 = curve.controlPoint1,
                        newControlPoint2 = curve.controlPoint2;

                    drawItem._setCurve(
                        isc.DrawItem._makeCoordinate(newStartPoint[0]),
                        isc.DrawItem._makeCoordinate(newStartPoint[1]),
                        isc.DrawItem._makeCoordinate(newControlPoint1[0]),
                        isc.DrawItem._makeCoordinate(newControlPoint1[1]),
                        isc.DrawItem._makeCoordinate(newControlPoint2[0]),
                        isc.DrawItem._makeCoordinate(newControlPoint2[1]),
                        drawItem.endPoint[0], drawItem.endPoint[1],
                        null, null);
                } else {
                    var v = drawItem._normalize(x, y, "global", "local");
                    drawItem._setCurve(
                        startPoint[0], startPoint[1],
                        isc.DrawItem._makeCoordinate(v[0]),
                        isc.DrawItem._makeCoordinate(v[1]),
                        controlPoint2[0], controlPoint2[1],
                        endPoint[0], endPoint[1],
                        null, null);
                }

                if (state == "stop") {
                    delete drawItem._dragControlPoint1StartPoint;
                    delete drawItem._dragControlPoint1ControlPoint2;
                    delete drawItem._dragControlPoint1EndPoint;
                }
            }
        });
    }
},

hideControlPoint1Knobs : function () {
    if (this._c1Knob) {
        this._c1Knob.destroy();
        delete this._c1Knob;
    }
    if (this._c1Line) {
        this._c1Line.erase();
        delete this._c1Line;
    }
},

c2LineDefaults: {
    _constructor: "DrawLine",
    lineWidth: 1,
    lineColor: "#8000ff",
    linePattern: "shortdot"
},

//> @attr drawCurve.c2Knob (AutoChild DrawKnob : null : IR)
// If this item is showing <smartclient>"controlPoint2"</smartclient>
// <smartgwt>{@link com.smartgwt.client.types.KnobType#CONTROLPOINT2}</smartgwt>
// +link{drawItem.knobs,control knobs}, this attribute specifies the AutoChild for the
// +link{DrawKnob} for control point 2 of current drawCurve.
//
// @visibility drawing
//<
c2KnobDefaults: {
    cursor: "move",
    knobShapeProperties: {
        _constructor: "DrawOval",
        radius: 4.5,
        lineWidth: 1,
        lineOpacity: 1,
        fillOpacity: 1,
        lineColor: "#333333",
        fillGradient: {
            id: "isc_defaultDrawCurveC2KnobGradient",
            x1: "0%", y1: "0%",
            x2: "0%", y2: "100%",
            colorStops: [
                {color: "#ffffff", offset: 0.15},
                {color: "#d6c1ec", offset: 0.5},
                {color: "#ffffff", offset: 0.85}
            ]
        }
    }
},

c2KnobConstructor: "DrawKnob",

showControlPoint2Knobs : function () {
    var createC2Knob = (this._c2Knob == null || this._c2Knob.destroyed),
        createC2Line = (this._c2Line == null || this._c2Line.destroyed),
        v = null;
    if (createC2Knob || createC2Line) {
        
        v = this._normalize(this.controlPoint2[0], this.controlPoint2[1], "local", "global");
    }

    if (createC2Line) {
        var w = this._normalize(this.endPoint[0], this.endPoint[1], "local", "global");
        this._c2Line = this.createAutoChild("c2Line", {
            _constructor: "DrawLine",
            startLeft: w[0], startTop: w[1],
            endLeft: v[0], endTop: v[1],
            drawPane: this.drawPane,
            autoDraw: true,
            _internal: true,
            exemptFromGlobalTransform: true,
            _globalTransformChanged : function () {
                
                var drawItem = this.creator,
                    v = drawItem._normalize(
                        drawItem.endPoint[0], drawItem.endPoint[1], "local", "global"),
                    w = drawItem._normalize(
                        drawItem.controlPoint2[0], drawItem.controlPoint2[1], "local", "global");
                this._setStartAndEndPoints(v[0], v[1], w[0], w[1], null, null);
            }
        });
    }

    if (createC2Knob) {
        this._c2Knob = this.createAutoChild("c2Knob", {
            x: v[0],
            y: v[1],
            drawPane: this.drawPane,

            resetKnobPosition : function () {
                var drawItem = this.creator,
                    v = drawItem._normalize(
                        drawItem.controlPoint2[0], drawItem.controlPoint2[1],
                        "local", "global");
                this.setCenterPoint(v[0], v[1], false);
            },

            updatePoints : function (x, y, dx, dy, state) {
                var drawItem = this.creator;
                if (state == "start") {
                    drawItem._dragControlPoint2StartPoint = drawItem.startPoint.duplicate();
                    drawItem._dragControlPoint2ControlPoint1 = drawItem.controlPoint1.duplicate();
                    drawItem._dragControlPoint2EndPoint = drawItem.endPoint.duplicate();
                }
                var startPoint = drawItem._dragControlPoint2StartPoint,
                    controlPoint1 = drawItem._dragControlPoint2ControlPoint1,
                    endPoint = drawItem._dragControlPoint2EndPoint;

                if (drawItem.keepInParentRect) {
                    var box = drawItem._getParentRect(),
                        curve = {
                            startPoint: startPoint.duplicate(),
                            controlPoint1: controlPoint1.duplicate(),
                            controlPoint2: [0, 0],
                            endPoint: endPoint.duplicate()
                        };

                    drawItem._normalizeCurve(curve, "local", "global");
                    curve.controlPoint2[0] = x;
                    curve.controlPoint2[1] = y;
                    drawItem._intersectCurveBox(curve.startPoint, curve, box);
                    drawItem._normalizeCurve(curve, "global", "local");

                    var newControlPoint1 = curve.controlPoint1,
                        newControlPoint2 = curve.controlPoint2,
                        newEndPoint = curve.endPoint;

                    drawItem._setCurve(
                        drawItem.startPoint[0], drawItem.startPoint[1],
                        isc.DrawItem._makeCoordinate(newControlPoint1[0]),
                        isc.DrawItem._makeCoordinate(newControlPoint1[1]),
                        isc.DrawItem._makeCoordinate(newControlPoint2[0]),
                        isc.DrawItem._makeCoordinate(newControlPoint2[1]),
                        isc.DrawItem._makeCoordinate(newEndPoint[0]),
                        isc.DrawItem._makeCoordinate(newEndPoint[1]),
                        null, null);
                } else {
                    var v = drawItem._normalize(x, y, "global", "local");
                    drawItem._setCurve(
                        startPoint[0], startPoint[1],
                        controlPoint1[0], controlPoint1[1],
                        isc.DrawItem._makeCoordinate(v[0]),
                        isc.DrawItem._makeCoordinate(v[1]),
                        endPoint[0], endPoint[1],
                        null, null);
                }

                if (state == "stop") {
                    delete drawItem._dragControlPoint2StartPoint;
                    delete drawItem._dragControlPoint2ControlPoint1;
                    delete drawItem._dragControlPoint2EndPoint;
                }
            }
        });
    }
},

hideControlPoint2Knobs : function () {
    if (this._c2Knob) {
        this._c2Knob.destroy();
        delete this._c2Knob;
    }
    if (this._c2Line) {
        this._c2Line.erase();
        delete this._c2Line;
    }
},

updateControlKnobs : function () {
    // Update the position of our start/end point knobs when we update our other control points.
    this.Super("updateControlKnobs", arguments);
    if (this._startKnob || this._c1Line) {
        var v = this._normalize(this.startPoint[0], this.startPoint[1], "local", "global");

        // If we're showing the control line, update its start point
        if (this._c1Line) {
            this._c1Line.setStartPoint(
                isc.DrawItem._makeCoordinate(v[0]), isc.DrawItem._makeCoordinate(v[1]));
        }
        if (this._startKnob) {
            this._startKnob.setCenterPoint(v[0], v[1]);
        }
    }
    if (this._endKnob || this._c2Line) {
        var v = this._normalize(this.endPoint[0], this.endPoint[1], "local", "global");
        if (this._c2Line) {
            this._c2Line.setStartPoint(
                isc.DrawItem._makeCoordinate(v[0]), isc.DrawItem._makeCoordinate(v[1]));
        }
        if (this._endKnob) {
            this._endKnob.setCenterPoint(v[0], v[1]);
        }
    }
    if (this._c1Knob) {
        var v = this._normalize(
                this.controlPoint1[0], this.controlPoint1[1], "local", "global");

        // We always render c1Line with c1Point.
        this._c1Line.setEndPoint(
            isc.DrawItem._makeCoordinate(v[0]), isc.DrawItem._makeCoordinate(v[1]));
        this._c1Knob.setCenterPoint(v[0], v[1]);
    }
    if (this._c2Knob) {
        var v = this._normalize(
                this.controlPoint2[0], this.controlPoint2[1], "local", "global");

        this._c2Line.setEndPoint(
            isc.DrawItem._makeCoordinate(v[0]), isc.DrawItem._makeCoordinate(v[1]));
        this._c2Knob.setCenterPoint(v[0], v[1]);
    }
},

//> @method drawCurve.moveStartPointTo()
// Move the start point, end point, and control points of the curve such that the
// +link{startPoint} ends up at the specified coordinates and the shape of the curve is
// unchanged.
//
// @param x (integer) new x coordinate in pixels
// @param y (integer) new y coordinate in pixels
// @visibility drawing
//<
moveStartPointTo : function (x, y) {
    this._movePointToPoint(x, y, this.startPoint[0], this.startPoint[1]);
},

//> @method drawCurve.moveBy()
// Increment start, end and control points of this curve
//
// @param x (int) new x coordinate in pixels
// @param y (int) new y coordinate in pixels
// @visibility drawing
//<

_updateLocalTransform : function (transform, cx, cy, initialShape, fireReshaped, viaLocalTransformOnly) {
    
    if (viaLocalTransformOnly) {
        this.Super("_updateLocalTransform", [transform, cx, cy, initialShape, fireReshaped, true], arguments);
        return;
    }

    var epsilon = 1e-9;
    if (Math.abs(transform.getDeterminant()) > epsilon) {
        if (initialShape == null) {
            initialShape = this._saveShape();
        }

        var invLocalTransform = initialShape.invLocalTransform;
        if (invLocalTransform == null) {
            this.Super(
                "_updateLocalTransform",
                [isc.AffineTransform._getIdentityTransform(), cx, cy, initialShape, false, true],
                arguments);
        } else {
            transform.leftMultiply(invLocalTransform);

            // Reset the components of the local transform.
            var translate = this.translate = (this.translate || new Array(2)),
                scale = this.scale = (this.scale || new Array(2));
            translate[0] = initialShape.translateX;
            translate[1] = initialShape.translateY;
            scale[0] = initialShape.scaleX;
            scale[1] = initialShape.scaleY;
            this.xShearFactor = initialShape.xShearFactor;
            this.yShearFactor = initialShape.yShearFactor;
            this.rotation = initialShape.rotation;
        }

        var sp = transform.transform(initialShape.startLeft, initialShape.startTop);
        this.startPoint[0] = sp[0];
        this.startPoint[1] = sp[1];

        var cp1 = transform.transform(initialShape.control1Left, initialShape.control1Top);
        this.controlPoint1[0] = cp1[0];
        this.controlPoint1[1] = cp1[1];

        var cp2 = transform.transform(initialShape.control2Left, initialShape.control2Top);
        this.controlPoint2[0] = cp2[0];
        this.controlPoint2[1] = cp2[1];

        var ep = transform.transform(initialShape.endLeft, initialShape.endTop);
        this.endPoint[0] = ep[0];
        this.endPoint[1] = ep[1];

        var center = this._getRotationCenter();
        this._updateRotationCenter(cx, cy, center.cx, center.cy);

        if (!isc.Browser._supportsCanvasIsPointInStroke) {
            this._calculateOffsetCurvePath();
        }

        if (this.drawingVML) {
            
            var vmlRendererHandle = this._vmlRendererHandle;
            if (cx != center.cx || cy != center.cy) {
                this._transform = null;
                vmlRendererHandle.setProperty("transform", this._getLocalTransform());
            }
            vmlRendererHandle
                .setProperty("fromX", this.startPoint[0])
                .setProperty("fromY", this.startPoint[1])
                .setProperty("control1X", this.controlPoint1[0])
                .setProperty("control1Y", this.controlPoint1[1])
                .setProperty("control2X", this.controlPoint2[0])
                .setProperty("control2Y", this.controlPoint2[1])
                .setProperty("toX", this.endPoint[0])
                .setProperty("toY", this.endPoint[1])
                .flush();
        } else if (this.drawingSVG) {
            this._svgHandle.setAttributeNS(null, "d", this.getPathSVG());
        } else if (this.drawingBitmap) {
            this.drawPane.redrawBitmap();
        }
        this._reshaped();
    } else {
        this.Super("_updateLocalTransform", [transform, cx, cy, initialShape, fireReshaped, true], arguments);
    }
},

_saveShape : function () {
    var translateX = 0, translateY = 0,
        scaleX = 1, scaleY = 1,
        xShearFactor = this.xShearFactor || 0,
        yShearFactor = this.yShearFactor || 0,
        rotation = this.rotation || 0;
    if (this.translate) {
        translateX = this.translate[0];
        translateY = this.translate[1];
    }
    if (this.scale && this.scale.length === 2) {
        scaleX = this.scale[0];
        scaleY = this.scale[1];
    }

    return {
        startLeft: this.startPoint[0],
        startTop: this.startPoint[1],
        control1Left: this.controlPoint1[0],
        control1Top: this.controlPoint1[1],
        control2Left: this.controlPoint2[0],
        control2Top: this.controlPoint2[1],
        endLeft: this.endPoint[0],
        endTop: this.endPoint[1],

        invLocalTransform: this._getInverseLocalTransform(),
        translateX: translateX,
        translateY: translateY,
        scaleX: scaleX,
        scaleY: scaleY,
        xShearFactor: xShearFactor,
        yShearFactor: yShearFactor,
        rotation: rotation
    };
}

}); // end DrawCurve.addProperties








//------------------------------------------------------------------------------------------
//> @class DrawBlockConnector
//
// DrawItem subclass to render multi-segment, orthogonal-routing paths.
//
// @inheritsFrom DrawCurve
// @treeLocation Client Reference/Drawing/DrawItem
// @visibility drawing
//<
//------------------------------------------------------------------------------------------
isc.defineClass("DrawBlockConnector", "DrawCurve");








//------------------------------------------------------------------------------------------
//> @class DrawPath
//
// Draws a multi-segment line.
//
// @inheritsFrom DrawItem
// @treeLocation Client Reference/Drawing/DrawItem
// @visibility drawing
//<
//------------------------------------------------------------------------------------------
isc.defineClass("DrawPath", "DrawItem");

isc.DrawPath.addProperties({
    //> @attr drawPath.knobs
    // DrawPath only supports the
    // <smartclient>"move"</smartclient>
    // <smartgwt>{@link com.smartgwt.client.types.KnobType#MOVE}</smartgwt>
    // knob type.
    // @see DrawItem.knobs
    // @include DrawItem.knobs
    //<

    //> @attr drawPath.showTitleLabelBackground (boolean : true : IRA)
    // @include DrawItem.showTitleLabelBackground
    //<
    showTitleLabelBackground: true,

    //> @attr drawPath.points (Array of Point : [[0,0], [100,100]] : IRW)
    // Array of Points for the line, specified in the +link{DrawPane,local coordinate system}.
    // @visibility drawing
    //<
    points: [[0,0], [100,100]],

    
    svgElementName: isc.Browser.isMoz ? "path" : "polyline",

init : function () {
    this.points = isc.clone(this.points);
    this._initBoundingParams();
    this.Super("init", arguments);
},

//> @method drawPath.getBoundingBox()
// Returns the min, max points
// @return (Array[] of double) x1, y1, x2, y2 coordinates
// @visibility drawing
//<
getBoundingBox : function (includeStroke, outputBox) {
    
    var bbox = (outputBox || new Array(4));
    bbox[0] = this._left;
    bbox[1] = this._top;
    bbox[2] = this._right;
    bbox[3] = this._bottom;
    return includeStroke != true ? bbox : this._adjustBoundingBox(true, false, bbox);
},

_getBoundingBoxOfTransformedShape : function (
    transform, includeStroke, includeHitTolerance, outputBox)
{
    
    var box = (outputBox || new Array(4));

    // Adjust the bounding box by the lineWidth and/or hitTolerance.
    var halfLineWidth = (includeStroke && this._hasStroke() ? (this.lineWidth / 2) : 0),
        hitTolerance = (includeHitTolerance ? this.hitTolerance : 0),
        radius = halfLineWidth + hitTolerance;

    var points = this.points,
        convexHull = this._convexHull,
        minX = 0, minY = 0, maxX = 0, maxY = 0;

    if (convexHull == null) {
        // There is nothing to transform.
        minX = this._left;
        minY = this._top;
        maxX = this._right;
        maxY = this._bottom;
    } else {
        var m00 = transform.m00, m01 = transform.m01, m02 = transform.m02,
            m10 = transform.m10, m11 = transform.m11, m12 = transform.m12;

        for (var i = 0, h = convexHull.length; i < h; ++i) {
            var point = points[convexHull[i]],
                u = point[0], v = point[1],
                x = m00 * u + m01 * v + m02,
                y = m10 * u + m11 * v + m12;
            if (i == 0) {
                minX = maxX = x;
                minY = maxY = y;
            } else {
                if (x < minX) minX = x;
                else if (maxX < x) maxX = x;
                if (y < minY) minY = y;
                else if (maxY < y) maxY = y;
            }
        }
    }

    box[0] = minX;
    box[1] = minY;
    box[2] = maxX;
    box[3] = maxY;
    if (radius != 0) {
        var halfOffsetWidth = radius * isc.Math._hypot(m00, m01),
            halfOffsetHeight = radius * isc.Math._hypot(m10, m11);

        box[0] -= halfOffsetWidth;
        box[1] -= halfOffsetHeight;
        box[2] += halfOffsetWidth;
        box[3] += halfOffsetHeight;
    }
    return box;
},

//----------------------------------------
//  DrawPath renderers
//----------------------------------------

getPathSVG : function () {
    if (this.points == null || this.points.length == 0) return "";
    var path = "M ", i, point = this.points[0];
    path += point[0] + " " + point[1] + " ";
    for (i = 1; i < this.points.length; ++i) {
        point = this.points[i];
        path += " L " + point[0] + " " + point[1];
    }
    return path;
},

getAttributesSVG : function () {
    if (this.svgElementName === "path") {
        return "d='" + this.getPathSVG() + "'";
    } else {
        return "points='" + this.getPointsText() + "'";
    }
},

// get the list of points as a series of integers like "0 0 100 100" (representing the points
// [0,0] and [100,100], which is what VML and SVG want for a series of points
getPointsText : function () {
    var buffer = isc.StringBuffer.create();
    this._getPointsText(buffer);
    return buffer.release(false);
},
_getPointsText : function (buffer) {
    var points = this.points;
    if (points == null) return;
    for (var i = 0, numPoints = points.length; i < numPoints; ++i) {
        if (i > 0) {
            buffer.append(" ");
        }
        buffer.append(points[i][0], " ", points[i][1]);
    }
},

_vmlPolylineConfigAdapter: new isc.VMLRenderer.shapeConfig(),
_renderVML : function (vmlRenderer, id, conversionContext) {
    var config = this._vmlPolylineConfigAdapter;
    isc.VMLRenderer.drawItemToShapeConfig(this, config);
    this._vmlRendererHandle = vmlRenderer.polyline(id, config);
},

// drawPath.drawBitmapPath()
drawBitmapPath : function (context) {
    var points = this.points;
    if (points == null || points.length == 0) return;
    var angle, arrowDelta = 10, lastPoint = points.length-1, originX, originY;
    if (this.startArrow == "open") {
        context.save();
        context.beginPath();
        context.strokeStyle = this.lineColor;
        context.lineWidth = this.lineWidth;
        context.lineCap = this.lineCap;
        angle = this.computeAngle(points[0][0],points[0][1],points[1][0],points[1][1]);
        originX = points[0][0]; 
        originY = points[0][1];
        context.scale(1,1);
        context.translate(originX, originY);
        context.rotate(angle*this._radPerDeg);
        this.bmMoveTo(arrowDelta,-arrowDelta, context);
        this.bmLineTo(0,0, context);
        this.bmLineTo(arrowDelta,arrowDelta, context);
        context.stroke();
        context.restore();
    } else if (this.startArrow == "block") {
        context.save();
        context.beginPath();
        context.fillStyle = this.lineColor;
        context.lineWidth = this.lineWidth;
        context.lineCap = this.lineCap;
        angle = this.computeAngle(points[0][0],points[0][1],points[1][0],points[1][1]);
        originX = points[0][0]; 
        originY = points[0][1];
        context.translate(originX, originY);
        context.rotate(angle*this._radPerDeg);
        this.bmMoveTo(arrowDelta,-arrowDelta, context);
        this.bmLineTo(0,0, context);
        this.bmLineTo(arrowDelta,arrowDelta, context);
        context.closePath();
        context.fill();
        context.stroke();
        context.restore();
    }
    if (this.endArrow == "open") {
        context.save();
        context.beginPath();
        context.strokeStyle = this.lineColor;
        context.lineWidth = this.lineWidth;
        context.lineCap = this.lineCap;
        angle = this.computeAngle(points[lastPoint-1][0],points[lastPoint-1][1],points[lastPoint][0],points[lastPoint][1]);
        originX = points[lastPoint][0]; 
        originY = points[lastPoint][1];
        context.translate(originX, originY);
        context.rotate(angle*this._radPerDeg);
        this.bmMoveTo(-arrowDelta,arrowDelta, context);
        this.bmLineTo(0,0, context);
        this.bmLineTo(-arrowDelta,-arrowDelta, context);
        context.stroke();
        context.restore();
    } else if (this.endArrow == "block") {
        context.save();
        context.beginPath();
        context.fillStyle = this.lineColor;
        context.lineWidth = this.lineWidth;
        context.lineCap = this.lineCap;
        angle = this.computeAngle(points[lastPoint-1][0],points[lastPoint-1][1],points[lastPoint][0],points[lastPoint][1]);
        originX = points[lastPoint][0]; 
        originY = points[lastPoint][1];
        context.translate(originX, originY);
        context.rotate(angle*this._radPerDeg);
        this.bmMoveTo(-arrowDelta,arrowDelta, context);
        this.bmLineTo(0,0, context);
        this.bmLineTo(-arrowDelta,-arrowDelta, context);
        context.closePath();
        context.fill();
        context.restore();
    }
    this.bmMoveTo(points[0][0], points[0][1], context);
    for (var i = 1; i < points.length; i++) {
        var point = points[i];
        if(this.linePattern.toLowerCase() !== "solid") {
            this._drawLinePattern(points[i-1][0],points[i-1][1],points[i][0],points[i][1],context);
        } else {
            this.bmLineTo(point[0], point[1], context);
        }
    }
},

//> @method drawPath.getCenter()
// Get the mean center of the path.
// @return (Point) the mean center
// @visibility drawing
//<
getCenter : function () {
    return [isc.DrawItem._makeCoordinate(this._centerX), isc.DrawItem._makeCoordinate(this._centerY)];
},

isPointInPath : function (x, y) {
    if (this._hasFill()) return this.Super("isPointInPath", arguments);

    var tolerance = Math.max(this.lineWidth / 2, 2) + this.hitTolerance;
    var normalized = this._normalize(x, y);

    var points = this.points,
        numPoints = points.length,
        pointA,
        pointB;
    for (var i = 0; i < numPoints - 1; ++i) {
        pointA = points[i];
        pointB = points[i + 1];
        if (isc.Math.euclideanDistanceToLine(pointA[0], pointA[1], pointB[0], pointB[1], normalized[0], normalized[1]) < tolerance) {
            return true;
        }
    }
    // If this DrawPath is also a DrawPolygon, then consider the line segment between the first
    // and last points (as long as there are at least 3 points; otherwise, we have already checked).
    if (isc.isA.DrawPolygon(this) && numPoints >= 3) {
        var firstPoint = points[0],
            lastPoint = points[numPoints - 1];
        if (isc.Math.euclideanDistanceToLine(firstPoint[0], firstPoint[1], lastPoint[0], lastPoint[1], normalized[0], normalized[1]) < tolerance) {
            return true;
        }
    }
    return false;
},

//----------------------------------------
//  DrawPath attribute setters
//----------------------------------------
setPoints : function (points, cx0, cy0) {
    this.points = points;
    if (cx0 != null && cy0 != null) {
        var center = this._getRotationCenter();
        this._updateRotationCenter(cx0, cy0, center.cx, center.cy);
    }

    this._initBoundingParams();
    this._redrawAfterSetPoints(false);

    this._reshaped();
},
_redrawAfterSetPoints : function (updateVMLRendererHandleTransform) {
    if (this.drawingVML) {
        if (updateVMLRendererHandleTransform) {
            this._transform = null;
            this._vmlRendererHandle.setProperty("transform", this._getLocalTransform());
        }
        this._vmlRendererHandle.setProperty("points", this.points).flush();

    } else if (this.drawingSVG) {
        // <path> uses 'd' ("path data").
        if (this.svgElementName === "path") {
            this._svgHandle.setAttributeNS(null, "d", this.getPathSVG());

        // <polyline>, <polygon> use 'points'.
        } else {
            this._svgHandle.setAttributeNS(null, "points", this.getPointsText());
        }

    } else if (this.drawingBitmap) {
        this.drawPane.redrawBitmap();
    }
},


_initBoundingParams : function () {
    var points = this.points,
        point0 = points[0];
    if (point0 == null) {
        this._left = this._boundingBox[0],
        this._top = this._boundingBox[1],
        this._right = this._boundingBox[2],
        this._bottom = this._boundingBox[3];
        if (isc.isA.DrawDiamond(this)) {
            
            this.left = isc.DrawItem._makeCoordinate(this._left);
            this.top = isc.DrawItem._makeCoordinate(this._top);
            this.width = isc.DrawItem._makeCoordinate(this._right) - this.left;
            this.height = isc.DrawItem._makeCoordinate(this._bottom) - this.top;
        }
        this._centerX = 0;
        this._centerY = 0;
        this._convexHull = null;
        return;
    }

    var n = points.length,
        sortIndex = new Array(n);
    for (var i = n; i--; ) {
        sortIndex[i] = i;
    }
    for (var i = 1; i < n; ++i) {
        var j = i,
            p = points[sortIndex[j - 1]],
            q = points[sortIndex[j]];
        while (p[0] > q[0] || (p[0] == q[0] && p[1] > q[1])) {
            var swap = sortIndex[j - 1];
            sortIndex[j - 1] = sortIndex[j];
            sortIndex[j] = swap;
            --j;
            if (j > 0) {
                p = points[sortIndex[j - 1]];
                q = points[sortIndex[j]];
            } else {
                break;
            }
        }
    }
    

    var left = points[sortIndex[0]][0],
        right = points[sortIndex[sortIndex.length - 1]][0],
        top = points[0][1],
        bottom = top,
        convexHull = [],
        k = 0;

    for (var i = 0; i < n; ++i) {
        var j = sortIndex[i],
            p = points[j],
            px = p[0], py = p[1];

        if (py < top) {
            top = py;
        } else if (bottom < py) {
            bottom = py;
        }

        while (k > 1) {
            var q = points[convexHull[k - 2]],
                r = points[convexHull[k - 1]],
                cross = (r[0] - q[0]) * (py - r[1]) - (px - r[0]) * (r[1] - q[1]);
            if (cross <= 0) {
                --k;
            } else {
                break;
            }
        }
        convexHull[k++] = j;
    }

    --k;
    var l = 1, sumX = 0, sumY = 0;
    for (var i = n; i--; ) {
        var j = sortIndex[i],
            p = points[j],
            px = p[0], py = p[1];
        while (l > 1) {
            var m = k + l,
                q = points[convexHull[m - 2]],
                r = points[convexHull[m - 1]],
                cross = (r[0] - q[0]) * (py - r[1]) - (px - r[0]) * (r[1] - q[1]);
            if (cross <= 0) {
                --l;
            } else {
                break;
            }
        }
        convexHull[k + (l++)] = j;

        // Use this loop over the points to compute the center point.
        sumX += px;
        sumY += py;
    }

    
    convexHull.length = k + l - 1;

    

    this._left = left;
    this._top = top;
    this._right = right;
    this._bottom = bottom;
    if (isc.isA.DrawDiamond(this)) {
        
        this.left = isc.DrawItem._makeCoordinate(this._left);
        this.top = isc.DrawItem._makeCoordinate(this._top);
        this.width = isc.DrawItem._makeCoordinate(this._right) - this.left;
        this.height = isc.DrawItem._makeCoordinate(this._bottom) - this.top;
    }
    this._centerX = (n > 0 ? (sumX / n) : 0);
    this._centerY = (n > 0 ? (sumY / n) : 0);
    this._convexHull = convexHull;
},

//> @method drawPath.moveFirstPointTo(left,top)
// Move all points in the path such that the first point ends up at the specified coordinates
// and the line lengths and angles are unchanged.
//
// @param left (integer) new left coordinate in pixels
// @param top (integer) new top coordinate in pixels
// @visibility drawing
//<
moveFirstPointTo : function (left, top) {
    var startPoint = this.points[0];
    this._movePointToPoint(left, top, startPoint[0], startPoint[1]);
},
//> @method drawPath.moveBy(dX,dY)
// Move the points by dX,dY
//
// @param dX (int) delta x coordinate in pixels
// @param dY (int) delta y coordinate in pixels
// @visibility drawing
//< 

//> @method drawPath.resizeTo()
// Resize to the specified size
// @param width (integer) new width
// @param height (integer) new height
// @visibility drawing
//<

//> @method drawPath.resizeBy()
// Resize by the specified delta
// @param dX (int) number of pixels to resize by horizontally
// @param dY (int) number of pixels to resize by vertically
// @visibility drawing
//<

// drawPath._updateLocalTransform()

_updateLocalTransform : function (transform, cx, cy, initialShape, fireReshaped, viaLocalTransformOnly) {
    if (initialShape == null) {
        initialShape = this._saveShape();
    }
    
    if (viaLocalTransformOnly) {
        this.Super("_updateLocalTransform", [transform, cx, cy, initialShape, fireReshaped, true], arguments);
        return;
    }

    var epsilon = 1e-9,
        points = this.points,
        numPoints = points.length,
        convexHull = this._convexHull;

    if (numPoints > 0 &&
        convexHull != null &&
        Math.abs(transform.getDeterminant()) > epsilon &&
        this.getClass().getClassName() !== "DrawLinePath")
    {
        var invLocalTransform = initialShape.invLocalTransform;
        if (invLocalTransform == null) {
            // Applying the transform directly onto the points' coordinates leaves the local
            // transform as the identity matrix.
            this.Super(
                "_updateLocalTransform",
                [isc.AffineTransform._getIdentityTransform(), cx, cy, initialShape, false, true],
                arguments);
        } else {
            

            // Reset the components of the local transform.
            var translate = this.translate = (this.translate || new Array(2)),
                scale = this.scale = (this.scale || new Array(2));
            translate[0] = initialShape.translateX;
            translate[1] = initialShape.translateY;
            scale[0] = initialShape.scaleX;
            scale[1] = initialShape.scaleY;
            this.xShearFactor = initialShape.xShearFactor;
            this.yShearFactor = initialShape.yShearFactor;
            this.rotation = initialShape.rotation;
        }

        var shape = initialShape.points,
            t00 = transform.m00, t01 = transform.m01, t02 = transform.m02,
            t10 = transform.m10, t11 = transform.m11, t12 = transform.m12,
            dt00t01 = isc.Math._hypot(t00, t01),
            dt10t11 = isc.Math._hypot(t10, t11),
            m00 = t00, m01 = t01, m02 = t02, m10 = t10, m11 = t11, m12 = t12;
        if (invLocalTransform != null) {
            transform.leftMultiply(invLocalTransform);
            m00 = transform.m00;
            m01 = transform.m01;
            m02 = transform.m02;
            m10 = transform.m10;
            m11 = transform.m11;
            m12 = transform.m12;
        }

        var radius = (this._hasStroke() ? (this.lineWidth / 2) : 0);
        if (!(radius == 0 || (m00 == 1 && m01 == 0 && m10 == 0 && m11 == 1))) {
            
            var diameter = 2 * radius;

            // Compute the bounding box of the shape transformed by `transform`.
            var left0 = 0, right0 = 0, top0 = 0, bottom0 = 0;
            for (var i = 0, h = convexHull.length; i < h; ++i) {
                var j = 2 * convexHull[i],
                    u = shape[j], v = shape[j + 1],
                    x = t00 * u + t01 * v + t02,
                    y = t10 * u + t11 * v + t12;
                if (i == 0) {
                    left0 = right0 = x;
                    top0 = bottom0 = y;
                } else {
                    if (x < left0) left0 = x;
                    else if (right0 < x) right0 = x;
                    if (y < top0) top0 = y;
                    else if (bottom0 < y) bottom0 = y;
                }
            }
            var width0 = right0 - left0,
                height0 = bottom0 - top0;

            var dl00l01 = 1, dl10l11 = 1;
            if (invLocalTransform != null) {
                var localTransform = initialShape.localTransform;
                dl00l01 = isc.Math._hypot(localTransform.m00, localTransform.m01);
                dl10l11 = isc.Math._hypot(localTransform.m10, localTransform.m11);
            }
            var sx = (width0 + dl00l01 * diameter) / (width0 + dt00t01 * diameter),
                sy = (height0 + dl10l11 * diameter) / (height0 + dt10t11 * diameter);

            
            var diameterSq = diameter * diameter,
                dl00l01Diameter = dl00l01 * diameter,
                dl10l11Diameter = dl10l11 * diameter,
                alphaX = (
                    (dt00t01 - dl00l01) * (dt00t01 + dl00l01) * diameterSq +
                    width0 * (2 * dl00l01Diameter - width0)),
                betaX = width0 * (width0 - dl00l01Diameter),
                sgnBetaX = (width0 <= dl00l01Diameter ? -1 : 1),
                sqrtXDisc = dt00t01 * diameter * width0,
                alphaY = (
                    (dt10t11 - dl10l11) * (dt10t11 + dl10l11) * diameterSq +
                    height0 * (2 * dl10l11Diameter - height0)),
                betaY = height0 * (height0 - dl10l11Diameter),
                sgnBetaY = (height0 <= dl10l11Diameter ? -1 : 1),
                sqrtYDisc = dt10t11 * diameter * height0;
            if (alphaX != 0) {
                var sxMinus = -(betaX + sgnBetaX * sqrtXDisc) / alphaX,
                    sxPlus = -(2 * betaX / alphaX + sxMinus);
                if (sxMinus > 0 &&
                    (sxPlus <= 0 || Math.abs(sx - sxMinus) < Math.abs(sx - sxPlus)))
                {
                    sx = sxMinus;
                } else if (sxPlus > 0) {
                    sx = sxPlus;
                }
            } else if (sgnBetaX == 1) {
                sx = width0 / (2 * (width0 - dl00l01Diameter));
            }
            if (alphaY != 0) {
                var syMinus = -(betaY + sgnBetaY * sqrtYDisc) / alphaY,
                    syPlus = -(2 * betaY / alphaY + syMinus);
                if (syMinus > 0 &&
                    (syPlus <= 0 || Math.abs(sy - syMinus) < Math.abs(sy - syPlus)))
                {
                    sy = syMinus;
                } else if (syPlus > 0) {
                    sy = syPlus;
                }
            } else if (sgnBetaY == 1) {
                sy = height0 / (2 * (height0 - dl10l11 * diameter));
            }

            var dx = (1 - sx) * (left0 + right0) / 2,
                dy = (1 - sy) * (top0 + bottom0) / 2;

            // Left multiply `transform` by `[sx, 0, dx; 0, sy, dy; 0, 0, 1]^-1`.
            transform.m00 = t00 / sx;
            transform.m01 = t01 / sx;
            transform.m02 = (t02 - dx) / sx;
            transform.m10 = t10 / sy;
            transform.m11 = t11 / sy;
            transform.m12 = (t12 - dy) / sy;

            // Skip past the local transform, which will be preserved.
            if (invLocalTransform != null) {
                transform.leftMultiply(invLocalTransform);
            }

            // The remaining affine transform will be applied directly onto the points'
            // coordinates.
            m00 = transform.m00;
            m01 = transform.m01;
            m02 = transform.m02;
            m10 = transform.m10;
            m11 = transform.m11;
            m12 = transform.m12;
        }

        // Compute the new min/max x/y coordinates.
        var left = 0, right = 0, top = 0, bottom = 0;
        for (var i = 0, h = convexHull.length; i < h; ++i) {
            var j = 2 * convexHull[i],
                u = shape[j], v = shape[j + 1],
                x = m00 * u + m01 * v + m02,
                y = m10 * u + m11 * v + m12;
            if (i == 0) {
                left = right = x;
                top = bottom = y;
            } else {
                if (x < left) left = x;
                else if (right < x) right = x;
                if (y < top) top = y;
                else if (bottom < y) bottom = y;
            }
        }

        // Save the new bounding box and center point of the points.
        this._left = left;
        this._top = top;
        this._right = right;
        this._bottom = bottom;
        var isDrawDiamond = isc.isA.DrawDiamond(this);
        if (isDrawDiamond) {
            
            this.left = isc.DrawItem._makeCoordinate(this._left);
            this.top = isc.DrawItem._makeCoordinate(this._top);
            this.width = isc.DrawItem._makeCoordinate(this._right) - this.left;
            this.height = isc.DrawItem._makeCoordinate(this._bottom) - this.top;
        }
        this._centerX = m00 * initialShape.centerX + m01 * initialShape.centerY + m02;
        this._centerY = m10 * initialShape.centerX + m11 * initialShape.centerY + m12;

        // Apply the updated `transform` onto all of the points.
        for (var i = 0, j = 0; i < numPoints; ++i, j += 2) {
            var point = points[i],
                u = shape[j], v = shape[j + 1];
            point[0] = m00 * u + m01 * v + m02;
            point[1] = m10 * u + m11 * v + m12;
            if (isDrawDiamond) {
                point[0] = isc.DrawItem._makeCoordinate(point[0]);
                point[1] = isc.DrawItem._makeCoordinate(point[1]);
            }
        }

        var center = this._getRotationCenter();
        this._updateRotationCenter(cx, cy, center.cx, center.cy);

        this._redrawAfterSetPoints(this.drawingVML && (cx != center.cx || cy != center.cy));
        if (fireReshaped) {
            this._reshaped();
        }
    } else {
        this.Super("_updateLocalTransform", [transform, cx, cy, initialShape, fireReshaped, true], arguments);
    }
},

_saveShape : function () {
    var points = this.points,
        n = points.length,
        shape = new Array(n * 2);
    for (var i = 0, j = 0; i < n; ++i, j += 2) {
        var point = points[i];
        shape[j] = point[0];
        shape[j + 1] = point[1];
    }

    var translateX = 0, translateY = 0,
        scaleX = 1, scaleY = 1,
        xShearFactor = this.xShearFactor || 0,
        yShearFactor = this.yShearFactor || 0,
        rotation = this.rotation || 0;
    if (this.translate) {
        translateX = this.translate[0];
        translateY = this.translate[1];
    }
    if (this.scale && this.scale.length === 2) {
        scaleX = this.scale[0];
        scaleY = this.scale[1];
    }

    return {
        points: shape,
        centerX: this._centerX,
        centerY: this._centerY,

        
        localTransform: this._getLocalTransform(),
        invLocalTransform: this._getInverseLocalTransform(),
        translateX: translateX,
        translateY: translateY,
        scaleX: scaleX,
        scaleY: scaleY,
        xShearFactor: xShearFactor,
        yShearFactor: yShearFactor,
        rotation: rotation
    };
}

});








//> @class DrawPolygon
//
// DrawItem subclass to render polygons
//
// @inheritsFrom DrawPath
// @treeLocation Client Reference/Drawing/DrawItem
// @visibility drawing
//<
isc.defineClass("DrawPolygon", "DrawPath").addProperties({

    //> @attr drawPolygon.knobs
    // DrawPolygon only supports the
    // <smartclient>"move"</smartclient>
    // <smartgwt>{@link com.smartgwt.client.types.KnobType#MOVE}</smartgwt>
    // knob type.
    // @see DrawItem.knobs
    // @include DrawItem.knobs
    //<

    //> @attr drawPolygon.showTitleLabelBackground (boolean : false : IRA)
    // @include DrawItem.showTitleLabelBackground
    //<
    showTitleLabelBackground: false,

    //> @attr drawPolygon.points (Array of Point : [[0,0], [50,50], [100,0]] : IRW)
    // Array of points of the polygon, specified in the +link{DrawPane,local coordinate system}.
    // @visibility drawing
    //<
    points: [[0,0], [50,50], [100,0]],

    //> @attr drawPolygon.lineCap     (LineCap : "butt" : IRW)
    // Style of drawing the corners of the polygon.
    //
    // @group line
    // @visibility drawing
    //<
    lineCap: "butt",

    //> @attr drawPolygon.titleRotationMode (TitleRotationMode : "withItemAlwaysUp" : IRA)
    // @include DrawItem.titleRotationMode
    //<
    titleRotationMode: "withItemAlwaysUp",

    svgElementName: "path"
});
isc.DrawPolygon.addMethods({

_isClosed : function () {
    return true;
},

// drawPolygon._redrawAfterSetPoints()
_redrawAfterSetPoints : function (updateVMLRendererHandleTransform) {
    if (this.drawingVML) {
        if (updateVMLRendererHandleTransform) {
            this._transform = null;
            this._vmlRendererHandle.setProperty("transform", this._getLocalTransform());
        }
        var undef;
        this._vmlRendererHandle.setProperty("_getPathVML", undef).flush();
    } else if (this.drawingSVG) {
        this._svgHandle.setAttributeNS(null, "d", this.getPathSVG());
    } else if (this.drawingBitmap) {
        this.drawPane.redrawBitmap();
    }
},

_getPointsText : function (buffer) {
    var points = this.points;
    for (var i = 0, numPoints = points.length; i < numPoints; ++i) {
        buffer.append(points[i][0], " ", points[i][1], " ");
    }
    buffer.append(points[0][0], " ", points[0][1]);
},

getPathSVG : function () {
    return this.Super("getPathSVG", arguments) + " Z";
},

getAttributesSVG : function () {
    return "d='" + this.getPathSVG() + "'";
},

_vmlShapeConfigAdapter: new isc.VMLRenderer.shapeConfig(),

// drawPolygon._renderVML()
_renderVML : function (vmlRenderer, id, conversionContext) {
    
    var config = this._vmlShapeConfigAdapter;
    isc.VMLRenderer.drawItemToShapeConfig(this, config);
    this._vmlRendererHandle = vmlRenderer.shape(id, config);
},

// drawPolygon._getPathVML()
_getPathVML : function (path) {
    var points = this.points,
        numPoints = points == null ? 0 : points.length;
    if (numPoints > 0) {
        var point = points[0];
        path.m(point[0], point[1]);
        for (var i = 1; i < numPoints; ++i) {
            point = points[i];
            path.l(point[0], point[1]);
        }
        path.x();
    }
},

//----------------------------------------
//  DrawPolygon attribute setters
//----------------------------------------
drawBitmapPath : function (context) {
    var points = this.points;
    this.bmMoveTo(points[0][0], points[0][1], context);
    for (var i = 1; i < points.length; i++) {
        var point = points[i];
        this.bmLineTo(point[0], point[1], context);
    }
    context.closePath();
}

});

isc.DrawPolygon.markUnsupportedMethods(null, ["setStartArrow", "setEndArrow"]);








//------------------------------------------------------------------------------------------
//> @class DrawTriangle
//
//  DrawItem subclass to render triangles
//
//  @inheritsFrom DrawPolygon
//  @treeLocation Client Reference/Drawing/DrawItem
//  @visibility drawing
//<
//------------------------------------------------------------------------------------------

isc.defineClass("DrawTriangle", "DrawPolygon").addProperties({

    //> @attr drawTriangle.points
    // Array of points of the triangle. specified in the
    // +link{DrawPane,local coordinate system}.
    // @include DrawPolygon.points
    //<


//> @method drawTriangle.getCenter()
// Returns the +externalLink{http://en.wikipedia.org/wiki/Incenter#Cartesian_coordinates,incenter}
// of the triangle in +link{DrawPane,local coordinates}.
// @return (Point) the incenter in local coordinates
// @visibility drawing
//<
getCenter : function () {
    // Compute the Cartesian coordinates of the triangle's incenter using the formula listed at
    // http://en.wikipedia.org/wiki/Incenter#Coordinates_of_the_incenter
    // Then figure out where the incenter is as a percentage of the bounding box width and
    // height.
    var a = isc.Math.euclideanDistance(this.points[1], this.points[2]),
        b = isc.Math.euclideanDistance(this.points[0], this.points[2]),
        c = isc.Math.euclideanDistance(this.points[0], this.points[1]),
        P = a + b + c,
        u = [a, b, c],
        vx = [this.points[0][0], this.points[1][0], this.points[2][0]],
        vy = [this.points[0][1], this.points[1][1], this.points[2][1]],
        incenter = [isc.Math._dot(u, vx) / P, isc.Math._dot(u, vy) / P];

    return incenter;
}

//> @method drawTriangle.resizeBy()
// Resize by the specified delta
// @param dX (int) number of pixels to resize by horizontally
// @param dY (int) number of pixels to resize by vertically
// @visibility drawing
//<

});








//------------------------------------------------------------------------------------------
//> @class DrawDiamond
//
// DrawItem subclass to render diamond shapes
//
// @inheritsFrom DrawItem
// @treeLocation Client Reference/Drawing/DrawItem
// @visibility drawing
//<
//------------------------------------------------------------------------------------------

isc.defineClass("DrawDiamond", "DrawPolygon");

isc.DrawDiamond.addClassProperties({
    _getPoints : function (left, top, width, height) {
        
        var midX = isc.DrawItem._makeCoordinate(left + width / 2),
            midY = isc.DrawItem._makeCoordinate(top + height / 2);
        return [
            [midX, top],
            [left + width, midY],
            [midX, top + height],
            [left, midY]
        ];
    }
});

isc.DrawDiamond.addProperties({
    //> @attr drawDiamond.left (int : 0 : IRW)
    // Left coordinate of the diamond. This is the X coordinate of the western point of the
    // diamond.
    // @visibility drawing
    //<
    left:0,

    //> @attr drawDiamond.top (int : 0 : IRW)
    // Top coordinate of the diamond. This is the Y coordinate of the northern point of the
    // diamond.
    // @visibility drawing
    //<
    top:0,

    //> @attr drawDiamond.width        (int : 100 : IRW)
    // Width of the diamond. Must be non-negative.
    // @visibility drawing
    //<
    width:100,

    //> @attr drawDiamond.height       (int : 100 : IRW)
    // Height of the diamond. Must be non-negative.
    // @visibility drawing
    //<
    height:100,

    
    //> @attr drawDiamond.titleRotationMode (TitleRotationMode : "withItemAlwaysUp" : IRA)
    // @include DrawItem.titleRotationMode
    //<
    titleRotationMode: "withItemAlwaysUp",

init : function () {
    this.points = isc.DrawDiamond._getPoints(this.left, this.top, this.width, this.height);

    this.Super("init", arguments);
},

setLeft : function (newLeft) {
    
    this._setRect(newLeft, this.top, this.width, this.height);
},

setTop : function (newTop) {
    
    this._setRect(this.left, newTop, this.width, this.height);
},

setWidth : function (newWidth) {
    
    this._setRect(this.left, this.top, newWidth, this.height);
},

setHeight : function (newHeight) {
    
    this._setRect(this.left, this.top, this.width, newHeight);
},

_setRect : function (left, top, width, height) {
    
    this.left = left;
    this.top = top;
    this.width = width;
    this.height = height;
    this.setPoints(isc.DrawDiamond._getPoints(left, top, width, height));
}

});








//------------------------------------------------------------------------------------------
//> @class DrawLinePath
//
//  DrawItem subclass to render a connector between two points. If the points are aligned
//  on one axis, this connector will draw as a straight line. If the points are offset on
//  both axes and there is enough space, the connector will by default draw short horizontal 
//  segments from the start and end points, and a diagonal segment connecting the ends of these
//  'tail' segments.  Connector style and orientation defaults may be changed through
//  configuration.
//
//  @inheritsFrom DrawItem
//  @treeLocation Client Reference/Drawing/DrawItem
//  @visibility drawing
//<
//------------------------------------------------------------------------------------------

isc.defineClass("DrawLinePath", "DrawPath").addProperties({

    //> @attr drawLinePath.connectorStyle     (ConnectorStyle : "diagonal" : IR)
    // The ConnectorStyle controlling the presentation and behavior of this line's 
    // tail segments.
    //
    // @group line
    // @visibility drawing
    //<
    connectorStyle: "diagonal",

    //> @type ConnectorStyle
    // Supported styles of connector styles.
    // @value "diagonal" Center segment is drawn diagonally between tail connector segments
    // @value "rightAngle" Center segment is drawn perpendicular to tail connector segments
    // @group line
    // @visibility drawing
    //<

    //> @attr drawLinePath.connectorOrientation     (ConnectorOrientation : "auto" : IR)
    // The ConnectorOrientation controlling the orientation and behavior of this line's 
    // tail segments.
    //
    // @group line
    // @visibility drawing
    //<
    connectorOrientation: "auto",

    //> @type ConnectorOrientation
    // Supported styles of connector orientations.
    // @value "horizontal" Tail segments are always horizontal; best for left-to-right connectors
    // @value "vertical"  Tail segments are always vertical; best for top-to-bottom connectors
    // @value "auto" Tail segments flip orientation according to longer axis of bounding box: if the
    //               bounding box is wider than it is tall, center segment is vertical
    // @group line
    // @visibility drawing
    //<

    //> @attr drawLinePath.knobs
    // Array of control knobs to display for this item. Each +link{knobType} specified in this
    // will turn on UI element(s) allowing the user to manipulate this DrawLinePath.  To update the
    // set of knobs at runtime use +link{drawItem.showKnobs()} and +link{drawItem.hideKnobs()}.
    // <p>
    // DrawLinePath supports the
    // <smartclient>"startPoint", "endPoint", "controlPoint1", and "controlPoint2"</smartclient>
    // <smartgwt>{@link com.smartgwt.client.types.KnobType#STARTPOINT}, {@link com.smartgwt.client.types.KnobType#ENDPOINT}, {@link com.smartgwt.client.types.KnobType#CONTROLPOINT1}, and {@link com.smartgwt.client.types.KnobType#CONTROLPOINT2}</smartgwt>
    // knob types.
    // @see DrawItem.knobs
    // @include DrawItem.knobs
    //<

    //> @attr drawLinePath.titleRotationMode (TitleRotationMode : "withLineAlwaysUp" : IRA)
    // @include DrawItem.titleRotationMode
    //<
    titleRotationMode: "withLineAlwaysUp",

    //> @attr drawLinePath.showTitleLabelBackground (boolean : true : IRA)
    // @include DrawItem.showTitleLabelBackground
    //<
    showTitleLabelBackground: true,

    //> @attr drawLinePath.startPoint (Point : [0,0] : IRW)
    // @include drawLine.startPoint
    //<
    startPoint: [0,0],

    //> @attr drawLinePath.endPoint (Point : [100,100] : IRW)
    // @include drawLine.endPoint
    //<
    endPoint: [100,100],

    //> @attr drawLinePath.controlPoint1 (Point: null : IRW)
    // The point at which the leading tail segment joins the connecting center segment.  
    // @visibility drawing
    //<
    controlPoint1: null,

    //> @attr drawLinePath.controlPoint2 (Point: null : IRW)
    // The point at which the trailing tail segment joins the connecting center segment.
    // Has no effect on lines with right angle ConnectorStyles. 
    // @visibility drawing
    //<
    controlPoint2: null,

    //> @attr drawLinePath.tailSize (int : 30 : IR)
    // Length of the horizontal/vertical "tail segments" between the start and end points of 
    // this DrawLinePath and the connecting center segment.
    // @visibility drawing
    //<
    tailSize: 30,

    //> @attr drawLinePath.startLeft (int : 0 , IRW)
    // @include drawLine.startLeft
    //<
    //> @attr drawLinePath.startTop (int : 0 , IRW)
    // @include drawLine.startTop
    //<

    //> @attr drawLinePath.endLeft (int : 0 , IRW)
    // @include drawLine.endLeft
    //<

    //> @attr drawLinePath.endTop (int : 0 , IRW)
    // @include drawLine.endTop
    //<

    startArrow:null,

    //> @attr drawLinePath.endArrow (ArrowStyle : "open", IRW)
    // @include drawItem.endArrow
    //<
    endArrow:"open",

    init : function () {
        this.startPoint = this.startPoint.duplicate();
        this.endPoint = this.endPoint.duplicate();
        this.startLeft = (this.startLeft == 0 ? 0 : (this.startLeft || this.startPoint[0]));
        this.startTop = (this.startTop == 0 ? 0 : (this.startTop || this.startPoint[1]));
        this.endLeft = (this.endLeft == 0 ? 0 : (this.endLeft || this.endPoint[0]));
        this.endTop = (this.endTop == 0 ? 0 : (this.endTop || this.endPoint[1]));

        // function will calculate control points if uninitalized
        this.points = this._getSegmentPoints(this.controlPoint1, this.controlPoint2);

        this.Super("init", arguments);
    },

    // Returns the direction in which the tail segments should be pointed, based on the
    // longest axis of the bounding box: if the bounding box is wider than it is tall, 
    // tail segments should be horizontal.
    getConnectorOrientationState : function() {
        var orientation = this.connectorOrientation;
        if (orientation === "auto") {
            var width = Math.abs(this.endLeft - this.startLeft),
                height = Math.abs(this.endTop - this.startTop);
            orientation = (width >= height ? "horizontal" : "vertical");
        }
        return orientation;
    },

    // Calculates a DrawLinePath's leading and/or trailing tail segments, if necessary,
    // to be represented by controlPoint1 / controlPoint2 knobs. 
     _getSegmentPoints : function (leader, trailer) {    

        //the end points
        var p1 = [this.startLeft, this.startTop];
        var p2 = [this.endLeft, this.endTop];

        var orientation = this.getConnectorOrientationState();
        var style = this.connectorStyle;

        //default tail length
        var tailSize = this.tailSize;

        // draw horizontal tail segments
        if (orientation === "horizontal") {

            // find the length of the leading tail segment and draw the tail segment at the same
            // point on the x axis, making a right angle
            if (style === "rightAngle") {

                if (this.controlPoint1 == null) {
                    tailSize = this.startLeft <= this.endLeft ? -this.tailSize : this.tailSize;
                } else {
                    tailSize = this.startLeft - this.controlPoint1[0];
                }

                leader = [this.startLeft - tailSize, this.startTop];
                trailer = [this.startLeft - tailSize, this.endTop];
            
            } else {

                if (this.startLeft <= this.endLeft) {
                    tailSize = -tailSize;
                }
            
                // don't change any point explicitly provided by the end user (as in move, etc)
                leader = leader || [this.startLeft - tailSize, this.startTop];
                trailer = trailer || [this.endLeft + tailSize, this.endTop];
            }
        }

        if (orientation === "vertical") {

            // find the length of the leading tail segment and draw the tail segment at the same
            // point on the y axis, making a right angle
            if (style === "rightAngle") {
            
                if (this.controlPoint1 == null) {
                    tailSize = this.startTop <= this.endTop ? -this.tailSize : this.tailSize;
                } else {
                    tailSize = this.startTop - this.controlPoint1[1];
                }

                leader = [this.startLeft, this.startTop - tailSize];
                trailer = [this.endLeft, this.startTop - tailSize];
            
            } else {
            
                if (this.startTop <= this.endTop) {
                    tailSize = -tailSize;
                }

                // don't change any point explicitly provided by the end user (as in move, etc)
                leader = leader || [this.startLeft, this.startTop - tailSize];
                trailer = trailer || [this.endLeft, this.endTop + tailSize];
            }
        }

        if (style === "rightAngle") {
            this.controlPoint1 = this.getCenter(leader, trailer);
        } else {
            this.controlPoint1 = leader;
            this.controlPoint2 = trailer;
        }
        
        this._segmentPoints = [p1, leader, trailer, p2];

        return this._segmentPoints;    
    },

    _drawLineStartArrow : function () {
        return false;
    },

    _drawLineEndArrow : function () {
        return false;
    },

    //> @method drawLinePath.getCenter()
    // Get the center point of the line path.
    // @return (Point) the center point
    // @visibility drawing
    //<
    getCenter : function (p1, p2) {

        var startPoint = p1 || this.startPoint;
        var endPoint = p2 || this.endPoint;

        return [startPoint[0] + isc.DrawItem._makeCoordinate((endPoint[0] - startPoint[0])/2), startPoint[1] + isc.DrawItem._makeCoordinate((endPoint[1] - startPoint[1])/2)]; 
    },

    //----------------------------------------
    //  DrawLinePath attribute setters
    //----------------------------------------


    //> @method drawLinePath.setStartPoint()
    // @include drawLine.setStartPoint()
    //<
    setStartPoint : function (left, top, fireMovedAndResized, cx0, cy0) {
        if (isc.isAn.Array(left)) { // conversion needed for SGWT
            top = left[1];
            left = left[0];
        }

        

        this.startLeft = this.startPoint[0] = left;
        this.startTop = this.startPoint[1] = top;

        // regenerate points
        
        this.setPoints(this._getSegmentPoints(null, this.controlPoint2), cx0, cy0);
    },

    //> @method drawLinePath.setEndPoint()
    // @include drawLine.setEndPoint()
    //<
    setEndPoint : function (left, top, fireMovedAndResized, cx0, cy0) {
        if (isc.isAn.Array(left)) { // conversion needed for SGWT
            top = left[1];
            left = left[0];
        }

        

        this.endLeft = this.endPoint[0] = left;
        this.endTop = this.endPoint[1] = top;

        // regenerate points
        
        this.setPoints(this._getSegmentPoints(this.controlPoint1, null), cx0, cy0);
    },

    //> @method drawLinePath.setControlPoint1()
    // Sets the coordinates of the controlPoint1 knob and by extension the coordinates of this
    // DrawLinePath's leading tail segment.
    // @param left (int) left coordinate for start point, in pixels
    // @param top (int) top coordinate for start point, in pixels
    // @visibility drawing
    //<
    setControlPoint1 : function (left, top, fireMovedAndResized, cx0, cy0) {
        if (isc.isAn.Array(left)) { // conversion needed for SGWT
            top = left[1];
            left = left[0];
        }

        

        this.controlPoint1[0] = left;
        this.controlPoint1[1] = top;

        // regenerate points so that the line gets dragged along with the knob
        this.setPoints(this._getSegmentPoints(this.controlPoint1, this.controlPoint2), cx0, cy0);
    },

    //> @method drawLinePath.setControlPoint2()
    // Sets the coordinates of the controlPoint2 knob and by extension the coordinates of this
    // DrawLinePath's trailing tail segment.
    // @param left (int) left coordinate for start point, in pixels
    // @param top (int) top coordinate for start point, in pixels
    // @visibility drawing
    //<
    setControlPoint2 : function (left, top, fireMovedAndResized, cx0, cy0) {
        if (isc.isAn.Array(left)) { // conversion needed for SGWT
            top = left[1];
            left = left[0];
        }

        

        this.controlPoint2[0] = left;
        this.controlPoint2[1] = top;

        // regenerate points so that the line gets dragged along with the knob
        this.setPoints(this._getSegmentPoints(this.controlPoint1, this.controlPoint2), cx0, cy0);
    },



    //> @method drawLinePath.getBoundingBox()
    // Returns the startPoint, endPoint
    // @return (Array[] of double) x1, y1, x2, y2 coordinates
    // @visibility drawing
    //<
    getBoundingBox : function (includeStroke, outputBox) {
        
        var x1 = this.startPoint[0],
            y1 = this.startPoint[1],
            x2 = this.endPoint[0],
            y2 = this.endPoint[1],
            bbox = (outputBox || new Array(4));
        bbox[0] = Math.min(x1, x2);
        bbox[1] = Math.min(y1, y2);
        bbox[2] = Math.max(x1, x2);
        bbox[3] = Math.max(y1, y2);
        return includeStroke != true ? bbox : this._adjustBoundingBox(true, false, bbox);
    },

    showResizeKnobs : null,
    hideResizeKnobs : null,

    showMoveKnobs : null,
    hideMoveKnobs : null,

    // steal start/endpoint knobs functions from drawLine
    showStartPointKnobs : isc.DrawLine.getPrototype().showStartPointKnobs,
    hideStartPointKnobs : isc.DrawLine.getPrototype().hideStartPointKnobs,
    showEndPointKnobs : isc.DrawLine.getPrototype().showEndPointKnobs,
    hideEndPointKnobs : isc.DrawLine.getPrototype().hideEndPointKnobs,

    c1KnobDefaults: {
        cursor: "move",
        knobShapeProperties: {
            _constructor: "DrawOval",
            radius: 4.5,
            lineWidth: 1,
            lineOpacity: 1,
            fillOpacity: 1,
            lineColor: "#333333",
            fillGradient: {
                id: "isc_defaultDrawLinePathC1KnobGradient",
                x1: "0%", y1: "0%",
                x2: "0%", y2: "100%",
                colorStops: [
                    {color: "#ffffff", offset: 0.15},
                    {color: "#ecc1cc", offset: 0.5},
                    {color: "#ffffff", offset: 0.85}
                ]
            }
        }
    },

    showControlPoint1Knobs : function() {

        if (this._c1Knob == null || this._c1Knob.destroyed) {
            
            var v = this._normalize(
                    this.controlPoint1[0], this.controlPoint1[1], "local", "global");
            this._c1Knob = this.createAutoChild("c1Knob", {
                _constructor: "DrawKnob",
                x: v[0],
                y: v[1],
                drawPane: this.drawPane,

                resetKnobPosition : function () {
                    var drawItem = this.creator,
                        v = drawItem._normalize(
                            drawItem.controlPoint1[0], drawItem.controlPoint1[1], "local", "global");
                    this.setCenterPoint(v[0], v[1], false);
                },

                updatePoints : function (x, y, dx, dy, state) {
                    var drawItem = this.creator,
                        orientation = drawItem.getConnectorOrientationState(),
                        v = drawItem._normalize(x, y, "global", "local"),
                        w = drawItem._normalize(dx, dy, "global", "local"),
                        z = drawItem._normalize(0, 0, "global", "local");
                    x = v[0];
                    y = v[1];
                    dx = w[0] - z[0];
                    dy = w[1] - z[1];

                    // Restrict movement to the axis appropriate for a given orientation.
                    if (orientation === "horizontal") {
                        y -= dy;
                    } else {
                        x -= dx;
                    }

                    var center = drawItem._getRotationCenter();
                    drawItem.setControlPoint1(
                        isc.DrawItem._makeCoordinate(x), isc.DrawItem._makeCoordinate(y),
                        false,
                        center.cx, center.cy);
                }
            });
        }
    },
    hideControlPoint1Knobs : function() {
        if (this._c1Knob) {
            this._c1Knob.destroy();
            delete this._c1Knob;
        }
    },

    c2KnobDefaults: {
        cursor: "move",
        knobShapeProperties: {
            _constructor: "DrawOval",
            radius: 4.5,
            lineWidth: 1,
            lineOpacity: 1,
            fillOpacity: 1,
            lineColor: "#333333",
            fillGradient: {
                id: "isc_defaultDrawLinePathC2KnobGradient",
                x1: "0%", y1: "0%",
                x2: "0%", y2: "100%",
                colorStops: [
                    {color: "#ffffff", offset: 0.15},
                    {color: "#ecc1cc", offset: 0.5},
                    {color: "#ffffff", offset: 0.85}
                ]
            }
        }
    },

    showControlPoint2Knobs : function() {

        if (this.connectorStyle === "diagonal" && (this._c2Knob == null || this._c2Knob.destroyed)) {
            
            var v = this._normalize(
                        this.controlPoint2[0], this.controlPoint2[1], "local", "global");
            this._c2Knob = this.createAutoChild("c2Knob", {
                _constructor: "DrawKnob",
                x: v[0],
                y: v[1],
                drawPane: this.drawPane,

                resetKnobPosition : function () {
                    var drawItem = this.creator,
                        v = drawItem._normalize(
                            drawItem.controlPoint2[0], drawItem.controlPoint2[1], "local", "global");
                    this.setCenterPoint(v[0], v[1], false);
                },

                updatePoints : function (x, y, dx, dy, state) {
                    var drawItem = this.creator,
                        orientation = drawItem.getConnectorOrientationState(),
                        v = drawItem._normalize(x, y, "global", "local"),
                        w = drawItem._normalize(dx, dy, "global", "local"),
                        z = drawItem._normalize(0, 0, "global", "local");
                    x = v[0];
                    y = v[1];
                    dx = w[0] - z[0];
                    dy = w[1] - z[1];

                    // Restrict movement to the axis appropriate for a given orientation.
                    if (orientation === "horizontal") {
                        y -= dy;
                    } else {
                        x -= dx;
                    }

                    var center = drawItem._getRotationCenter();
                    drawItem.setControlPoint2(
                        isc.DrawItem._makeCoordinate(x), isc.DrawItem._makeCoordinate(y),
                        false,
                        center.cx, center.cy);
                }
            });
        }
    },
    hideControlPoint2Knobs : function() {
        if (this._c2Knob) {
            this._c2Knob.destroy();
            delete this._c2Knob;
        }
    },

    updateStartPointKnob : function() {
        if (this._startKnob) {
            var v = this._normalize(this.startLeft, this.startTop, "local", "global");
            this._startKnob.setCenterPoint(v[0], v[1]);
        }
    },
    updateEndPointKnob : function() {
        if (this._endKnob) {
            var v = this._normalize(this.endLeft, this.endTop, "local", "global");
            this._endKnob.setCenterPoint(v[0], v[1]);
        }
    },
    updateControlPoint1Knob : function() {
        if (this._c1Knob) {
            var v = this._normalize(
                    this.controlPoint1[0], this.controlPoint1[1], "local", "global");
            this._c1Knob.setCenterPoint(v[0], v[1]);
        }
    },
    updateControlPoint2Knob : function() {
        if (this._c2Knob) {
            var v = this._normalize(
                    this.controlPoint2[0], this.controlPoint2[1], "local", "global");
            this._c2Knob.setCenterPoint(v[0], v[1]);
        }
    },
    updateControlKnobs : function() {
        // update the position of our start/end point knobs when we update our other control points
        this.Super("updateControlKnobs", arguments);
        this.updateStartPointKnob();
        this.updateEndPointKnob();
        this.updateControlPoint1Knob();
        this.updateControlPoint2Knob();
    },

    //> @method drawLinePath.moveBy()
    // @include drawLine.moveBy()
    //<

    //> @method drawLinePath.moveStartPointTo()
    // Moves the line path such that the +link{startPoint} ends up at the specified point.
    //
    // @param left (integer) new left coordinate in pixels
    // @param top (integer) new top coordinate in pixels
    // @visibility drawing
    //<
    moveStartPointTo : function (left, top) {
        this._movePointToPoint(left, top, this.startLeft, this.startTop);
    },

    _updateLocalTransform : function (transform, cx, cy, initialShape, fireReshaped, viaLocalTransformOnly) {
        
        if (viaLocalTransformOnly) {
            this.Super("_updateLocalTransform", [transform, cx, cy, initialShape, fireReshaped, true], arguments);
            return;
        }

        var epsilon = 1e-9,
            det = transform.getDeterminant();
        if (Math.abs(det) > epsilon) {
            var dx = isc.DrawItem._makeCoordinate(
                    transform.m02 * transform.m11 - transform.m12 * transform.m01),
                dy = isc.DrawItem._makeCoordinate(
                    transform.m12 * transform.m00 - transform.m02 * transform.m10);
            this.Super(
                "_updateLocalTransform",
                [transform.duplicate().rightMultiply(
                    isc.AffineTransform.getTranslateTransform(-dx, -dy)),
                 cx, cy, initialShape, false, true],
                arguments);

            this.startLeft += dx;
            this.startPoint[0] += dx;
            this.startTop += dy;
            this.startPoint[1] += dy;
            this.endLeft += dx;
            this.endPoint[0] += dx;
            this.endTop += dy;
            this.endPoint[1] += dy;

            this.controlPoint1[0] += dx;
            this.controlPoint1[1] += dy;

            this.controlPoint2[0] += dx;
            this.controlPoint2[1] += dy;

            var center = this._getRotationCenter();
            this._updateRotationCenter(cx, cy, center.cx, center.cy);

            if (cx != center.cx || cy != center.cy) {
                this._transform = null;
                if (this.drawingVML) {
                    this._vmlRendererHandle.setProperty("transform", this._getLocalTransform());

                } else if (this.drawingSVG) {
                    var t = this._getLocalTransform();
                    this._svgHandle.setAttributeNS(null, "transform", "matrix(" + t.m00 + " " + t.m10 + " " +
                                                                                  t.m01 + " " + t.m11 + " " +
                                                                                  t.m02 + " " + t.m12 + ")");
                }
            }

            // regenerate points
            this.setPoints(this._getSegmentPoints(this.controlPoint1, this.controlPoint2));

        } else {
            this.Super("_updateLocalTransform", [transform, cx, cy, initialShape, fireReshaped, true], arguments);
        }
    }

}); // end DrawLinePath.addProperties








//> @type DrawShapeCommandType
// @value "close" Draws a straight line from the current point to the last "moveto" point. There
// are no arguments.
// @value "moveto" Start a new sub-path at a given (x,y) coordinate. The args array for this
// command type is a two-element array of the X and Y coordinates.
// @value "lineto" Draw a line from the current point to the given (x,y) coordinate which becomes
// the new current point. Multiple (x,y) coordinates may be specified to draw a path, in which
// case the last point becomes the new current point. The args array for this command type is
// an array of one or more Points (two-element arrays of the X and Y coordinates).
// @value "circleto" Draw a segment of a specified circle. A straight line (the "initial line
// segment") is drawn from the current point to the start of the circular arc. The args array
// for this command type contains 4 values:
// <ol start="0">
// <li>The center (cx,cy) Point (two-element array) of the circle.</li>
// <li>radius</li>
// <li>startAngle - Start angle in degrees</li>
// <li>endAngle - End angle in degrees</li>
// </ol>
// Note that the +explorerExample{circletoCommand,"circleto" Command example} can be very helpful
// when learning how to write "circleto" commands.
// @visibility drawing
//<


//> @object DrawShapeCommand
// Holds the information of a drawing command.
// @treeLocation Client Reference/Drawing/DrawItem/DrawShape
// @visibility drawing
//<

//> @attr drawShapeCommand.type (DrawShapeCommandType : null : IR)
// The command type.
// @visibility drawing
//<

//> @attr drawShapeCommand.args (Array : null : IR)
// The command arguments. The number of arguments and their types depend on this command's +link{type,type}.
// @visibility drawing
//<

//> @class DrawShape
// DrawItem to render a shape defined by executing the series of drawing commands in the
// +link{DrawShape.commands,commands} array.
// @inheritsFrom DrawItem
// @treeLocation Client Reference/Drawing/DrawItem
// @visibility drawing
//<
isc.defineClass("DrawShape", "DrawItem");

isc.DrawShape.addClassProperties({

    _$close: "close",
    _$moveto: "moveto",
    _$lineto: "lineto",
    _$circleto: "circleto"

});

isc.DrawShape.addProperties({

    //> @attr drawShape.titleRotationMode (TitleRotationMode : "withItemAlwaysUp" : IRA)
    // @include DrawItem.titleRotationMode
    //<
    titleRotationMode: "withItemAlwaysUp",

    svgElementName: "path",

    //> @attr drawShape.commands (Array of DrawShapeCommand : null : IRW)
    // The drawing commands that will be executed to render the shape.
    //
    // @visibility drawing
    //<
    //commands: null,

    
    fillSubpathsIndependently: false,


//> @method drawShape.setCommands()
// Sets the +link{commands,commands} that define this shape.
// @param commands (Array of DrawShapeCommand) the new commands.
// @visibility drawing
//<
setCommands : function (commands) {
    this.commands = commands;
    if (this.drawingVML) {
        var undef;
        this._vmlRendererHandle.setProperty("_getPathVML", undef).flush();
    } else if (this.drawingSVG) {
        this._svgHandle.setAttributeNS(null, "d", this._getPathDataSVG());
    } else if (this.drawingBitmap) {
        this.drawPane.redrawBitmap();
    }
    this._reshaped();
    this._initBoundingParams();
},

// Sets correct values for left, top, width, height based on points' values.
_initBoundingParams : function (includeStroke) {
    var bb = this.getBoundingBox(false, this._tempBoundingBox);
    this.left = bb[0];
    this.top = bb[1];
    this.width = bb[2] - bb[0];
    this.height = bb[3] - bb[1];
},

getBoundingBox : function (includeStroke, outputBox) {
    
    var box = (outputBox || new Array(4)),
        commands = this.commands,
        drawPane = this.drawPane,
        innerContentWidth = drawPane._getViewPortWidth(),
        innerContentHeight = drawPane._getViewPortHeight(),
        left = innerContentWidth,
        top = innerContentHeight,
        right = 0,
        bottom = 0,
        first = true,
        justMove = true,
        currentPoint = null;
    if (!commands || commands.length == 0) {
        box[0] = box[1] = box[2] = box[3] = 0;
        return box;
    }
    for (var i = 0, numCommands = commands.length; i < numCommands; ++i) {
        var command = commands[i],
            type = command.type,
            args = command.args;
        if (type == "moveto") {
            currentPoint = args;
        } else if (type == "lineto") {
            justMove = false;
            if (currentPoint != null) {
                if (first) {
                    left = right = currentPoint[0];
                    top = bottom = currentPoint[1];
                    first = false;
                } else {
                    if (currentPoint[0] < left) left = currentPoint[0];
                    else if (right < currentPoint[0]) right = currentPoint[0];
                    if (currentPoint[1] < top) top = currentPoint[1];
                    else if (bottom < currentPoint[1]) bottom = currentPoint[1];
                }
                currentPoint = null;
            }
            for (var j = args.length; j--; ) {
                var point = args[j];
                if (point[0] < left) left = point[0];
                else if (right < point[0]) right = point[0];
                if (point[1] < top) top = point[1];
                else if (bottom < point[1]) bottom = point[1];
            }
        } else if (type == "circleto") {
            justMove = false;

            var centerPoint = args[0],
                cx = centerPoint[0],
                cy = centerPoint[1],
                radius = args[1],
                startAngle = args[2],
                endAngle = args[3],
                wrap = (startAngle > endAngle);

            if (currentPoint != null) {
                if (first) {
                    left = right = currentPoint[0];
                    top = bottom = currentPoint[1];
                    first = false;
                } else {
                    if (currentPoint[0] < left) left = currentPoint[0];
                    else if (right < currentPoint[0]) right = currentPoint[0];
                    if (currentPoint[1] < top) top = currentPoint[1];
                    else if (bottom < currentPoint[1]) bottom = currentPoint[1];
                }
                currentPoint = null;
            }

            var startX = cx + radius * Math.cos(startAngle * Math.PI / 180),
                startY = cy + radius * Math.sin(startAngle * Math.PI / 180),
                endX = cx + radius * Math.cos(endAngle * Math.PI / 180),
                endY = cy + radius * Math.sin(endAngle * Math.PI / 180);
            if (startX < left) left = startX;
            else if (right < startX) right = startX;
            if (startY < top) top = startY;
            else if (bottom < startY) bottom = startY;
            if (endX < left) left = endX;
            else if (right < endX) right = endX;
            if (endY < top) top = endY;
            else if (bottom < endY) bottom = endY;

            if (wrap ? (startAngle < 270 || 270 < endAngle) : (startAngle < 270 && 270 < endAngle)) {
                var cBottom = cy + radius;
                if (bottom < cBottom) bottom = cBottom;
            }
            if (wrap ? (startAngle < 90 || 90 < endAngle) : (startAngle < 90 && 90 < endAngle)) {
                var cTop = cy - radius;
                if (cTop < top) top = cTop;
            }
            if (wrap ? (startAngle < 180 || 180 < endAngle) : (startAngle < 180 && 180 < endAngle)) {
                var cLeft = cx - radius;
                if (cLeft < left) left = cLeft;
            }
            if (wrap ? (startAngle < 0 || 0 < endAngle) : (startAngle < 0 && 0 < endAngle)) {
                var cRight = cx + radius;
                if (right < cRight) right = cRight;
            }
        }
    }

    // If all of the commands are just "moveto"s then there is no real bounding box because
    // nothing is drawn.  In this case say that the bounding box is 0-width and -height box at
    // the last point moved to.
    if (justMove) {
        if (currentPoint == null) {
            left = right = top = bottom = 0;
        } else {
            left = right = currentPoint[0];
            top = bottom = currentPoint[1];
        }
    }

    box[0] = left;
    box[1] = top;
    box[2] = right;
    box[3] = bottom;
    return box;
},

_getBoundingBoxOfTransformedShape : function (
    transform, includeStroke, includeHitTolerance, outputBox)
{
    
    var convert = this._radPerDeg,
        box = outputBox || new Array(4);

    var commands = this.commands,
        left = this.drawPane._getViewPortWidth(),
        top = this.drawPane._getViewPortHeight(),
        right = 0,
        bottom = 0,
        first = true,
        justMove = true,
        currentPoint = null;

    if (!commands || commands.length == 0) {
        box[0] = box[1] = box[2] = box[3] = 0;
        return box;
    }

    var m00 = transform.m00, m01 = transform.m01, m02 = transform.m02,
        m10 = transform.m10, m11 = transform.m11, m12 = transform.m12,
        dm00m01 = isc.Math._hypot(m00, m01),
        dm10m11 = isc.Math._hypot(m10, m11),
        gamma = m00 * m10 + m01 * m11;

    for (var i = 0, numCommands = commands.length; i < numCommands; ++i) {
        var command = commands[i],
            type = command.type,
            args = command.args;
        if (type == "moveto") {
            currentPoint = args;
        } else if (type == "lineto" || type == "circleto") {
            justMove = false;
            if (currentPoint != null) {
                var x0 = m00 * currentPoint[0] + m01 * currentPoint[1] + m02,
                    y0 = m10 * currentPoint[0] + m11 * currentPoint[1] + m12;
                if (first) {
                    left = right = x0;
                    top = bottom = y0;
                    first = false;
                } else {
                    if (x0 < left) left = x0;
                    else if (right < x0) right = x0;
                    if (y0 < top) top = y0;
                    else if (bottom < y0) bottom = y0;
                }
                currentPoint = null;
            }

            if (type == "lineto") {
                for (var j = args.length; j--; ) {
                    var point = args[j],
                        x = m00 * point[0] + m01 * point[1] + m02,
                        y = m10 * point[0] + m11 * point[1] + m12;
                    if (x < left) left = x;
                    else if (right < x) right = x;
                    if (y < top) top = y;
                    else if (bottom < y) bottom = y;
                }
            } else { // type == "circleto"
                var centerPoint = args[0],
                    cx = centerPoint[0],
                    cy = centerPoint[1],
                    radius = args[1],
                    startAngle = args[2],
                    endAngle = args[3],
                    anticlockwise = (endAngle < startAngle),
                    diffAngle = endAngle - startAngle,
                    wholeCircle = (
                        diffAngle >= 360 ||
                        (diffAngle != 0 && Math.round(diffAngle) % 360 == 0));

                var oldStartX = cx + radius * Math.cos(startAngle * Math.PI / 180),
                    oldStartY = cy + radius * Math.sin(startAngle * Math.PI / 180),
                    oldEndX = cx + radius * Math.cos(endAngle * Math.PI / 180),
                    oldEndY = cy + radius * Math.sin(endAngle * Math.PI / 180),
                    startX = m00 * oldStartX + m01 * oldStartY + m02,
                    startY = m10 * oldStartX + m11 * oldStartY + m12,
                    endX = m00 * oldEndX + m01 * oldEndY + m02,
                    endY = m10 * oldEndX + m11 * oldEndY + m12;

                if (startX < left) left = startX;
                else if (right < startX) right = startX;
                if (startY < top) top = startY;
                else if (bottom < startY) bottom = startY;
                if (endX < left) left = endX;
                else if (right < endX) right = endX;
                if (endY < top) top = endY;
                else if (bottom < endY) bottom = endY;

                var centerX = m00 * cx + m01 * cy + m02,
                    centerY = m10 * cx + m11 * cy + m12,
                    halfArcWidth = dm00m01 * radius,
                    halfArcHeight = dm10m11 * radius,
                    minArcX = centerX - halfArcWidth,
                    minArcY = centerY - halfArcHeight,
                    maxArcX = centerX + halfArcWidth,
                    maxArcY = centerY + halfArcHeight,
                    startTheta = Math.atan2(startY - centerY, startX - centerX),
                    endTheta = Math.atan2(endY - centerY, endX - centerX),
                    temp = gamma * radius,
                    tempXy = (dm00m01 <= 0 ? 0 : (temp / dm00m01)),
                    tempYx = (dm10m11 <= 0 ? 0 : (temp / dm10m11)),
                    minArcXy = (centerY - tempXy),
                    minArcYx = (centerX - tempYx),
                    maxArcXy = (centerY + tempXy),
                    maxArcYx = (centerX + tempYx),
                    minArcXTheta = Math.atan2(minArcXy - centerY, minArcX - centerX),
                    minArcYTheta = Math.atan2(minArcY - centerY, minArcYx - centerX),
                    maxArcXTheta = Math.atan2(maxArcXy - centerY, maxArcX - centerX),
                    maxArcYTheta = Math.atan2(maxArcY - centerY, maxArcYx - centerX);

                if (anticlockwise) {
                    var swapTheta = startTheta;
                    startTheta = endTheta;
                    endTheta = swapTheta;
                }
                var wrap = (startTheta > endTheta);
                if (minArcX < left && (wrap ?
                    (startTheta < minArcXTheta || minArcXTheta < endTheta) :
                    (startTheta < minArcXTheta && minArcXTheta < endTheta)))
                {
                    left = minArcX;
                }
                if (minArcY < top && (wrap ?
                    (startTheta < minArcYTheta || minArcYTheta < endTheta) :
                    (startTheta < minArcYTheta && minArcYTheta < endTheta)))
                {
                    top = minArcY;
                }
                if (right < maxArcX && (wrap ?
                    (startTheta < maxArcXTheta || maxArcXTheta < endTheta) :
                    (startTheta < maxArcXTheta && maxArcXTheta < endTheta)))
                {
                    right = maxArcX;
                }
                if (bottom < maxArcY && (wrap ?
                    (startTheta < maxArcYTheta || maxArcYTheta < endTheta) :
                    (startTheta < maxArcYTheta && maxArcYTheta < endTheta)))
                {
                    bottom = maxArcY;
                }
            }
        }
    }

    if (justMove) {
        // If all of the commands are just "moveto"s then there is no real bounding box
        // because nothing is drawn.  In this case say that the bounding box is 0-width and
        // -height box at the last point moved to.
        if (currentPoint == null) {
            left = right = top = bottom = 0;
        } else {
            left = right = currentPoint[0];
            top = bottom = currentPoint[1];
        }
    } else {
        // Adjust the bounding box by the lineWidth and/or hitTolerance.
        var halfLineWidth = (includeStroke && this._hasStroke() ? (this.lineWidth / 2) : 0),
            hitTolerance = (includeHitTolerance ? this.hitTolerance : 0),
            strokeRadius = halfLineWidth + hitTolerance,
            halfStrokeRadiusWidth = dm00m01 * strokeRadius,
            halfStrokeRadiusHeight = dm10m11 * strokeRadius;
        left -= halfStrokeRadiusWidth;
        right += halfStrokeRadiusWidth;
        top -= halfStrokeRadiusHeight;
        bottom += halfStrokeRadiusHeight;
    }

    box[0] = left;
    box[1] = top;
    box[2] = right;
    box[3] = bottom;
    return box;
},

//> @method drawShape.moveBy()
// Move the drawShape by the specified delta
// @param dX (int) number of pixels to move horizontally
// @param dY (int) number of pixels to move vertically
// @visibility drawing
//<

//> @method drawShape.resizeBy()
// Resize by the specified delta
// @param dX (int) number of pixels to resize by horizontally
// @param dY (int) number of pixels to resize by vertically
// @visibility drawing
//<

// drawShape._updateLocalTransform()
_updateLocalTransform : function (transform, cx, cy, initialShape, fireReshaped, viaLocalTransformOnly) {
    
    if (viaLocalTransformOnly) {
        this.Super("_updateLocalTransform", [transform, cx, cy, initialShape, fireReshaped, true], arguments);
        return;
    }

    var epsilon = 1e-9,
        commands = this.commands;
    if (isc.isAn.Array(commands) &&
        commands.length > 0 &&
        Math.abs(transform.getDeterminant()) > epsilon)
    {
        var close = this._$close,
            moveto = this._$moveto,
            lineto = this._$lineto,
            circleto = this._$circleto,
            numCommands = commands.length;

        if (commands.containsProperty("type", circleto)) {
            var twopi = 2 * Math.PI,
                radPerDeg = isc.DrawItem._radPerDeg,
                decomp = isc.AffineTransform._decomposeTransform(transform, cx, cy),
                theta = decomp.theta,
                sin = Math.sin(theta),
                cos = Math.cos(theta),
                alpha1 = (decomp.kx * decomp.ky + 1) * decomp.sx,
                alpha2 = decomp.sy,
                // Choose a positive alpha to scale by so that we do not have to consider
                // flips.
                alpha = Math.min(Math.abs(alpha1), Math.abs(alpha2)),
                t = isc.AffineTransform.create();
            
            if (alpha == 0) {
                alpha = 1;
            }
            var scale = alpha / (decomp.sx * decomp.sy),
                dx = scale * (decomp.dx * decomp.sy - decomp.dy * decomp.kx * decomp.sx),
                dy = -scale * (
                    decomp.dx * decomp.ky * decomp.sy -
                    decomp.dy * (1 + decomp.kx * decomp.ky) * decomp.sx);
            t.translate(dx, dy);
            if (alpha != 1) {
                t.scale(alpha, alpha);
            }
            t.rotate(theta / radPerDeg, cx, cy);

            for (var i = numCommands; i--; ) {
                var command = commands[i],
                    type = command.type,
                    args = command.args;

                if (type == moveto) {
                    var v = t.transform(args[0], args[1]);
                    args[0] = v[0];
                    args[1] = v[1];
                } else if (type == lineto) {
                    for (var j = args.length; j--; ) {
                        var point = args[j],
                            v = t.transform(point[0], point[1]);
                        point[0] = v[0];
                        point[1] = v[1];
                    }
                } else if (type == circleto) {
                    var centerPoint = args[0],
                        radius = args[1],
                        startAngle = args[2] * radPerDeg,
                        endAngle = args[3] * radPerDeg,
                        anticlockwise = (args[2] < args[3]),
                        x0 = centerPoint[0], y0 = centerPoint[1],
                        startX = x0 + radius * Math.cos(startAngle),
                        startY = y0 + radius * Math.sin(startAngle),
                        endX = x0 + radius * Math.cos(endAngle),
                        endY = y0 + radius * Math.sin(endAngle);

                    var u = t.transform(x0, y0),
                        x1 = centerPoint[0] = u[0],
                        y1 = centerPoint[1] = u[1];
                    args[1] = alpha * radius;

                    // Calculate the new start and end angles.
                    var v = t.transform(startX, startY),
                        newStartAngle = Math.atan2(v[1] - y1, v[0] - x1),
                        w = t.transform(endX, endY),
                        newEndAngle = Math.atan2(w[1] - y1, w[0] - x1);

                    // Preserve `anticlockwise`.
                    newStartAngle = (twopi + (newStartAngle % twopi)) % twopi;
                    newEndAngle = (twopi + (newEndAngle % twopi)) % twopi;
                    if (anticlockwise) {
                        // Enforce `newStartAngle < newEndAngle`.
                        if (newStartAngle >= newEndAngle) {
                            newEndAngle += twopi;
                        }
                    } else {
                        // Enforce `newStartAngle >= newEndAngle`.
                        if (newStartAngle < newEndAngle) {
                            newStartAngle += twopi;
                        }
                    }
                    args[2] = newStartAngle / radPerDeg;
                    args[3] = newEndAngle / radPerDeg;
                }
            }

            transform.rightMultiply(t.getInverse());
        } else {
            for (var i = numCommands; i--; ) {
                var command = commands[i],
                    type = command.type,
                    args = command.args;

                if (type == moveto) {
                    var v = transform.transform(args[0], args[1]);
                    args[0] = v[0];
                    args[1] = v[1];
                } else if (type == lineto) {
                    for (var j = args.length; j--; ) {
                        var point = args[j],
                            v = transform.transform(point[0], point[1]);
                        point[0] = v[0];
                        point[1] = v[1];
                    }
                }
            }

            transform = isc.AffineTransform._getIdentityTransform();
        }

        this.Super("_updateLocalTransform", [transform, cx, cy, initialShape, false, true], arguments);

        var center = this._getRotationCenter();
        this._updateRotationCenter(cx, cy, center.cx, center.cy);

        if (this.drawingVML) {
            if (cx != center.cx || cy != center.cy) {
                this._transform = null;
                this._vmlRendererHandle.setProperty("transform", this._getLocalTransform());
            }
        } else if (this.drawingSVG) {
            if (cx != center.cx || cy != center.cy) {
                this._transform = null;
                var t = this._getLocalTransform();
                this._svgHandle.setAttributeNS(null, "transform", "matrix(" + t.m00 + " " + t.m10 + " " +
                                                                              t.m01 + " " + t.m11 + " " +
                                                                              t.m02 + " " + t.m12 + ")");
            }
        }

        this.setCommands(commands);

    } else {
        this.Super("_updateLocalTransform", [transform, cx, cy, initialShape, fireReshaped, true], arguments);
    }
},

_$close: isc.DrawShape._$close,
_$moveto: isc.DrawShape._$moveto,
_$lineto: isc.DrawShape._$lineto,
_$circleto: isc.DrawShape._$circleto,

_vmlShapeConfigAdapter: new isc.VMLRenderer.shapeConfig(),
_renderVML : function (vmlRenderer, id, conversionContext) {
    var config = this._vmlShapeConfigAdapter;
    isc.VMLRenderer.drawItemToShapeConfig(this, config);
    this._vmlRendererHandle = vmlRenderer.shape(id, config);
},
_getPathVML : function (path) {
    var commands = this.commands,
        numCommands = (commands != null ? commands.length : 0),
        fillSubpathsIndependently = this.fillSubpathsIndependently,
        lastCommandWasMove = true;

    for (var i = 0; i < numCommands; ++i) {
        var command = commands[i],
            type = command.type,
            args = command.args,
            commandIsMove = (type == this._$moveto);
        if (type == this._$close) {
            path.x();
        } else if (commandIsMove) {
            if (fillSubpathsIndependently && !lastCommandWasMove) {
                path.e();
            }
            var point = args;
            path.m(point[0], point[1]);
        } else if (type == this._$lineto) {
            var points = args;
            path.ls(points, 0, points.length);
        } else if (type == this._$circleto) {
            var centerPoint = args[0],
                cx = centerPoint[0],
                cy = centerPoint[1],
                r = args[1],
                startAngle = args[2],
                endAngle = args[3],
                totalAngle = endAngle - startAngle;
            totalAngle = Math.min(totalAngle, 360);
            path.ae(cx, cy, r, r, -startAngle, -totalAngle);
        }
        lastCommandWasMove = commandIsMove;
    }
},

_getPathDataSVG : function (outputTemplate) {
    var template = outputTemplate || [];

    var commands = this.commands;
    if (commands != null && commands.length > 0) {
        var radPerDeg = isc.DrawShape._radPerDeg;
        for (var i = 0; i < commands.length; ++i) {
            var command = commands[i],
                type = command.type,
                args = command.args;
            if (type == "close") {
                template.push(" Z");
            } else if (type == "moveto") {
                var point = args;
                template.push(" M ", point[0], " ", point[1]);
            } else if (type == "lineto") {
                var points = args,
                    point = points[0];
                template.push(" L ", point[0], " ", point[1]);
                for (var n = 1; n < points.length; ++n) {
                    point = points[n];
                    template.push(" ", point[0], " ", point[1]);
                }
            } else if (type == "circleto") {
                var centerPoint = args[0],
                    cx = centerPoint[0],
                    cy = centerPoint[1],
                    radius = args[1],
                    startAngle = args[2],
                    endAngle = args[3],
                    largeArcFlag = Math.abs(endAngle - startAngle) > 180 ? "1" : "0",
                    sweepFlag;

                // Note: SVG's "positive angle direction" is the opposite of ours.
                //
                // Clamp the angle range to strictly less than 360° so that the SVG renderer
                // does not fail to draw a full circle.
                if (startAngle <= endAngle) {
                    sweepFlag = "1";
                    endAngle = Math.min(endAngle, startAngle + 359.9);
                } else {
                    sweepFlag = "0";
                    endAngle = Math.max(startAngle - 359.9, endAngle);
                }

                // First compute the starting point of the arc. We need to "lineto" this point.
                var sx = cx + radius * Math.cos(startAngle * radPerDeg),
                    sy = cy - radius * Math.sin(-startAngle * radPerDeg);
                template.push(" L ", sx, " ", sy);

                // Next compute the ending point of the arc.
                var x = cx + radius * Math.cos(endAngle * radPerDeg),
                    y = cy - radius * Math.sin(-endAngle * radPerDeg);

                template.push(" A ", radius, " ", radius, " ", startAngle <= endAngle ? -startAngle : -endAngle, " ", largeArcFlag, " ", sweepFlag, " ", x, " ", y);
            }
        }
    }

    if (outputTemplate == null) return template.join("");
},

_$attributesSVGTemplate: [
    "d='"                                   // [0]
],
getAttributesSVG : function () {
    var template = this._$attributesSVGTemplate,
        origTemplateLength = template.length;

    this._getPathDataSVG(template);
    template.push("'"); // End the `d' attribute.

    var attrs = template.join("");
    template.setLength(origTemplateLength);
    return attrs;
},

drawBitmapPath : function (context) {
    var commands = this.commands;
    if (commands != null && commands.length > 0) {
        if (this.lineWidth == 0) {
            context.strokeStyle = this.fillColor;
            context.globalAlpha = this.fillOpacity;
        }
        context.beginPath();
        for (var i = 0; i < commands.length; ++i) {
            var command = commands[i],
                type = command.type,
                args = command.args;
            if (type == "close") {
                context.closePath();
            } else if (type == "moveto") {
                var point = args;
                context.moveTo(point[0], point[1]);
            } else if (type == "lineto") {
                var points = args,
                    point = points[0];
                context.lineTo(point[0], point[1]);
                for (var n = 1; n < points.length; ++n) {
                    point = points[n];
                    context.lineTo(point[0], point[1]);
                }
            } else if (type == "circleto") {
                var centerPoint = args[0],
                    cx = centerPoint[0],
                    cy = centerPoint[1],
                    radius = args[1],
                    startAngle = args[2],
                    endAngle = args[3];
                context.arc(cx, cy, radius, startAngle * isc.DrawShape._radPerDeg, endAngle * isc.DrawShape._radPerDeg, endAngle < startAngle);
            }
        }
    }
}

}); // end DrawShape.addProperties

isc.DrawShape.markUnsupportedMethods(null, ["setStartArrow", "setEndArrow"]);








//------------------------------------------------------------------------------------------
//  Helpers
//------------------------------------------------------------------------------------------

isc.GraphMath = {
// convert polar coordinates (angle, distance) to screen coordinates (x, y)
// takes angles in degrees, at least -360 to 360 are supported, 0 degrees is 12 o'clock
zeroPoint : [0,0],
polar2screen : function (angle, radius, basePoint, round) {
    basePoint = basePoint || this.zeroPoint;
    var radians = Math.PI - ((angle+450)%360)/180*Math.PI;
    var point = [
        basePoint[0] + (radius * Math.cos(radians)), // x
        basePoint[1] + (-radius * Math.sin(radians)) // y
    ]
    if (round) {
        point[0] = Math.round(point[0]);
        point[1] = Math.round(point[1]);
    }
    return point;
},

// convert screen coordinates (x, y) to polar coordinates (angle, distance)
// returns an angle in degrees, 0-360, 0 degrees is 12 o'clock
screen2polar : function (x, y) {
    return [
        ((Math.PI - Math.atan2(-y,x))/Math.PI*180 + 270)%360, // angle
        isc.Math._hypot(x, y)]; // radius
},

// find the delta to go from angle1 to angle2, by the shortest path around the circle.  eg
// angle1=359, angle2=1, diff = 2.
// Does not currently support angles outside of 0-360
angleDifference : function (angle1, angle2) {
/*
[
isc.GraphMath.angleDifference(180, 90), // -90
isc.GraphMath.angleDifference(90, 180), // 90
isc.GraphMath.angleDifference(359, 1), // 2
isc.GraphMath.angleDifference(1, 359), // -2
isc.GraphMath.angleDifference(360, 1), // 1
isc.GraphMath.angleDifference(0, 1) // 1
];
*/
    var larger = Math.max(angle1, angle2),
        smaller = Math.min(angle1, angle2),
        clockwiseDiff = larger - smaller,
        counterDiff = smaller+360 - larger;
    if (counterDiff < clockwiseDiff) {
        var direction = larger == angle1 ? 1 : -1;
        return direction * counterDiff;
    } else {
        var direction = larger == angle1 ? -1 : 1;
        return direction * clockwiseDiff;
    }
},

// straight line difference between two points
straightDistance : function (pt1, pt2) {
    return isc.Math._hypot(pt1[0] - pt2[0], pt1[1] - pt2[1]);
}

} // end isc.GraphMath



// center a canvas on a new position
isc.Canvas.addProperties({
    setCenter : function (x, y) {
        this.moveTo(x - this.getVisibleWidth()/2, y - this.getVisibleHeight()/2);
    },
    getCenter : function () {
        return [
            this.getLeft() + this.getVisibleWidth()/2,
            this.getTop() + this.getVisibleHeight()/2
        ]
    }
})






/*
 LinkedList. 
*/
isc.defineClass("LinkedList", "Class").addProperties({
length: 0,
init : function () {
    this.head = null;
},
add: function(data) {
    var node = {next:null,data:data};
    if(!this.head) {
        this.head = node;
    } else {
        var current = this.head;
        while(current.next) {
            current = current.next;
        }
        current.next = node;
    }
    this.length++;
},
get: function(index) {
    var data = null;
    if(index >= 0 && index < this.length) {
        var current = this.head, i = 0;
        while(i++ < index) {
            current = current && current.next;
        }
        data = current.data;
    }
    return data;
},
remove: function(index) {
    var data = null;
    if(index >= 0 && index < this.length) {
        var current = this.head, i = 0;
        if(index === 0) {
            this.head = current.next;
        } else {
            var previous;
            while(i++ < index) {
                previous = current;
                current = current && current.next;
            }
            previous.next = current.next;
        }
        data = current.data;
        this.length--;
    }
    return data;
},
toArray: function() {
    var array = [], current = this.head;
    while(current) {
        array.push(current.data);
        current = current.next;
    }
    return array;
}
});

/*
 A quadtree provides indexing and quick lookup of shapes located in 2D space. A quadtree
 will split a space into 
 4 subspaces when a threshold of max_objects has been reached. Subdivision will stop when
 max_depth has been reached.
 Given a point, a quadtree will return the number of objects found in the quadrant containing
 this point.
*/
isc.defineClass("QuadTree").addClassProperties({

_itemAndAncestors : function (drawItem) {
    var ret = [drawItem];
    var drawPane = drawItem.drawPane;
    for (; drawItem != null; drawItem = drawItem.drawGroup) {
        ret.unshift(drawItem);
    }
    ret.unshift(drawPane);
    return ret;
},

_quadTreeItemCompareFunc : function (item1, item2) {
    item1 = item1.shape;
    item2 = item2.shape;

    // Compute the nearest common ancestor of the two items and then compare the indices. This
    // takes into account zIndexes because when an item's zIndex is changed, it is moved in the
    // parent's drawItems array.
    //
    // A general purpose algorithm for determining the nearest common ancestor is found here:
    // http://stackoverflow.com/questions/3960843/how-to-find-the-nearest-common-ancestors-of-two-or-more-nodes/5350888#5350888

    var item1AndAncestors = isc.QuadTree._itemAndAncestors(item1),
        item2AndAncestors = isc.QuadTree._itemAndAncestors(item2);
    
    for (var i = 0; i < item1AndAncestors.length; ++i) {
        if (item1AndAncestors[i] != item2AndAncestors[i]) {
            var nearestCommonAncestor = item1AndAncestors[i - 1];

            if (nearestCommonAncestor === item1) return -1;
            if (nearestCommonAncestor === item2) return 1;

            var index1 = nearestCommonAncestor.drawItems.indexOf(item1AndAncestors[i]);
            var index2 = nearestCommonAncestor.drawItems.indexOf(item2AndAncestors[i]);
            if (index1 >= 0 && index2 >= 0) {
                return index1 - index2;
            } else {
                
                return item1._addOrder - index2._addOrder;
            }
        }
    }

    
    return 0;
}

});

isc.QuadTree.addProperties({
depth: 0,
maxDepth: 0,
maxChildren: 0,
init : function () {
    this.root = null;
    this.bounds = null;
},
insert : function(item) {
    if(isc.isAn.Array(item)) {
        var len = item.length;
        for(var i = 0; i < len; i++) {
            this.root.insert(item[i]);
        }
    } else {
        this.root.insert(item);
    }
},
remove : function(item) {
    this.root.remove(item);
},
clear : function() {
    this.root.clear();
},
retrieve : function(item) {
    return this.root.retrieve(item).slice();
},
update : function (item, newX, newY, newWidth, newHeight) {
    return this.root.update(item, newX, newY, newWidth, newHeight);
}
});

isc.defineClass("QuadTreeNode", "Class").addClassProperties({
TOP_LEFT:0,
TOP_RIGHT:1,
BOTTOM_LEFT:2,
BOTTOM_RIGHT:3
});

isc.QuadTreeNode.addProperties({
depth: 0,
maxDepth: 4,
maxChildren: 4,
init : function() {
    this.bounds = null;
    this.nodes = [];
    this.children = isc.LinkedList.create();
},
insert : function(item) {
    if(this.nodes.length) {
        var quadrants = this.findQuadrants(item);
        for(var i = 0; i < quadrants.length; ++i) {
            var quadrant = quadrants[i];
            this.nodes[quadrant].insert(item);
        }
        return;
    }
    this.children.add(item);
    var len = this.children.length;
    if(!(this.depth >= this.maxDepth) && len > this.maxChildren) {
        this.subdivide();
        for(var i = 0; i < len; i++) {
            this.insert(this.children.get(i));
        }
        this.children = isc.LinkedList.create();
    }
},
remove : function(item) {
    if(this.nodes.length) {
        var quadrants = this.findQuadrants(item);
        for(var i = 0; i < quadrants.length; ++i) {
            var quadrant = quadrants[i];
            this.nodes[quadrant].remove(item);
        }
    }
    var children = this.children;
    for (var j = 0, numChildren = children.length; j < numChildren; ++j) {
        var data = children.get(j);
        if (data === item) {
            children.remove(j);
            --numChildren;
            --j;
        }
    }
},
retrieve : function(point) {
    if(this.nodes.length) {
        var quadrant = this.findQuadrant(point);
        return this.nodes[quadrant].retrieve(point);
    }
    return this.children.toArray();
},
findQuadrant: function(point) {
    var b = this.bounds,
        left = !(point.x > b.x + b.width / 2),
        top = !(point.y > b.y + b.height / 2);
    //top left
    var quadrant = isc.QuadTreeNode.TOP_LEFT;
    if(left) {
        //left side
        if(!top) {
            //bottom left
            quadrant = isc.QuadTreeNode.BOTTOM_LEFT;
        }
    } else {
        //right side
        if(top) {
            //top right
            quadrant = isc.QuadTreeNode.TOP_RIGHT;
        } else {
            //bottom right
            quadrant = isc.QuadTreeNode.BOTTOM_RIGHT;
        }
    }
    return quadrant;
},
findQuadrants: function(item) {
    var quadrants = [];
    var qmap = {};
    var quadrant = this.findQuadrant({x:item.x,y:item.y});
    quadrants.push(quadrant);
    qmap[quadrant] = true;
    quadrant = this.findQuadrant({x:item.x+item.width,y:item.y+item.height});
    if(!qmap[quadrant]) {
        quadrants.push(quadrant);
        qmap[quadrant] = true;
    }
    quadrant = this.findQuadrant({x:item.x,y:item.y+item.height});
    if(!qmap[quadrant]) {
        quadrants.push(quadrant);
        qmap[quadrant] = true;
    }
    quadrant = this.findQuadrant({x:item.x+item.width,y:item.y});
    if(!qmap[quadrant]) {
        quadrants.push(quadrant);
    }
    return quadrants;
},
subdivide : function() {
    var depth = this.depth + 1;
    var bx = this.bounds.x;
    var by = this.bounds.y;
    //floor the values
    var b_w_h = (this.bounds.width / 2)|0;
    var b_h_h = (this.bounds.height / 2)|0;
    var bx_b_w_h = bx + b_w_h;
    var by_b_h_h = by + b_h_h;
    //top left
    this.nodes[isc.QuadTreeNode.TOP_LEFT] = isc.QuadTreeNode.create({depth:depth});
    this.nodes[isc.QuadTreeNode.TOP_LEFT].bounds = {x:bx, y:by, width:b_w_h, height:b_h_h};
    //top right
    this.nodes[isc.QuadTreeNode.TOP_RIGHT] = isc.QuadTreeNode.create({depth:depth});
    this.nodes[isc.QuadTreeNode.TOP_RIGHT].bounds = {x:bx_b_w_h,y:by,width:b_w_h,height:b_h_h};
    //bottom left
    this.nodes[isc.QuadTreeNode.BOTTOM_LEFT] = isc.QuadTreeNode.create({depth:depth});
    this.nodes[isc.QuadTreeNode.BOTTOM_LEFT].bounds = {x:bx,y:by_b_h_h,width:b_w_h,height:b_h_h};
    //bottom right
    this.nodes[isc.QuadTreeNode.BOTTOM_RIGHT] = isc.QuadTreeNode.create({depth:depth});
    this.nodes[isc.QuadTreeNode.BOTTOM_RIGHT].bounds = {x:bx_b_w_h,y:by_b_h_h,width:b_w_h,height:b_h_h};
},
clear : function() {    
    this.children = isc.LinkedList.create();
    var len = this.nodes.length;
    for(var i = 0; i < len; i++) {
        this.nodes[i].clear();
    }
    this.nodes = [];
    this.depth = 0;
    this.maxDepth = 4;
    this.maxChildren = 4;
},
update : function (item, newX, newY, newWidth, newHeight) {
    this.remove(item);
    item.x = newX;
    item.y = newY;
    item.width = newWidth;
    item.height = newHeight;
    this.insert(item);
}
});
