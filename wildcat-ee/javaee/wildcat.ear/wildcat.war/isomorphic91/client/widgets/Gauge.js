/*
 * Isomorphic SmartClient
 * Version v9.1p_2014-03-26 (2014-03-26)
 * Copyright(c) 1998 and beyond Isomorphic Software, Inc. All rights reserved.
 * "SmartClient" is a trademark of Isomorphic Software, Inc.
 *
 * licensing@smartclient.com
 *
 * http://smartclient.com/license
 */



//> @object GaugeSector
//
// Represents a sector on the gauge.
//
// @treeLocation Client Reference/Drawing/Gauge
// @visibility drawing
//<

//> @attr gaugeSector.fillColor (CSSColor : null : IR)
// @visibility drawing
//<

//> @attr gaugeSector.startAngle (float : 0 : IR)
// @visibility drawing
//<

//> @attr gaugeSector.endAngle (float : 0 : IR)
// @visibility drawing
//<

//> @attr gaugeSector.value (float : 0 : IR)
// @visibility drawing
//<


//> @class Gauge
//
// The Gauge widget class implements a graphical speedometer-style gauge for displaying
// a measurement by means of a needle on a dial. The dial is divided into sectors, each having
// its own color and value.
// <P>
// <b>NOTE:</b> you must load the Drawing +link{group:loadingOptionalModules,Optional Module}
// before you can use Gauge.
//
// @inheritsFrom DrawPane
// @treeLocation Client Reference/Drawing
// @visibility drawing
// @example Gauge
//<

isc.ClassFactory.defineClass("Gauge", "DrawPane");

isc.Gauge.addProperties({
width: 400,
height: 400,

redrawOnResize: true,

//> @attr gauge.dialRadius (float : 150 : IR)
// Radius in pixels of the dial.
//
// @visibility drawing
//<
dialRadius: 150,

//> @attr gauge.knobRadius (float : 6 : IR)
// Radius in pixels of the knob.
//
// @visibility internal
//<

knobRadius: 6,

//> @attr gauge.fontSize (int : 11 : IR)
// Font size of sector labels. Must be at least 3.
//
// @see DrawLabel.fontSize
// @visibility drawing
//<
fontSize: 11,

//> @attr gauge.borderWidth (int : 1 : IR)
// Pixel width for gauge sector borders.
//
// @see DrawItem.lineWidth
// @visibility drawing
//<
borderWidth: 1,

//> @attr gauge.borderColor (CSSColor : "#333333" : IR)
// Color for gauge sector borders.
//
// @see DrawItem.lineColor
// @visibility drawing
//<
borderColor: "#333333",

//> @attr gauge.scaleColor (CSSColor : null : IR)
// Color for gauge scale markings.
// 
// Default value of null means to use the color specified by
// +link{gauge.borderColor}.
//<
scaleColor: null,

//> @attr gauge.needleColor (CSSColor : "#000000" : IR)
// Color for gauge needle.
// 
// Value of null means to use the color specified by
// +link{gauge.borderColor}.  No matter the value, the needle
// border color will be taken from +link{gauge.borderColor}.
//<
needleColor: "#000000",

//> @attr gauge.sectorColors (Array of CSSColor : [ "#AFFFFF", "#008080", "#AAAFFF", "#FF0000", "#FFCC99", "#800080" ] : IR)
// Array of preset fill colors used by the default implementation of +link{Gauge.getDefaultFillColor()}
// to initialize the fill color of new sectors.
//
// <p>The default array of colors is:
// <table border="0" cellpadding="0" cellspacing="2">
//   <tr>
//     <td style="background-color:#AFFFFF;width:90px;height:90px;text-align:center"><a style="vertical-align:middle">#AFFFFF</a></td>
//     <td style="background-color:#008080;color:#FFF;width:90px;height:90px;text-align:center"><a style="vertical-align:middle">#008080</a></td>
//     <td style="background-color:#AAAFFF;width:90px;height:90px;text-align:center"><a style="vertical-align:middle">#AAAFFF</a></td>
//     <td style="background-color:#FF0000;color:#FFF;width:90px;height:90px;text-align:center"><a style="vertical-align:middle">#FF0000</a></td>
//     <td style="background-color:#FFCC99;width:90px;height:90px;text-align:center"><a style="vertical-align:middle">#FFCC99</a></td>
//     <td style="background-color:#800080;color:#FFF;width:90px;height:90px;text-align:center"><a style="vertical-align:middle">#800080</a></td>
//   </tr>
// </table>
// @see DrawItem.fillColor
// @visibility drawing
//<
sectorColors: [ "#AFFFFF", "#008080", "#AAAFFF", "#FF0000", "#FFCC99", "#800080" ],

//> @attr gauge.minValue (float : 0 : IRW)
// The minimum dial value.
//
// @visibility drawing
//<
minValue: 0,

//> @attr gauge.maxValue (float : 100 : IRW)
// The maximum dial value.
//
// @visibility drawing
//<
maxValue: 100,

//> @attr gauge.value (float : 0 : IRW)
// The current value on the dial.
//
// @visibility drawing
//<
value: 0,

//> @attr gauge.numMajorTicks (int : 0 : IRW)
// The number of major tick lines.
//
// @visibility drawing
//<
numMajorTicks: 0,

//> @attr gauge.numMinorTicks (int : 0 : IRW)
// The number of minor tick lines.
//
// @visibility drawing
//<
numMinorTicks: 0,

//> @attr gauge.labelPrefix (string : "" : IRW)
// The label prefix.
//
// @see Gauge.formatLabelContents
// @visibility drawing
//<
labelPrefix: "",

//> @attr gauge.labelSuffix (string : "%" : IRW)
// The label suffix.
//
// @see Gauge.formatLabelContents
// @visibility drawing
//<
labelSuffix: "%",

//> @attr gauge.drawnClockwise (boolean : true : IRW)
// Whether the sectors are drawn clockwise, and increasing the value causes the
// needle to move clockwise.
//
// @visibility drawing
//<
drawnClockwise: true,

//> @attr gauge.sectors (Array of GaugeSector : null : IRW)
// The GaugeSectors contained in this Gauge.  
//
// If this this property is not specified, the gauge will
// be created with a default sector filling the gauge.
// @visibility drawing
//<
sectors: null,

//> @attr gauge.pivotPoint (Point : null : R)
// The pivot point of the needle.
//
// @visibility drawing
//<
pivotPoint: null,

//> @attr gauge.sectorDefaults (GaugeSector Properties : : IRA)
// Default GaugeSector properties for a newly created sector.
//<
sectorDefaults: {
    fillColor: null,
    startAngle: 0,
    endAngle: 0,
    value: 0
}

});

isc.Gauge.addMethods({
initWidget : function () {
    this.Super("initWidget", arguments);

    if (this.fontSize < 3) {
        this.logWarn("Gauge specified with fontSize:" + this.fontSize + ". Setting to 3.");
        this.fontSize = 3;
    }

    if (!(this.sectorColors instanceof Array)) {
        this.logWarn("Gauge specified with non-array sectorColors. Setting to [ \"#AFFFFF\" ].");
        this.sectorColors = [ "#AFFFFF" ];
    }
    if (this.sectorColors.length == 0) {
        this.logWarn("Gauge specified with empty sectorColors array. Setting to [ \"#AFFFFF\" ].");
        this.sectorColors = [ "#AFFFFF" ];
    }

    // If passed a min value that is greater than the max value, swap them.
    if (this.minValue > this.maxValue) {
        this.logWarn("Gauge specified with minValue:" + this.minValue + ", greater than " +
                     "maxValue:" + this.maxValue + ". Swapping.");
        var tmp = this.minValue;
        this.minValue = this.maxValue;
        this.maxValue = tmp;
    }

    // If passed a max value that is not at least 1 greater than the min value, set the max
    // value to 1 + minValue.
    if (!(this.maxValue >= 0.99999 + this.minValue)) {
        this.logWarn("Gauge specified with maxValue:" + this.maxValue + ", not at least 1 " +
                     "greater than minValue:" + this.minValue);
        this.maxValue = 1 + this.minValue;
    }

    // Ensure that the value is between the min value and the max value.
    this.value = Math.min(Math.max(this.value, this.minValue), this.maxValue);

    this.numMajorTicks = Math.floor(this.numMajorTicks);
    this.numMajorTicks = Math.max(0, this.numMajorTicks);
    if (this.numMajorTicks != 0) {
        this.numMajorTicks = Math.max(2, this.numMajorTicks);
    }

    this.numMinorTicks = Math.floor(this.numMinorTicks);
    this.numMinorTicks = Math.max(0, this.numMinorTicks);
    if (this.numMinorTicks != 0) {
        this.numMinorTicks = Math.max(2, this.numMinorTicks);
    }

    this._makePivotPoint(this.getInnerContentWidth(), this.getInnerContentHeight());

    this.setSectors(this.sectors ? this.sectors : [{
        fillColor: this.getDefaultFillColor(0),
        startAngle: 0,
        endAngle: 180,
        value: this.maxValue
    }]);

},

resized : function (deltaX, deltaY) {
    this._makePivotPoint(this.getInnerContentWidth(), this.getInnerContentHeight());
    this._makeItems();
},

_makeItems : function () {
    this.erase();

    if (this._tickLines != null) {
        var existingTickLines = this._tickLines;
        for (var i = 0; i < existingTickLines.length; ++i) {
            existingTickLines[i].destroy();
        }
        delete this._tickLines;
    }
    if (this._labels != null) {
        var existingLabels = this._labels;
        for (var i = 0; i < existingLabels.length; ++i) {
            existingLabels[i].destroy();
        }
        delete this._labels;
    }

    this._makeDrawSectors();
    this._makeNeedle();
    this._positionNeedleTriangle(this.value);
    this._makeLabels();
},

_makePivotPoint : function (width, height) {
    this.pivotPoint = [ width / 2, height * 0.70 ];
},

_makeDrawSectors : function () {
    

    var drawSectors = new Array(this.sectors.length);

    if (this._drawSectors) {
        for (var i = 0; i < this._drawSectors.length; ++i) {
            this._drawSectors[i].destroy();
        }
    }

    var sectors = this.sectors;
    for (var sectorIndex = 0; sectorIndex < sectors.length; ++sectorIndex) {
        var sector = sectors[sectorIndex];

        var startAngle, endAngle;
        if (this.drawnClockwise) {
            startAngle = sector.startAngle - 180;
            endAngle = sector.endAngle - 180;
        } else {
            startAngle = -sector.endAngle;
            endAngle = -sector.startAngle;
        }

        var drawSector;
        drawSectors[sectorIndex] = drawSector = isc.DrawSector.create({
            radius: this.dialRadius,
            centerPoint: this.pivotPoint,
            startAngle: startAngle,
            endAngle: endAngle,
            lineWidth: this.borderWidth,
            lineColor: this.borderColor,
            fillColor: sector.fillColor
        });
        this.addDrawItem(drawSector, true);
    }
    this._drawSectors = drawSectors;

    this._makeTickLines();
},

_makeTickLines : function () {
    

    var numMajorTicks = this.numMajorTicks;
    var numMinorTicks = this.numMinorTicks;
    var drawnClockwise = this.drawnClockwise;
    var pivotPoint = this.pivotPoint;

    var tickLines;
    var i;

    if (!this._tickLines) {
        tickLines = new Array(numMajorTicks + numMinorTicks);
    } else if (this._tickLines.length != numMajorTicks + numMinorTicks) {
        for (i = numMajorTicks + numMinorTicks; i < this._tickLines.length; ++i) {
            this._tickLines[i].destroy();
        }
        tickLines = new Array(numMajorTicks + numMinorTicks);
        for (i = 0; i < tickLines.length; ++i) {
            tickLines[i] = this._tickLines[i];
        }
    } else {
        tickLines = this._tickLines;
    }

    var scaleColor = this.scaleColor ? this.scaleColor : this.borderColor;

    for (i = 0; i < numMajorTicks; ++i) {
        var angleRad = Math.PI - Math.PI * i / (numMajorTicks - 1);
        if (!drawnClockwise) {
            angleRad = Math.PI - angleRad;
        }

        // See _positionNeedleTriangle() for a short explanation of the math. Essentially
        // polar coordinates are being converted to Cartesian.
        var x = (this.dialRadius + 3) * Math.cos(angleRad);
        var y = (this.dialRadius + 3) * Math.sin(angleRad);
        var p = [ pivotPoint[0] + x, pivotPoint[1] - y ];

        var x2 = (this.dialRadius - 10) * Math.cos(angleRad);
        var y2 = (this.dialRadius - 10) * Math.sin(angleRad);
        var p2 = [ pivotPoint[0] + x2, pivotPoint[1] - y2 ];

        if (!tickLines[i]) {
            tickLines[i] = isc.DrawLine.create({
                lineColor: scaleColor,
                startPoint: p2,
                endPoint: p,
                lineWidth: 2
            });
        } else {
            tickLines[i].setLineColor(scaleColor);
            tickLines[i].setStartPoint(p[0], p[1]);
            tickLines[i].setEndPoint(p2[0], p2[1]);
        }
    }

    for (var j = 0; j < numMinorTicks; ++j, ++i) {
        var angleRad = Math.PI - Math.PI * j / (numMinorTicks - 1);
        if (!drawnClockwise) {
            angleRad = Math.PI - angleRad;
        }

        var x = this.dialRadius * Math.cos(angleRad);
        var y = this.dialRadius * Math.sin(angleRad);
        var p = [ pivotPoint[0] + x, pivotPoint[1] - y ];

        var x2 = (this.dialRadius - 5) * Math.cos(angleRad);
        var y2 = (this.dialRadius - 5) * Math.sin(angleRad);
        var p2 = [ pivotPoint[0] + x2, pivotPoint[1] - y2 ];

        if (!tickLines[i]) {
            tickLines[i] = isc.DrawLine.create({
                lineColor: scaleColor,
                startPoint: p2,
                endPoint: p,
                lineWidth: 1
            });
        } else {
            tickLines[i].setLineColor(scaleColor);
            tickLines[i].setStartPoint(p[0], p[1]);
            tickLines[i].setEndPoint(p2[0], p2[1]);
        }
    }

    for (i = 0; i < tickLines.length; ++i) {
        this.addDrawItem(tickLines[i], true);
    }
    this._tickLines = tickLines;
},

_makeNeedle : function () {
    

    var triangleColor = this.needleColor ? this.needleColor : this.borderColor;

    if (!this._needleTriangle) {
        this._needleTriangle = isc.DrawTriangle.create({
            drawPane: this,
            autoDraw: true,
            lineColor: this.borderColor,
            lineWidth: this.borderWidth,
            fillColor: triangleColor
        });
    } else {
        var triangle = this._needleTriangle;
        triangle.setLineColor(this.borderColor);
        triangle.setLineWidth(this.borderWidth);
        triangle.setFillColor(triangleColor);
        this.addDrawItem(triangle, true);
    }

    this._makeKnob();
},

_makeKnob : function () {
    

    var knobColor = this.needleColor ? this.needleColor : this.borderColor;

    if (!this._knob) {
        this._knob = isc.DrawOval.create({
            drawPane: this,
            autoDraw: true,
            centerPoint: this.pivotPoint,
            radius: this.knobRadius,
            lineColor: this.borderColor,
            lineWidth: this.borderWidth,
            fillColor: knobColor
        });
    } else {
        var knob = this._knob;
        knob.setCenterPoint(this.pivotPoint[0], this.pivotPoint[1]);
        knob.setRadius(this.knobRadius);
        knob.setLineColor(this.borderColor);
        knob.setLineWidth(this.borderWidth);
        knob.setFillColor(knobColor);
        this.addDrawItem(knob, true);
    }
},

_makeLabels : function () {
    

    var sectors = this.sectors;
    var labels = new Array(1 + sectors.length);

    if (this._labels) {
        for (var i = 0; i < this._labels.length; ++i) {
            this._labels[i].destroy();
        }
    }

    var minimumLabel;
    labels[0] = minimumLabel = this._makePositionedLabel(this.formatLabelContents(this.minValue),
                                                         this.minValue);
    this.addDrawItem(minimumLabel, true);

    for (var sectorIndex = 0; sectorIndex < sectors.length; ++sectorIndex) {
        var sector = sectors[sectorIndex];
        var label = this._makePositionedLabel(this.formatLabelContents(sector.value),
                                              sector.value);
        labels[1 + sectorIndex] = label;
        this.addDrawItem(label, true);
    }

    this._labels = labels;
},

//> @method gauge.setMinValue()
// Sets the minimum dial value, rescaling all sectors and the dial value.
//
// @param minValue (float) the new minimum dial value. Must be at least 1 less than the
// maximum dial value. If <code>minValue</code> is not at least 1 less than the maximum value,
// then it is set to <code>maxValue - 1</code>.
// @visibility drawing
//<
setMinValue : function (minValue) {
    minValue = Math.min(0 + minValue, this.maxValue - 1);
    this._rescaleSectors(minValue, this.maxValue);
},

//> @method gauge.setMaxValue()
// Sets the maximum dial value, rescaling all sectors and the dial value.
//
// @param maxValue (float) the new maximum dial value. Must be at least 1 greater than the
// minimum dial value. If <code>maxValue</code> is not at least 1 greater than the minimum
// value, then it is set to <code>1 + minValue</code>.
// @visibility drawing
//<
setMaxValue : function (maxValue) {
    maxValue = Math.max(0 + maxValue, 1 + this.minValue);
    this._rescaleSectors(this.minValue, maxValue);
},

_rescaleSectors : function (newMinValue, newMaxValue) {
    var maxValue = this.maxValue;
    var minValue = this.minValue;
    var sectors = this.sectors;

    var scale = maxValue - minValue;
    var newScale = newMaxValue - newMinValue;

    var prevEndAngle = 0;
    for (var sectorIndex = 0; sectorIndex < sectors.length; ++sectorIndex) {
        var sector = sectors[sectorIndex];
        var newValue = newScale * (sector.value - minValue) / scale + newMinValue;
        sector.value = newValue;
        sector.startAngle = prevEndAngle;
        sector.endAngle = prevEndAngle = this._getSectorEndAngle(newValue, newMinValue, newScale);
    }

    this.value = newScale * (this.value - minValue) / scale + newMinValue;

    this.minValue = minValue = newMinValue;
    this.maxValue = maxValue = newMaxValue;

    this._makeItems();
},

//> @method gauge.setValue()
// Sets the value on the dial that the needle is displaying.
//
// @param value (float) the new dial value. Must be between +link{gauge.minValue, minValue} and
// +link{gauge.maxValue, maxValue}.
// @visibility drawing
//<
setValue : function (value) {
    // Ensure that `value` is between the min value and the max value.
    value = Math.min(Math.max(0 + value, this.minValue), this.maxValue);
    this._positionNeedleTriangle(value);
    this.value = value;
},

//> @method gauge.setNumMajorTicks()
// Sets the number of major tick lines.
//
// <p><b>NOTE:</b> To divide the dial into <i>n</i> regions, you will need <i>n</i> + 1 ticks.
// For example, if the minimum value is 0 and the maximum value is 100, then to place major
// tick lines at 0, 10, 20, 30, ..., 90, 100, you need 11 (10 + 1) major ticks.
//
// @param numMajorTicks (int) the number of major tick lines to draw. Must be either 0 or an
// integer greater than or equal to 2.
// @visibility drawing
//<
setNumMajorTicks : function (numMajorTicks) {
    numMajorTicks = Math.floor(numMajorTicks);
    if (this.numMajorTicks != numMajorTicks) {
        numMajorTicks = Math.max(0, numMajorTicks);
        if (numMajorTicks != 0) {
            numMajorTicks = Math.max(2, numMajorTicks);
        }
        this.numMajorTicks = numMajorTicks;
        this._makeItems();
    }
},

//> @method gauge.setNumMinorTicks()
// Sets the number of minor tick lines.
//
// <p><b>NOTE:</b> To divide the dial into <i>n</i> regions, you will need <i>n</i> + 1 ticks.
// For example, if the minimum value is 0 and the maximum value is 100, then to place minor
// tick lines at 0, 1, 2, 3, 4, 5, ..., 99, 100, you need 101 (100 + 1) minor ticks.
//
// @param numMinorTicks (int) the number of minor tick lines to draw. Must be either 0 or an
// integer greater than or equal to 2.
// @visibility drawing
//<
setNumMinorTicks : function (numMinorTicks) {
    numMinorTicks = Math.floor(numMinorTicks);
    if (this.numMinorTicks != numMinorTicks) {
        numMinorTicks = Math.max(0, numMinorTicks);
        if (numMinorTicks != 0) {
            numMinorTicks = Math.max(2, numMinorTicks);
        }
        this.numMinorTicks = numMinorTicks;
        this._makeItems();
    }
},

//> @method gauge.setLabelPrefix()
// Sets the +link{Gauge.labelPrefix,labelPrefix} property and re-creates all sector labels.
//
// @param labelPrefix (String) the new label prefix.
// @visibility drawing
//<
setLabelPrefix : function (labelPrefix) {
    if (labelPrefix == null) labelPrefix = "";
    if (this.labelPrefix != labelPrefix) {
        this.labelPrefix = labelPrefix;
        this.reformatLabelContents();
    }
},

//> @method gauge.setLabelSuffix()
// Sets the +link{Gauge.labelSuffix,labelSuffix} property and re-creates all sector labels.
//
// @param labelSuffix (String) the new label suffix.
// @visibility drawing
//<
setLabelSuffix : function (labelSuffix) {
    if (labelSuffix == null) labelSuffix = "";
    if (this.labelSuffix != labelSuffix) {
        this.labelSuffix = labelSuffix;
        this.reformatLabelContents();
    }
},

//> @method gauge.setDrawnClockwise()
// Sets the +link{gauge.drawnClockwise, drawnClockwise} property and redraws the gauge.
//
// @param drawnClockwise (boolean) whether the sectors are drawn clockwise.
// @visibility drawing
//<
setDrawnClockwise : function (drawnClockwise) {
    drawnClockwise = !!drawnClockwise;
    if (this.drawnClockwise != drawnClockwise) {
        this.drawnClockwise = drawnClockwise;
        this._makeItems();
    }
},

//> @method gauge.formatLabelContents() (A)
// Formats a value as a string to be used as the contents of a +link{DrawLabel}. The default
// implementation prepends +link{gauge.labelPrefix, labelPrefix} and appends
// +link{gauge.labelSuffix, labelSuffix} to <code>value</code>.
// <p>
// <b>NOTE:</b> If a subclass overrides this, then whenever it changes the way that values are
// formatted, it must call +link{Gauge.reformatLabelContents()}.
//
// @param value (float) the value to format.
// @return (string) label contents.
// @visibility drawing
//<
formatLabelContents : function (value) {
    return this.labelPrefix + (0 + value) + this.labelSuffix;
},

//> @method gauge.reformatLabelContents() (A)
// Resets the contents of all labels. This involves calling +link{Gauge.formatLabelContents()}
// to get the label contents for each corresponding value and repositioning the label.
//
// @visibility drawing
//<
reformatLabelContents : function () {
    this._makeItems();
},

//> @method gauge.getNumSectors()
// Gets the number of sectors.
//
// @return (int) the number of sectors on this gauge.
// @visibility drawing
//<
getNumSectors : function () {
    return this.sectors.length;
},

//> @method gauge.getSectorValue()
// Gets the value of the sector at <code>sectorIndex</code>.
//
// @param sectorIndex (int) index of the target sector.
// @return (float) the value of the sector at <code>sectorIndex</code>.
// @visibility drawing
//<
getSectorValue : function (sectorIndex) {
    return this.sectors[sectorIndex].value;
},

//> @method gauge.getDefaultFillColor() (A)
// Gets the default fill color for the sector at index <code>sectorIndex</code>. The default
// implementation cycles through +link{gauge.sectorColors, sectorColors}
// using modular arithmetic.
//
// @param sectorIndex (int) index of the target sector.
// @return (CSSColor) a fill color.
// @visibility drawing
//<
getDefaultFillColor : function (sectorIndex) {
    var numDefaultColors = this.sectorColors.length;
    return this.sectorColors[sectorIndex % numDefaultColors];
},

//> @method gauge.getSectorFillColor()
// Gets the fill color of the sector at index <code>sectorIndex</code>.
//
// @param sectorIndex (int) index of the target sector.
// @return (CSSColor) the fill color of the sector at <code>sectorIndex</code>.
// @see DrawItem.fillColor
// @visibility drawing
//<
getSectorFillColor : function (sectorIndex) {
    return this.sectors[sectorIndex].fillColor;
},

//> @method gauge.setSectorFillColor()
// Sets the fill color of the sector at <code>sectorIndex</code>.
//
// @param sectorIndex (int) index of the target sector.
// @param fillColor (CSSColor) the new fill color.
// @see DrawItem.setFillColor
// @visibility drawing
//<
setSectorFillColor : function (sectorIndex, fillColor) {
    var sectors = this.sectors;
    var drawSectors = this._drawSectors;

    sectors[sectorIndex].fillColor = fillColor;

    if (drawSectors && drawSectors.length == sectors.length) {
        drawSectors[sectorIndex].setFillColor(fillColor);
    } else {
        this._makeItems();
    }
},

// Shift fill colors left by one - when new sector has no specified color
_shiftSectorFillColor : function (sector, index) {
    if (index < this.sectors.length) { 
        sector.fillColor = this.sectors[index].fillColor;
    } else {
        sector.fillColor = this.getDefaultFillColor(index);
    }
},

//> @method gauge.getSectorLabelContents()
// Gets the label contents of the label for the sector at sectorIndex.
//
// @param sectorIndex (int) index of the target sector.
// @return (String) the label contents of the sector's label.
// @visibility drawing
//<
getSectorLabelContents : function (sectorIndex) {
    return this.formatLabelContents(this.sectors[sectorIndex].value);
},

//> @method gauge.setBorderColor()
// Sets the border color for this gauge.
//
// @param color (CSSColor) color of the gauge border
//< 
setBorderColor : function (color) {
    this.borderColor = color;
    this.redraw();
},

//> @method gauge.setBorderWidth()
// Sets the border width for this gauge.
//
// @param color (int) width of the gauge border
//< 
setBorderWidth : function (width) {
    this.borderWidth = width;
    this.redraw();
},

//> @method gauge.setSectors()
// Sets the sectors for this gauge.
// @param (Array of GaugeSector) the sectors to show on the gauge.
// @visibility drawing
//<
setSectors : function (sectors) {
    this.sectors = [];
    for (var i = 0; i < sectors.length; i++) {
        this.addSector(sectors[i], true);
    }

    this._makeItems();
},

//> @method gauge.addSector()
// Adds a new sector.
//
// @param newSector (GaugeSector | double) the new GaugeSector or the new sector's value. This
// is formatted with +link{Gauge.formatLabelContents()} to get its label.
// @return (int) the index of the newly-added sector.
// @visibility drawing
//<
addSector : function (newSector, dontMakeItems) {
    if (!isc.isAn.Object(newSector)) {
        newSector = { value: newSector };
    }

    var value = newSector.value;

    // Apply sector defaults.
    newSector = isc.addProperties({}, this.sectorDefaults, newSector);

    // Ensure that `value` is between the min value and the max value.
    value = Math.min(Math.max(0 + value, this.minValue), this.maxValue);

    var sectors = this.sectors;
    var maxValue = this.maxValue;
    var minValue = this.minValue;

    var newSectorIndex = 0;

    // Determine where the new GaugeSector object should go in `this.sectors`.
    for (; newSectorIndex < sectors.length &&
           sectors[newSectorIndex].value < value; ++newSectorIndex)
    {
        /*empty*/
    }

    var tmp = new Array(sectors.length + 1);
    for (var sectorIndex = 0; sectorIndex < newSectorIndex; ++sectorIndex) {
        tmp[sectorIndex] = sectors[sectorIndex];
    }

    var prevEndAngle,
        scale = maxValue - minValue,
        shiftSectorColors = !newSector.fillColor;

    newSector.startAngle = this._getSectorStartAngle(newSectorIndex, minValue, scale);
    newSector.endAngle = prevEndAngle = this._getSectorEndAngle(value, minValue, scale);
    tmp[newSectorIndex] = newSector;

    if (shiftSectorColors) this._shiftSectorFillColor(newSector, newSectorIndex);

    if (newSectorIndex < sectors.length) {
        sectors[newSectorIndex].startAngle = prevEndAngle;
    }

    for (var sectorIndex = newSectorIndex; sectorIndex < sectors.length; ++sectorIndex) {
        var sector = sectors[sectorIndex];

        // Continue shifting sector fill colors left by one.
        if (shiftSectorColors) this._shiftSectorFillColor(sector, sectorIndex+1);

        tmp[sectorIndex + 1] = sector;
    }

    this.sectors = sectors = tmp;

    if (!dontMakeItems) {
        this._makeItems();
    }

    return newSectorIndex;
},

//> @method gauge.removeSector()
// Removes the sector at sectorIndex.
// <p>
// <b>NOTE:</b> There must always be one sector and it is not possible to remove the sole remaining
// sector. Calling this method to attempt to remove the sole remaining sector is a no-op.
//
// @param sectorIndex (int) the index of the sector to remove.
// @visibility drawing
//<
removeSector : function (sectorIndex) {
    var sectors = this.sectors;

    if (sectorIndex == 0 && sectors.length == 1) {
        return;
    }

    var tmp = new Array(sectors.length - 1);

    for (var i = 0; i < sectorIndex; ++i) {
        tmp[i] = sectors[i];
    }

    var prevEndAngle = 0;
    if (sectorIndex > 0) {
        prevEndAngle = sectors[sectorIndex - 1].endAngle;
    }

    if (sectorIndex < sectors.length - 1) {
        sectors[sectorIndex + 1].startAngle = prevEndAngle;
    }

    for (var i = sectorIndex + 1; i < sectors.length; ++i) {
        tmp[i - 1] = sectors[i];
    }

    if (sectorIndex == sectors.length - 1) {
        tmp[sectorIndex - 1].endAngle = 180;
        tmp[sectorIndex - 1].value = this.maxValue;
    }

    this.sectors = sectors = tmp;

    this._makeItems();
},

_getSectorStartAngle : function (sectorIndex, minValue, scale) {
    if (sectorIndex == 0) {
        return 0;
    } else {
        var prevSectorValue = this.sectors[sectorIndex - 1].value;
        return 180 * (prevSectorValue - minValue) / scale;
    }
},

_getSectorEndAngle : function (value, minValue, scale) {
    return 180 * (value - minValue) / scale;
},

_positionNeedleTriangle : function (value) {
    

    var pivotPoint = this.pivotPoint;

    var scale = this.maxValue - this.minValue;
    var angleRad = Math.PI - Math.PI * (value - this.minValue) / scale;
    if (!this.drawnClockwise) {
        angleRad = Math.PI - angleRad;
    }

    // The outermost point of the needle triangle (in polar coordinates with the origin at
    // `pivotPoint`) has r = this.dialRadius - 15.

    // Compute the Cartesian coordinates of the outermost point, with the origin being
    // `pivotPoint`.
    var x = (this.dialRadius - 15) * Math.cos(angleRad);
    var y = (this.dialRadius - 15) * Math.sin(angleRad);
    // Translate to Drawing coordinates.
    var p = [ pivotPoint[0] + x, pivotPoint[1] - y ];

    // `p1` and `p2` are points on the center knob. The line segment from `p1` to `p2` is
    // perpendicular to the line segment (0, 0) to `p`.
    // The math is the same. It's just that the angles are different (+/- Pi/2, or 90 deg) and
    // r is this.knobRadius.
    var angleRad1 = angleRad - Math.PI / 2.0;
    var x1 = this.knobRadius * Math.cos(angleRad1);
    var y1 = this.knobRadius * Math.sin(angleRad1);
    var p1 = [ pivotPoint[0] + x1, pivotPoint[1] - y1 ];

    var angleRad2 = angleRad + Math.PI / 2.0;
    var x2 = this.knobRadius * Math.cos(angleRad2);
    var y2 = this.knobRadius * Math.sin(angleRad2);
    var p2 = [ pivotPoint[0] + x2, pivotPoint[1] - y2 ];

    this._needleTriangle.setPoints([ p, p1, p2 ]);
},

_makePositionedLabel : function (contents, value) {
    

    var pivotPoint = this.pivotPoint;

    var scale = this.maxValue - this.minValue;
    var angleRad = Math.PI - Math.PI * (value - this.minValue) / scale;
    if (!this.drawnClockwise) {
        angleRad = Math.PI - angleRad;
    }

    var labelProps = {
        fontSize: this.fontSize,
        contents: contents,
        left: 0,
        top: 0
    };
    var labelDims = this.measureLabel(contents, labelProps),
        labelWidth = labelDims.width,
        labelHeight = labelDims.height;

    var x = (this.dialRadius + 5) * Math.cos(angleRad);
    var y = (this.dialRadius + 5) * Math.sin(angleRad);

    var left;
    if (angleRad < 9.0 * Math.PI / 20.0 - 0.001) {
        // (x, y) are the Cartesian coordinates of the bottom left point of the label, where
        // the origin is `pivotPoint`.
        left = Math.round(pivotPoint[0] + x + 2);
    } else if (angleRad <= 11.0 * Math.PI / 20.0 + 0.001) {
        // (x, y) are the Cartesian coordinates of the bottom middle point of the label, where
        // the origin is `pivotPoint`.
        left = Math.round(pivotPoint[0] + x - labelWidth / 2);
    } else {
        // (x, y) are the Cartesian coordinates of the bottom right point of the label, where
        // the origin is `pivotPoint`.
        left = Math.round(pivotPoint[0] + x - labelWidth - 2);
    }

    var top;
    if (Math.PI / 4.0 <= angleRad &&
        angleRad <= 3.0 * Math.PI / 4.0) {
        top = Math.round(pivotPoint[1] - y - labelHeight * (1.0 - Math.abs(Math.PI / 2.0 - angleRad) * 0.5));
    } else {
        top = Math.round(pivotPoint[1] - y - labelHeight / 2);
    }

    labelProps.left = left;
    labelProps.top = top;
    return isc.DrawLabel.create(labelProps);
}
});
