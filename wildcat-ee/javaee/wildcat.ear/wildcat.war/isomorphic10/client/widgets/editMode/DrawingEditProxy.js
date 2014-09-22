/*
 * Isomorphic SmartClient
 * Version v10.0p_2014-09-10 (2014-09-10)
 * Copyright(c) 1998 and beyond Isomorphic Software, Inc. All rights reserved.
 * "SmartClient" is a trademark of Isomorphic Software, Inc.
 *
 * licensing@smartclient.com
 *
 * http://smartclient.com/license
 */



//> @class DrawPaneEditProxy
// +link{EditProxy} that handles +link{DrawPane,DrawPanes} when editMode is enabled.
//
// @group devTools
// @treeLocation Client Reference/Tools/EditProxy
// @visibility external
//<
isc.defineClass("DrawPaneEditProxy", "CanvasEditProxy").addMethods({
    setEditMode : function (editingOn) {
        this.Super("setEditMode", arguments);

        // Set editMode on all children
        var liveObjects = this.creator.editContext.getEditNodeArray().getProperty("liveObject");
        liveObjects.map("setEditMode", editingOn, this.creator.editContext);

        // Remove any selections/outlines
        if (!editingOn) this.creator.editContext.deselectAllComponents();
    },

    destroy : function () {
        if (this._keyPressEventID) {
            isc.Page.clearEvent("keyPress", this._keyPressEventID);
            delete this._keyPressEventID;
        }
        this.Super("destroy", arguments);
    },

    drop : function () {
        var liveObject = this.creator,
            source = liveObject.ns.EH.getDragTarget()
        ;

        // If the source isn't a Palette then perform the standard drop interaction.
        if (!isc.isA.Palette(source)) {
            return liveObject.drop.apply(arguments);
        }

        var data = source.transferDragData(),
            paletteNode = (isc.isAn.Array(data) ? data[0] : data)
        ;
        if (!paletteNode) return false;

        var editNode = liveObject.editContext.makeEditNode(paletteNode);
        if (!editNode) return false;

        var editProxy = this;
        liveObject.editContext.requestLiveObject(editNode, function (editNode) {
            if (editNode) {
                // Add the new component at the current mouse position.
                var node;
                if (isc.isA.DrawPane(liveObject)) {
                    node = liveObject.editContext.addNode(editNode, liveObject.editNode);
                } else {
                    // Wrap the DrawItem in a DrawPane
                    var dropType;

                    if (!source.isA("Palette")) {
                        if (source.isA("FormItemProxyCanvas")) {
                            source = source.formItem;
                        }
                        dropType = source._constructor || source.Class;
                    } else {
                        paletteNode.dropped = true;
                        dropType = paletteNode.type || paletteNode.className;
                    }

                    // Establish the actual drop node (this may not be the canvas accepting the drop - for a
                    // composite component like TabSet, the dropped-on canvas will be the tabBar or 
                    // paneContainer)
                    var dropTargetNode = this.findEditNode(dropType);
                    if (dropTargetNode) {
                        dropTargetNode = dropTargetNode.editNode;
                    }

                    node = liveObject.editContext.addWithWrapper(editNode, dropTargetNode, true);
                }
                node.liveObject.moveTo(liveObject.getOffsetX(), liveObject.getOffsetY());
                
                if (editProxy.canSelectChildren && node.liveObject.editProxy.canSelect != false) {
                    liveObject.editContext.selectSingleComponent(node.liveObject);
                }
            }
        }, source);

        return isc.EventHandler.STOP_BUBBLING;
    },
    
    // Title editing for a single selected item is supported by two means:
    // - When title is null and component is selected, just start typing
    // - Double-clicking
    
    selectedEditNodesUpdated : function (editNode, editNodesList) {
        // Handle one selection replace with another
        if (editNodesList != null && editNodesList.length == 1) {
                this.enableKeyMovement(true);

                // disable/re-enable key movement during inline edits
                var component = editNode.liveObject;
                if (this._observedComponent != null && component != this._observedComponent) {
                    this.ignore(this._observedComponent.editProxy, "startInlineEditing");
                    this.ignore(this._observedComponent.editProxy, "inlineEditingComplete");
                }
                this.observe(component.editProxy, "startInlineEditing", "observer.editingStarted()");
                this.observe(component.editProxy, "inlineEditingComplete", "observer.editingComplete()");
                this._observedComponent = component;
        } else if (editNodesList == null || editNodesList.length != 1) {
            if (this._observedComponent) {
                this.ignore(this._observedComponent.editProxy, "startInlineEditing");
                this.ignore(this._observedComponent.editProxy, "inlineEditingComplete");
                this._observedComponent = null;
            }

            this.enableKeyMovement(false);
        }
    },
    
    enableKeyMovement : function (enable) {
        if (enable) {
            if (!this._keyPressEventID) {
                this._keyPressEventID = isc.Page.setEvent("keyPress", this);
            }
        } else {
            if (this._keyPressEventID) {
                isc.Page.clearEvent("keyPress", this._keyPressEventID);
                delete this._keyPressEventID;
            }
        }
    },

    // Keypress positioning of mask must be disabled while in inline edit
    // mode or the key is processed outside of the entry
    editingStarted : function () {
        this.enableKeyMovement(false);
    },
    
    editingComplete : function () {
        this.enableKeyMovement(true);
    },

    pageKeyPress : function (target, eventInfo) {
        if (!this.creator.isVisible()) return;

        var key = isc.EH.getKeyEventCharacter(),
            liveObject = this.creator,
            selection = liveObject.editContext.getSelectedComponents()
        ;
        if (selection.length != 1) return;

        // If root pane (or child) does not have focus, ignore keyPress 
        var rootPane = liveObject.editContext.getRootEditNode().liveObject;
        if (!rootPane.containsFocus()) return;

        if (isc.isA.AlphaNumericChar(key)) {
            selection[0].editProxy.startInlineEditing(key);
            return false;
        } else {
            var target = selection[0],
                shiftPressed = isc.EH.shiftKeyDown(),
                vGap = (shiftPressed ? 1 : target.drawPane.snapVGap),
                hGap = (shiftPressed ? 1 : target.drawPane.snapHGap),
                result = false
            ;

            switch (isc.EH.getKey()) {
            case "Arrow_Up":
                target.moveBy(0, vGap * -1);
                break;
            case "Arrow_Down":
                target.moveBy(0, vGap);
                break;
            case "Arrow_Left":
                target.moveBy(hGap * -1, 0);
                break;
            case "Arrow_Right":
                target.moveBy(hGap, 0);
                break;
            default:
                result = null;
                break;
            }

            return result;
        }
    }
});

//> @class DrawItemEditProxy
// +link{EditProxy} that handles +link{DrawItem,DrawItems} except for
// +link{DrawLabel,DrawLabels} when editMode is enabled.
//
// @group devTools
// @treeLocation Client Reference/Tools/EditProxy
// @visibility external
//<
isc.defineClass("DrawItemEditProxy", "EditProxy").addMethods({

    getOverrideProperties : function () {
        var properties = this.Super("getOverrideProperties", arguments);

        isc.addProperties(properties, {
            canDrag: true,
            cursor: "move"
        });

        return properties;
    },

    // DrawItems do not support an EditMask - ignore any requests 
    showEditMask : function () {
        this.logWarn("showEditMask called on DrawItem EditProxy - ignored");
    },
    hideEditMask : function () {
        this.logWarn("hideEditMask called on DrawItem EditProxy - ignored");
    },

    //> @method DrawItemEditProxy.showSelectedAppearance
    //<
    showSelectedAppearance : function (show) {
        var liveObject = this.creator;
        if (show) {
            var knobs = liveObject._editModeKnobs;
            // Show all knobs except "move"
            if (!knobs && liveObject.getSupportedKnobs) {
                knobs = liveObject.getSupportedKnobs();
                if (knobs) knobs.remove("move");
                liveObject._editModeKnobs = knobs;
            }
            if (liveObject.showKnobs) liveObject.showKnobs(knobs);

            // Bring component to front
            if (liveObject.bringToFront) liveObject.bringToFront();
        } else {
            liveObject.hideAllKnobs();
        }
    },
        
    click : function () {
        var liveObject = this.creator;
        if (liveObject.drawPane.editProxy.canSelectChildren && this.canSelect != false) {
        	liveObject.editContext.selectSingleComponent(liveObject);
	    }
        return isc.EH.STOP_BUBBLING;
    },

    // DRAG EVENTS - Defer to DrawItem instead of EditProxy
    dragStart : function (event, info) {
        var liveObject = this.creator;
        if (!liveObject.drawPane.editProxy.canSelectChildren || this.canSelect == false) {
            return false;
        }

        // Bring component to front
        if (liveObject.bringToFront) liveObject.bringToFront();

        liveObject.dragStart(event, info);
    },
    dragMove : function (event, info, bubbledFromDrawItem) {
        this.creator.dragMove(event, info, bubbledFromDrawItem);
    },
    dragStop : function (event, info) {
        var liveObject = this.creator;
        liveObject.dragStop(event, info);
        // Auto-select component after drag
        if (liveObject.drawPane.editProxy.canSelectChildren &&
            liveObject.editProxy.canSelect != false &&
            !liveObject.editContext.isComponentSelected(liveObject))
        {
            liveObject.editContext.selectSingleComponent(liveObject);
        }
    },
    
    // Component editor handling
    // ---------------------------------------------------------------------------------------

    supportsInlineEdit: true,

    //> @method drawItemEditProxy.getInlineEditText()
    // Returns the text based on the current component state to be edited inline.
    // Called by the +link{editProxy.inlineEditForm} to obtain the starting edit value.
    // <p>
    // Returns the component's title.
    //
    // @visibility external
    //<
    getInlineEditText : function () {
        return this.creator.title;
    },

    //> @method drawItemEditProxy.setInlineEditText()
    // Save the new value into the component's state. Called by the
    // +link{editProxy.inlineEditForm} to commit the change.
    // <p>
    // Updates the component's title.
    //
    // @param newValue (String) the new component title
    //
    // @visibility external
    //<
    setInlineEditText : function (newValue) {
        this.creator.editContext.setNodeProperties(this.creator.editNode, { title: newValue });
    }
});

//> @class DrawLabelEditProxy
// +link{EditProxy} that handles +link{DrawLabel,DrawLabels} when editMode is enabled.
//
// @group devTools
// @treeLocation Client Reference/Tools/EditProxy
// @visibility external
//<
isc.defineClass("DrawLabelEditProxy", "DrawItemEditProxy").addMethods({

    // Component editor handling
    // ---------------------------------------------------------------------------------------

    inlineEditMultiline: true,

    //> @method drawLabelEditProxy.getInlineEditText()
    // Returns the text based on the current component state to be edited inline.
    // Called by the +link{editProxy.inlineEditForm} to obtain the starting edit value.
    // <p>
    // Returns the component's <code>contents</code>.
    //
    // @visibility external
    //<
    getInlineEditText : function () {
        return this.creator.contents;
    },

    //> @method drawLabelEditProxy.setInlineEditText()
    // Save the new value into the component's state. Called by the
    // +link{editProxy.inlineEditForm} to commit the change.
    // <p>
    // Updates the component's <code>contents</code>.
    //
    // @param newValue (String) the new component contents
    //
    // @visibility external
    //<
    setInlineEditText : function (newValue) {
        this.creator.editContext.setNodeProperties(this.creator.editNode, { contents: newValue });
    }
});


//> @class FacetChartEditProxy
// +link{EditProxy} that handles +link{FacetChart,FacetCharts} when editMode is enabled.
//
// @group devTools
// @treeLocation Client Reference/Tools/EditProxy
// @visibility external
//<
isc.defineClass("FacetChartEditProxy", "DrawPaneEditProxy").addMethods({

    // Component editor handling
    // ---------------------------------------------------------------------------------------

    inlineEditMultiline: true,

    //> @attr facetChartEditProxy.dataSeparatorChar (String : "," : IR)
    // If +link{inlineEditEvent,inline editing} for this chart edits the
    // +link{facetChart.data}, character that should be used as a separator between
    // values, or between pairs of label vs values.
    // <p>
    // The +link{dataEscapeChar,dataEscapeChar} can be used to enter the separator
    // char as part of a field name or value.
    //
    // @visibility external
    //<
    dataSeparatorChar: ",",

    //> @attr facetChartEditProxy.dataDisplaySeparatorChar (String : ":" : IR)
    // If +link{inlineEditEvent,inline editing} for this chart edits the
    // +link{facetChart.data}, character that should be used as a separator for
    // entering label vs value entries.
    // <p>
    // With the default of ":", the following input defines four values with titles:
    // <pre>
    //      North:10, South:20, East:30, West:40
    // </pre>
    // <p>
    // The +link{dataEscapeChar,dataEscapeChar} can be used to enter literal colon characters.
    //
    // @visibility external
    //<
    dataDisplaySeparatorChar: ":",

    //> @attr facetChartEditProxy.dataEscapeChar (String : "\" : IR)
    // If +link{inlineEditEvent,inline editing} for this chart edits the
    // +link{facetChart.data}, character that can be used to enter literal separator
    // chars (such as the +link{dataSeparatorChar,dataSeparatorChar}).
    // <p>
    // Repeat this character twice to enter it literally.  For example, with the default
    // of "\", inputting "\\" would result in a literal backslash in the value.
    //
    // @visibility external
    //<
    dataEscapeChar: "\\",

    //> @method facetChartEditProxy.getInlineEditText()
    // Returns the text based on the current component state to be edited inline.
    // Called by the +link{editProxy.inlineEditForm} to obtain the starting edit value.
    //
    // @visibility external
    //<
    getInlineEditText : function () {
        var liveObject = this.creator,
            seriesName = "series"
        ;

        var string = "";
        if (liveObject.title) string += "== " + liveObject.title + "\n";

        var facets = liveObject.facets,
            labels = [],
            titles = []
        ;
        for (var i = 0; i < facets.length; i++) {
            if (facets[i].inlinedValues) {
                var values = facets[i].values;
                for (var j = 0; j < values.length; j++) {
                    var value = values[j];
                    labels.add(value.id);
                    titles.add(value.title || value.id);
                }
            }
        }

        var data = liveObject.data,
            useValuesOnly = (liveObject.showDataLabels == false);

        for (var i = 0; i < data.length; i++) {
            var title = data[i][seriesName];
            if (title) string += "-- " + title + "\n";

            var series = "";
            for (var j = 0; j < labels.length; j++) {
                var value = data[i][labels[j]];
                if (series.length > 0) series += this.dataSeparatorChar;
                if (useValuesOnly) series += value;
                else series += titles[j] + this.dataDisplaySeparatorChar + value;
            }
            string += series + "\n";
        }

        return string;
    },

    //> @method facetChartEditProxy.setInlineEditText()
    // Save the new value into the component's state. Called by the
    // +link{editProxy.inlineEditForm} to commit the change.
    // <p>
    // Updates the component's <code>facets</code> and <code>data</code>.
    // <p>
    // Lines starting with "--" or "==" are considered titles. A single title
    // is used as the chart title. Titles are matched to the next series of
    // data. If titles are provided for each series, a legend will be shown.
    // <p>
    // Series data can be entered as a list of values separated by commas
    // (see +link{dataSeparatorChar,dataSeparatorChar}) or as a valueMap-style
    // list of <code>label:value</code> pairs. The first data series defines the
    // number of chart values and the titles, if provided.
    //
    // @param newValue (String) the new component data
    //
    // @visibility external
    //<
    setInlineEditText : function (newValue) {
        var seriesName = "series",
            seriesList = newValue.split("\n"),
            title,
            chartTitle,
            labels,
            facets = [],
            data = [],
            showLegend = true,
            showDataLabels = true
        ;
        for (var i = 0; i < seriesList.length; i++) {
            var series = seriesList[i];
            if (!series) continue;
            series = series.trim();
            if (series.length == 0) continue;

            if (series.startsWith("==") || series.startsWith("--")) {
                if (title) chartTitle = title;
                title = series.substring(2).trim();
            } else {
                var value = isc.EditProxy.parseStringValueMap(series,
                        this.dataSeparatorChar,
                        this.dataEscapeChar,
                        this.dataDisplaySeparatorChar);

                var seriesData = value.valueMap;
                if (!labels) {
                    labels = [];
                    if (isc.isAn.Array(seriesData)) {
                        // No labels on first series; auto-assign labels
                        for (var j = 0; j < seriesData.length; j++) {
                            labels.add("value" + j);
                        }
                        showDataLabels = false;
                    } else {
                        // Extract labels from first series
                        for (var key in seriesData) {
                            labels.add(key);
                        }
                    }

                    var facet = {
                        inlinedValues: true,
                        values: []
                    };
                    for (var j = 0; j < labels.length; j++) {
                        facet.values.add({ id: labels[j].toLowerCase(), title: labels[j] });
                        labels[j] = labels[j].toLowerCase();
                    }
                    facets.add(facet);
                    facets.add({ id: seriesName });
                }

                var record = {};
                record[seriesName] = title || ""; // Must be non-null for chart to parse
                if (!title) showLegend = false;

                if (isc.isAn.Array(seriesData)) {
                    // Slot data into record using labels
                    for (var j = 0; j < labels.length; j++) {
                        var label = labels[j];
                        record[label] = parseFloat(seriesData[j]) || 0;
                    }
                } else {
                    for (var key in seriesData) {
                        if (labels.contains(key.toLowerCase())) {
                            record[key.toLowerCase()] = parseFloat(seriesData[key]);
                        }
                    }
                    for (var j = 0; j < labels.length; j++) {
                        var label = labels[j];
                        if (record[label] == null) record[label] = 0;
                    }
                }
                data.add(record);

                // Title has been used; clear it
                title = null;
            }
        }

        if (data.length <= 1) {
            // No legend for a single facet
            showLegend = false;
            if (!chartTitle && data.length == 1 && data[0][seriesName] != seriesName) {
                // Single-series and single title provided; use as chart title
                chartTitle = data[0][seriesName];
            }
        }

        this.creator.editContext.setNodeProperties(this.creator.editNode, {
            title: chartTitle,
            showLegend: showLegend,
            showDataLabels: showDataLabels,
            facets: facets,
            data: data
        });
    }
});
