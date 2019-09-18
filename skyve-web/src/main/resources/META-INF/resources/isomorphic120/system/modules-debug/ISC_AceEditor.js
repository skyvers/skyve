/*

  SmartClient Ajax RIA system
  Version v12.0p_2019-08-11/LGPL Deployment (2019-08-11)

  Copyright 2000 and beyond Isomorphic Software, Inc. All rights reserved.
  "SmartClient" is a trademark of Isomorphic Software, Inc.

  LICENSE NOTICE
     INSTALLATION OR USE OF THIS SOFTWARE INDICATES YOUR ACCEPTANCE OF
     ISOMORPHIC SOFTWARE LICENSE TERMS. If you have received this file
     without an accompanying Isomorphic Software license file, please
     contact licensing@isomorphic.com for details. Unauthorized copying and
     use of this software is a violation of international copyright law.

  DEVELOPMENT ONLY - DO NOT DEPLOY
     This software is provided for evaluation, training, and development
     purposes only. It may include supplementary components that are not
     licensed for deployment. The separate DEPLOY package for this release
     contains SmartClient components that are licensed for deployment.

  PROPRIETARY & PROTECTED MATERIAL
     This software contains proprietary materials that are protected by
     contract and intellectual property law. You are expressly prohibited
     from attempting to reverse engineer this software or modify this
     software for human readability.

  CONTACT ISOMORPHIC
     For more information regarding license rights and restrictions, or to
     report possible license violations, please contact Isomorphic Software
     by email (licensing@isomorphic.com) or web (www.isomorphic.com).

*/

if(window.isc&&window.isc.module_Core&&!window.isc.module_AceEditor){isc.module_AceEditor=1;isc._moduleStart=isc._AceEditor_start=(isc.timestamp?isc.timestamp():new Date().getTime());if(isc._moduleEnd&&(!isc.Log||(isc.Log && isc.Log.logIsDebugEnabled('loadTime')))){isc._pTM={ message:'AceEditor load/parse time: ' + (isc._moduleStart-isc._moduleEnd) + 'ms', category:'loadTime'};
if(isc.Log && isc.Log.logDebug)isc.Log.logDebug(isc._pTM.message,'loadTime');
else if(isc._preLog)isc._preLog[isc._preLog.length]=isc._pTM;
else isc._preLog=[isc._pTM]}isc.definingFramework=true;


if (window.isc && isc.version != "v12.0p_2019-08-11/LGPL Deployment" && !isc.DevUtil) {
    isc.logWarn("SmartClient module version mismatch detected: This application is loading the core module from "
        + "SmartClient version '" + isc.version + "' and additional modules from 'v12.0p_2019-08-11/LGPL Deployment'. Mixing resources from different "
        + "SmartClient packages is not supported and may lead to unpredictable behavior. If you are deploying resources "
        + "from a single package you may need to clear your browser cache, or restart your browser."
        + (isc.Browser.isSGWT ? " SmartGWT developers may also need to clear the gwt-unitCache and run a GWT Compile." : ""));
}




//> @class AceEditor
// Component that wraps the Ace Editor.
// @inheritsFrom Canvas
// @treeLocation Client Reference/Control
// @visibility sgwt, noJavaDoc
//<
isc.ClassFactory.defineClass("AceEditor", "Canvas");
isc.AceEditor.addClassProperties({
    _applyCreateProperties : function (newInstance) {
        for (var i = 1; i < arguments.length; i++) {
            var arg = arguments[i];
            if (isc.isAn.Object(arg)) {
                for (var propertyName in arg) {
                    var value = arg[propertyName],
                        functionName = "set" + propertyName.substring(0,1).toUpperCase() +
                                       propertyName.substring(1)
                    ;
                    if (value != null) {
                        if (isc.isA.Function(newInstance[functionName])) {
                            newInstance[functionName](value);
                        }
                        else newInstance[propertyName] = value;
                    }
                }
            }
        }
    }
});
isc.AceEditor.addProperties({

    init : function () {
        // Override any configuration with required settings
        this.redrawOnResize = false;
        this.useClipDiv = false;
        this.styleName = "";

        this.Super("init", arguments);
    },

    destroy : function () {
        if (this.editor) this.editor.destroy();
        this.Super("destroy", arguments);
    },

    getInnerHTML : function () {
        var value = this.getValue();
        if (this.editor) {
            // TODO: Should the selection be retained?
            this.editor.destroy();
            this.editor = null;
        }
        return value;
    },

    modifyContent : function () {
        var editor = this._createEditor(),
            self = this
        ;

        // Register event handlers on editor
        editor.on('blur', function () { self._focusChanged(false); });
        editor.on('focus', function () { self._focusChanged(true); });
        editor.on('change', function (e) { self._changed(e); });

        this.editor = editor;

        // Push editor.setOptions() configuration
        if (this.autoComplete != null) this.setAutoComplete(this.autoComplete);
        if (this.liveAutoComplete != null) this.setLiveAutoComplete(this.liveAutoComplete);
    },

    _createEditor : function () {
        if (!isc.AceEditor._aceObjConstructor) {
            isc.AceEditor._aceObjConstructor = ace.require("./editor").Editor;
            ace.require("./lib/event");
        }

        var containerElement = this.getContentElement();

        var renderer = isc.AceRenderer.create(this.rendererDefaults, this.rendererProperties,
                                              { containerElement: containerElement }),
            undoManager = isc.AceUndoManager.create(),
            document = isc.AceDocument.create({ value: this.getValue() }),
            editSession = isc.AceEditSession.create({ document: document,
                undoManager: undoManager, mode: this.mode }, this.editSessionDefaults,
                                                    this.editSessionProperties)
        ;

        this.renderer = renderer;
        this.session = editSession;

        var editor = new isc.AceEditor._aceObjConstructor(renderer.getAceObj(),
                                                          editSession.getAceObj());
        // Reference so AceCompleter.insertMatch can be called correctly
        editor.scRef = this;

        // disable native browser key event handling on the AceEditor textArea
        var textAreaElement = containerElement.getElementsByTagName("textarea")[0];
        textAreaElement.setAttribute("handleNativeEvents", "false");

        isc.AceEditor._applyCreateProperties(editor, {
            theme: this.theme,
            readOnly: this.readOnly
        });

        return editor;
    },

    _getStyleRules : function (sheet) {
        if (sheet == null) return;

        var rules;
        try {
            rules = sheet.rules;
            if (rules == null) rules = sheet.cssRules;
        } catch (e) {
            // happens if stylesheet is loaded from a foreign host
            this.logDebug("skipped stylesheet '" + sheet +
                          "' due to exception accessing rules");
        }
        return rules;
    },

    // Z-index for auto-compelete popup is fixed in embedded
    // auto-complete CSS. This needs to be adjusted to show
    // on top of editor canvas.
    //
    // Only need to do this once
    _updateAutoCompleteCss : function () {
        if (this._updatedCss) return;
        this._updatedCss = true;

        var styleSheets = document.styleSheets;

        // for every loaded stylesheet without a URL
        for (var i = 0; i < styleSheets.length; i++) {
            if (styleSheets[i].href != null) continue;

            var rules = this._getStyleRules(styleSheets[i]);
            if (rules == null) continue;

            // first collect all sizes in an array
            var sizes = [];
            for (var j = 0; j < rules.length; j++) {
                var style = rules[j].style;
                if (style == null) continue;

                if (rules[j].selectorText == ".ace_editor.ace_autocomplete") {
                    var zindex = style["z-index"];
                    if (zindex != null) {
                        zindex = parseInt(zindex);
                        var minZIndex = this.getZIndex(true);
                        if (zindex < minZIndex) {
                            style["z-index"] = minZIndex + 2;
                        }
                    }

                }
            }
        }
    },

    _changed : function (e) {
        if (this.changed) this.changed(e.start, e.end, e.action, e.lines);
    },

    focus : function () {
        this.Super("focus", arguments);
        this.editor.focus();
    },

    blur : function (reason) {
        this.Super("blur", arguments);
        this.editor.blur();
    },

    resized : function () {
        if (this.editor) this.editor.resize();
    },

    dropMove : function () {
        if (this.editor.getReadOnly() || !this.willAcceptDrop()) return;
        // Show cursor at drop point
        var newCursorPosition = this.editor.renderer.
                screenToTextCoordinates(isc.EventHandler.getX(), isc.EventHandler.getY());
        this.editor.navigateTo(newCursorPosition.row, newCursorPosition.column);
    },

    //> @method aceEditor.getAceObj()
    // Returns the internal Ace Editor object.
    //
    // @return (JavaScriptObject) editor object
    // @visibility sgwt
    //<
    getAceObj : function () {
        return this.editor;
    },

    //> @method aceEditor.getSession()
    // Returns the Ace editor edit session object.
    //
    // @return (AceEditSession) the edit session object
    // @visibility sgwt
    //<
    getSession : function () {
        return this.session;
    },

    //> @method aceEditor.setSession()
    // Sets a new editsession to use.
    //
    // @param session (AceEditSession) the new edit session
    // @visibility sgwt
    //<
    setSession : function (session) {
        this.session = session;
        this.editor.setSession(session.getAceObj());
    },

    //> @method aceEditor.getRenderer()
    // Returns the Ace editor renderer object.
    //
    // @return (AceRenderer) the renderer object
    // @visibility sgwt
    //<
    getRenderer : function () {
        return this.renderer;
    },

    //> @attr aceEditor.mode (String : null : IRW)
    // Set the editor text mode.
    //
    // @setter setMode()
    // @visibility sgwt
    //<

    //> @method aceEditor.setMode()
    // Set the editor text mode.
    //
    // @param mode (String) text mode
    // @visibility sgwt
    //<
    setMode : function (mode) {
        this.mode = mode;
        if (this.getSession()) this.getSession().getAceObj().setMode(mode);
    },

    //> @attr aceEditor.value (String : null : IRW)
    // The value to edit.
    //
    // @setter setValue()
    // @visibility sgwt
    //<

    //> @method aceEditor.setValue()
    // Changes the value of the editor to newValue.
    //
    // @param newValue (String)  a string to be set as the editor contents
    // @visibility sgwt
    //<
    setValue : function(newValue) {
        if (this.editor) {
            this.editor.setValue(newValue);
        }
    },

    //> @method aceEditor.getValue()
    // Returns the value of the editor as a string.
    //
    // @return (String) value of this editor
    // @visibility sgwt
    //<
    getValue : function() {
        var value = this.value;
        if (this.editor) {
            value = this.editor.getValue();
        }
        return value;
    },

    //> @attr aceEditor.readOnly (Boolean : null : IRW)
    // Set the editor to read-only.
    //
    // @setter setReadOnly()
    // @visibility sgwt
    //<

    //> @method aceEditor.setReadOnly()
    // Set the editor read-only state.
    //
    // @param readOnly (Boolean)  true to place editor in read-only mode
    // @visibility sgwt
    //<
    setReadOnly : function (readOnly) {
        this.readOnly = readOnly;
        if (this.editor) this.editor.setReadOnly(readOnly);
    },

    //> @method aceEditor.addSelectionMarker()
    // Adds the selection and cursor.
    //
    // @param orientedRange (AceRange) oriented selection range
    // @return (AcePosition) selection range
    // @visibility sgwt
    //<
    addSelectionMarker : function (orientedRange) {
        return this.editor.addSelectionMarker(orientedRange);
    },

    //> @method aceEditor.removeSelectionMarker()
    // Removes the selection marker.
    //
    // @param selectionRange (AceRange) selection range from addSelectionMarker
    // @visibility sgwt
    //<
    removeSelectionMarker : function (selectionRange) {
        this.editor.removeSelectionMarker(selectionRange);
    },

    //> @method aceEditor.getCursorPosition()
    // Returns the current position of the cursor.
    //
    // @return (AcePosition) current cursor position
    // @visibility sgwt
    //<
    getCursorPosition : function () {
        return this.editor.getCursorPosition();
    },

    //> @method aceEditor.getCursorPositionScreen()
    // Returns the screen position of the cursor.
    //
    // @return (AcePosition) current cursor screen position
    // @visibility sgwt
    //<
    getCursorPositionScreen : function () {
        return this.editor.getCursorPositionScreen();
    },

    //> @method aceEditor.clearSelection()
    // Empties the selection (by de-selecting it).
    //
    // @visibility sgwt
    //<
    clearSelection : function () {
        this.editor.clearSelection();
    },

    //> @method aceEditor.getSelectionRange()
    // Returns the range for the current selection.
    //
    // @return (AceRange) current selection range
    // @visibility sgwt
    //<
    getSelectionRange : function () {
        var range = this.editor.getSelectionRange();
        if (range == null) return null;
        return (range.scRef ? range.scRef : isc.AceRange.create({ range: range }));
    },

    //> @method aceEditor.getFirstVisibleRow()
    // Returns the index of the first visible row.
    //
    // @return (Integer) first visible row index
    // @visibility sgwt
    //<
    getFirstVisibleRow : function () {
        return this.editor.getFirstVisibleRow();
    },

    //> @method aceEditor.getLastVisibleRow()
    // Returns the index of the last visible row.
    //
    // @return (Integer) first visible row index
    // @visibility sgwt
    //<
    getLastVisibleRow : function () {
        return this.editor.getLastVisibleRow();
    },

    //> @method aceEditor.isRowVisible()
    // Returns true if the row is currently visible on the screen.
    //
    // @param row (Integer) row
    // @return (Boolean) true if row is visible
    // @visibility sgwt
    //<
    isRowVisible : function (row) {
        return this.editor.isRowVisible(row);
    },

    //> @method aceEditor.insert()
    // Inserts text at the current cursor position.
    //
    // @param text (String) text to insert
    // @visibility sgwt
    //<
    insert : function (text) {
        this.editor.insert(text);
    },

    //> @method aceEditor.redo()
    // Perform a redo operation on the document, reimplementing the last change.
    //
    // @visibility sgwt
    //<
    redo : function () {
        this.editor.redo();
    },

    //> @method aceEditor.undo()
    // Perform a undo operation on the document, reverting the last change.
    //
    // @visibility sgwt
    //<
    undo : function () {
        this.editor.undo(dontSelect || false);
    },

    //> @method aceEditor.updateSelectionMarkers()
    // Updates the cursor and marker layers.
    //
    // @visibility sgwt
    //<
    updateSelectionMarkers : function () {
        this.editor.updateSelectionMarkers();
    },

    //> @attr aceEditor.autoComplete (Boolean : null : IRW)
    // Should this editor allow auto-completion of text?
    //
    // @setter setAutoComplete()
    // @visibility sgwt
    //<

    //> @method aceEditor.setAutoComplete()
    // Setter for autoComplete.
    //
    // @param autoComplete (Boolean) true to enable auto-completion
    // @visibility sgwt
    //<
    setAutoComplete : function(autoComplete) {
        this.autoComplete = autoComplete;
        if (this.editor) {
            window.ace.require("./ext/language_tools");
            this.editor.setOptions({ enableBasicAutocompletion: autoComplete });
            if (autoComplete) {
                this._updateAutoCompleteCss();
            }
        }
    },

    //> @attr aceEditor.liveAutoComplete (Boolean : null : IRW)
    // Should this editor allow live auto-completion of text?
    //
    // @setter setLiveAutoComplete()
    // @visibility sgwt
    //<

    //> @method aceEditor.setLiveAutoComplete()
    // Setter for liveAutoComplete.
    //
    // @param liveAutoComplete (Boolean) true to enable live auto-completion
    // @visibility sgwt
    //<
    setLiveAutoComplete : function(liveAutoComplete) {
        this.liveAutoComplete = liveAutoComplete;
        if (this.editor) {
            ace.require("./ext/language_tools");
            this.editor.setOptions({ enableLiveAutocompletion: liveAutoComplete });
            if (liveAutoComplete) {
                this._updateAutoCompleteCss();
            }
        }
    },

    //> @method aceEditor.setCompleters()
    // Replaces all existing auto-completion handlers.
    //
    // @param completers (Array of AceCompleter) auto-completion handlers
    // @visibility sgwt
    //<
    setCompleters : function (completers) {
        if (!isc.AceEditor._aceLangTools) {
            isc.AceEditor._aceLangTools = ace.require("./ext/language_tools");
        }

        if (completers != null) {
            completers = (isc.isAn.Array(completers) ? completers : [completers]);
            for (var i = 0; i < completers.length; i++) {
                this._prepareCompleter(completers[i]);
            }
        }
        isc.AceEditor._aceLangTools.setCompleters(completers);
    },

    //> @method aceEditor.addCompleter()
    // Add new auto-completion handler.
    //
    // @param completer (AceCompleter) auto-completion handler
    // @visibility sgwt
    //<
    addCompleter : function (completer) {
        if (!isc.AceEditor._aceLangTools) {
            isc.AceEditor._aceLangTools = ace.require("./ext/language_tools");
        }

        this._prepareCompleter(completer);
        isc.AceEditor._aceLangTools.addCompleter(completer);
    },

    _prepareCompleter : function (completer) {
        // Wrap getCompletions() to mark each completion with
        // the completer instance so the insertMatch() method
        // is called.
        completer._getCompletions = completer.getCompletions;
        completer.getCompletions = function (editor, session, pos, prefix, callback) {
            if (completer._getCompletions) {
                return completer._getCompletions(editor, session, pos, prefix,
                    function(results) {
                        if (completer.useCustomInsertMatch && completer.insertMatch) {
                            if (results != null) {
                                for (var i = 0; i < results.length; i++) {
                                    results[i].completer = (completer.getJsObj ?
                                                            completer.getJsObj : completer);
                                }
                            }
                        }
                        // First param, err, is only used to indicate
                        // there are no results. A null or empty results
                        // is equivalent
                        callback(null, results);
                    }
                );
            } else {
                callback(null, []);
            }
        }

        if (completer.useCustomInsertMatch && completer.insertMatch) {
            // Wrap insertMatch() to provide the filterText value so
            // it can be removed to insert the full value
            completer._insertMatch = completer.insertMatch;
            completer.insertMatch = function (editor, data) {
                var filterText = editor.completer.completions.filterText;
                completer._insertMatch(editor.scRef, filterText, data);
            }
        }
    }
});

isc.AceEditor.registerStringMethods({
    //> @method aceEditor.changed()
    // Called when the editor's value has been changed.
    // @param start  (AcePosition) the start position of the change
    // @param end    (AcePosition) the end position of the change
    // @param action (String) action that caused change
    // @param lines  (Array) list of affected lines
    // @group eventHandling
    // @visibility sgwt
    //<
    changed:"start,end,action,lines"
});


//> @class AceCompleter
// Ace Editor autocomplete handler.
//
// @treeLocation Client Reference/Control/AceEditor
// @visibility sgwt
//<
isc.ClassFactory.defineClass("AceCompleter");
isc.AceCompleter.addProperties({
    //> @attr aceCompleter.useCustomInsertMatch (Boolean : null : IR)
    // When set, the custom +link{insertMatch} method is called
    // to allow control over how the insertion of the selected match
    // is performed. Ace will not perform any cleanup of the partial
    // entry or insert the selected value.
    //
    // @visibility sgwt
    //<
});
isc.AceCompleter.registerStringMethods({
    //> @method aceCompleter.getCompletions()
    // Returns the list of completions.
    // @param editor (AceEditor) the editor
    // @param session (AceEditSession) the edit session
    // @param pos    (AcePosition) the position of the entry
    // @param prefix (String) the entered prefix to auto-complete
    // @param callback (AceCompleterCompletionsCallback) callback called to return completions
    // @visibility sgwt
    //<
    getCompletions:"editor,session,pos,prefix,callback",

    //> @method aceCompleter.insertMatch()
    // Called to insert matched auto-complete value when
    // +link{aceCompleter.useCustomInsertMatch} is <code>true</code>.
    // Ace will not call this method and will insert the matched value
    // directly when not set.
    // @param editor (AceEditor) the editor
    // @param filterText (String) the partial text of the auto-complete filter
    // @param data (AceCompletionResult) the completion result
    // @visibility sgwt
    //<
    insertMatch:"editor,filterText,data"
});

//> @method Callbacks.AceCompleterCompletionsCallback
// A +link{type:Callback} to return auto-completion results for user selection
// from +link{aceCompleter.getCompletions}.
//
// @param results (Array of AceCompletionResult) list of auto-complete results
//
// @visibility sgwt
//<


//> @class AceCompletionResult
// Ace Editor auto-completion result. Used to show
// list of potential auto-completion values and for
// inserting the new value into the editor.
//
// @treeLocation Client Reference/Control/AceEditor
// @visibility sgwt
//<
isc.ClassFactory.defineClass("AceCompletionResult");
isc.AceCompletionResult.addProperties({
    //> @attr aceCompletionResult.caption (String : null : IR)
    // Set the result caption. Note this value must be unique
    // within the potential completion results.
    //
    // @visibility sgwt
    //<

    //> @attr aceCompletionResult.value (String : null : IR)
    // Set the result value to be inserted if selected.
    //
    // @visibility sgwt
    //<

    //> @attr aceCompletionResult.score (Integer : null : IR)
    // Set the result score. Used to sort results by best match.
    //
    // @visibility sgwt
    //<

    //> @attr aceCompletionResult.meta (String : null : IR)
    // Set the result meta value. Displayed to far right of
    // potential completion dialog.
    //
    // @visibility sgwt
    //<

    //> @attr aceCompletionResult.tooltip (String : null : IR)
    // Set the result tooltip.
    //
    // @visibility sgwt
    //<
});


//> @class AceRenderer
// Ace Editor renderer.
//
// @treeLocation Client Reference/Control/AceEditor
// @visibility sgwt
//<
isc.ClassFactory.defineClass("AceRenderer");
isc.AceRenderer.addClassProperties({
    baseProperties: [
        "showGutter",
        "showPrintMargin"
    ]
});
isc.AceRenderer.addMethods({
    init : function () {
        if (!isc.AceRenderer._aceObjConstructor) {
            isc.AceRenderer._aceObjConstructor =
                ace.require("./virtual_renderer").VirtualRenderer;
        }

        var renderer = new isc.AceRenderer._aceObjConstructor(this.containerElement,
                                                              this.theme);
        renderer.scRef = this;

        // Some property setters cannot be called until the session has been set
        var self = this;
        renderer._setSession = renderer.setSession;
        renderer.setSession = function (session) {
            this._setSession(session);
            self.pushInitialSettings();
        };

        this.renderer = renderer;
    },

    destroy : function () {
        if (this.renderer) this.renderer.destroy();
        this.Super("destroy", arguments);
    },

    getAceObj : function () {
        return this.renderer;
    },

    pushInitialSettings : function () {
        for (var i = 0; i < isc.AceRenderer.baseProperties.length; i++) {
            var propertyName = isc.AceRenderer.baseProperties[i],
                value = this[propertyName],
                functionName = "set" + propertyName.substring(0,1).toUpperCase() +
                               propertyName.substring(1)
            ;
            if (value != null) {
                if (isc.isA.Function(this[functionName])) this[functionName](value);
                else this[propertyName] = value;
            }
        }
    },

    //> @method aceRenderer.hideCursor()
    // Hides the cursor icon.
    //
    // @visibility sgwt
    //<
    hideCursor : function () {
        this.renderer.hideCursor();
    },

    //> @method aceRenderer.showCursor()
    // Shows the cursor icon.
    //
    // @visibility sgwt
    //<
    showCursor : function () {
        this.renderer.showCursor();
    }
});
isc.AceRenderer.addProperties({
    //> @attr aceRenderer.theme (String : null : IRW)
    // Set the editor theme.
    //
    // @setter setTheme()
    // @visibility sgwt
    //<

    //> @method aceRenderer.setTheme()
    // Set the editor theme.
    //
    // @param theme (String) new theme found in ace/theme directory
    // @visibility sgwt
    //<
    setTheme : function (theme) {
        this.theme = theme;
        this.renderer.setTheme(theme);
    },

    //> @attr aceRenderer.showGutter (Boolean : true : IRW)
    // Set the renderer to show gutter.
    //
    // @setter setShowGutter()
    // @visibility sgwt
    //<

    //> @method aceRenderer.setShowGutter()
    // Set the renderer to show gutter.
    //
    // @param show (Boolean) true to show gutter
    // @visibility sgwt
    //<
    setShowGutter : function (show) {
        this.showGutter = show;
        this.renderer.setShowGutter(show);
    },

    //> @attr aceRenderer.showPrintMargin (Boolean : true : IRW)
    // Set the renderer to show print margin.
    //
    // @setter setShowPrintMargin()
    // @visibility sgwt
    //<

    //> @method aceRenderer.setShowPrintMargin()
    // Set the renderer to show print margin.
    //
    // @param show (Boolean) true to show print margin
    // @visibility sgwt
    //<
    setShowPrintMargin : function (show) {
        this.showPrintMargin = show;
        this.renderer.setShowPrintMargin(show);
    }
});

//> @class AceDocument
// Ace Editor document.
//
// @treeLocation Client Reference/Control/AceEditor
// @visibility sgwt
//<
isc.ClassFactory.defineClass("AceDocument");
isc.AceDocument.addMethods({
    init : function () {
        if (!isc.AceDocument._aceObjConstructor) {
            isc.AceDocument._aceObjConstructor = ace.require("./document").Document;
        }

        this.document = new isc.AceDocument._aceObjConstructor(this.value);
        this.document.scRef = this;
    },

    //> @method aceDocument.getAceObj()
    // Returns the internal Ace Document object.
    //
    // @return (JavaScriptObject) document object
    // @visibility sgwt
    //<
    getAceObj : function () {
        return this.document;
    },

    //> @method aceDocument.createAnchor()
    // Creates a new Anchor to define a floating point in the document.
    //
    // @param row (Integer) the row
    // @param column (Integer) the column
    // @return (AceAnchor) the new anchor
    // @visibility sgwt
    //<
    createAnchor : function (row, column) {
        if (isc.isAn.Object(row)) {
            column = row.column;
            row = row.row;
        }
        var anchor = this.document.createAnchor(row, column);
        return (anchor ? isc.AceAnchor.create({ anchor: anchor }) : null);
    },

    //> @method aceDocument.getLength()
    // Returns the number of rows in the document.
    //
    // @return (Integer) the row count
    // @visibility sgwt
    //<
    getLength : function () {
        return this.document.getLength();
    },

    //> @method aceDocument.getTextRange()
    // Returns the text within the specified range.
    //
    // @param range (AceRange) the range
    // @return (String) the text in the range
    // @visibility sgwt
    //<
    getTextRange : function (range) {
        return this.document.getTextRange(range.getAceObj());
    }
});
isc.AceDocument.addProperties({
    //> @attr aceDocument.value  (String : null : IRW)
    // The document contents.
    //
    // @setter setValue()
    // @getter getValue()
    // @visibility sgwt
    //<

    //> @method aceDocument.getValue()
    // Returns all lines in the document as a single string.
    //
    // @return (String) the document text
    // @visibility sgwt
    //<
    getValue : function () {
        return this.document.getValue();
    },

    //> @method aceDocument.setValue()
    // Replaces all the lines in the current document with the new value.
    //
    // @param text (String) the document text
    // @visibility sgwt
    //<
    setValue : function (text) {
        this.document.setValue(text);
    }
});

//> @class AceEditSession
// Ace Editor edit session.
//
// @treeLocation Client Reference/Control/AceEditor
// @visibility sgwt
//<
isc.ClassFactory.defineClass("AceEditSession");
isc.AceEditSession.addClassProperties({
    baseProperties: [
        "undoManager",
        "tabSize"
    ]
});
isc.AceEditSession.addMethods({
    init : function () {
        if (!isc.AceEditSession._aceObjConstructor) {
            isc.AceEditSession._aceObjConstructor = ace.require("./edit_session").EditSession;
        }

        // Avoid creating a new session object when wrapping an existing one
        // This should be unnecessary but occurs because SGWT doesn't see the
        // original editor session object as "created" so it calls getOrCreateJsObj()
        // triggering a new instance to be created around the existing object.
        // Will be looking into preventing this at a later date.
        if (!this.session) {
            var contents = (this.document ? this.document.getAceObj() : this.value);
            this.session = new isc.AceEditSession._aceObjConstructor(contents, this.mode);
            this.session.scRef = this;

            this.pushInitialSettings();
        }
    },

    //> @method aceEditSession.getAceObj()
    // Returns the internal Ace Session object.
    //
    // @return (JavaScriptObject) session object
    // @visibility sgwt
    //<
    getAceObj : function () {
        return this.session;
    },

    pushInitialSettings : function () {
        for (var i = 0; i < isc.AceEditSession.baseProperties.length; i++) {
            var propertyName = isc.AceEditSession.baseProperties[i],
                value = this[propertyName],
                functionName = "set" + propertyName.substring(0,1).toUpperCase() +
                               propertyName.substring(1)
            ;
            if (value != null) {
                if (isc.isA.Function(this[functionName])) this[functionName](value);
                else this[propertyName] = value;
            }
        }
    },

    //> @method aceEditSession.getValue()
    // Returns the current document as a string.
    //
    // @return (String) current document as string
    // @visibility sgwt
    //<
    getValue : function() {
        return this.session.getValue();
    },

    //> @method aceEditSession.setUndoManager
    // Sets the undo manager.
    //
    // @param undoManager (AceUndoManager) the undo manager
    // @visibility sgwt
    //<
    setUndoManager : function (undoManager) {
        this.undoManager = undoManager;
        this.session.setUndoManager(undoManager.getAceObj());
    },

    //> @method aceEditSession.getUndoManager
    // Returns the current undo manager.
    //
    // @return (AceUndoManager) the undo manager
    // @visibility sgwt
    //<
    getUndoManager : function () {
        return this.undoManager || isc.AceUndoManager.create({
            undoManager: this.session.getUndoManager()
        });
    },

    //> @method aceEditSession.getDocument
    // Returns the document.
    //
    // @return (AceDocument) the edited document
    // @visibility sgwt
    //<
    getDocument : function () {
        return this.document;
    },

    //> @method aceEditSession.insert()
    // Inserts text at the specified position.
    //
    // @param position (AcePosition) position of insert
    // @param text (String) text to insert
    // @visibility sgwt
    //<
    insert : function (position, text) {
        this.session.insert(position, text);
    },

    //> @method aceEditSession.remove()
    // Removes the text at the specified range.
    //
    // @param range (AceRange) range of text
    // @visibility sgwt
    //<
    remove: function (range) {
        this.session.remove(range.getAceObj());
    },

    //> @method aceEditSession.getLength()
    // Returns the number of rows in the document.
    //
    // @return (Integer) the row count
    // @visibility sgwt
    //<
    getLength : function () {
        return this.session.getLength();
    },

    //> @method aceEditSession.getLine()
    // Returns a verbatim copy of the given line as it is in the document.
    //
    // @param row (Integer) the row
    // @return (String) the line text
    // @visibility sgwt
    //<
    getLine : function (row) {
        return this.session.getLine(row);
    },

    //> @method aceEditSession.getLines()
    // Returns an array of strings of the rows between <code>firstRow</code>
    // and <code>lastRow</code> inclusive.
    //
    // @param firstRow (Integer) the first row
    // @param lastRow (Integer) the last row
    // @return (Array of String) the lines with the range
    // @visibility sgwt
    //<
    getLines : function (firstRow, lastRow) {
        return this.session.getLines(firstRow, lastRow);
    },

    //> @method aceEditSession.addMarker()
    // Adds a new marker to the given fixed range.
    //
    // @param range (AceRange) range of text
    // @param style (CSSStyleName) CSS style for marker
    // @param type (String) type of marker
    // @param [inFront] (Boolean) true to add to front markers; false to add to back markers
    // @return (Integer) the new marker id
    // @visibility sgwt
    //<
    addMarker : function (range, style, type, inFront) {
        return this.session.addMarker(range.getAceObj(), style, type, inFront);
    },

    //> @method aceEditSession.addFloatingMarker()
    // Adds a new marker to the given floating range. Anchors are
    // placed at the start and end of the range which move as text
    // is edited so the marker moves as well.
    //
    // @param range (AceRange) range of text to be marked
    // @param style (CSSStyleName) CSS style for marker
    // @param type (String) type of marker
    // @param [inFront] (Boolean) true to add to front markers; false to add to back markers
    // @return (Integer) the new marker id
    // @visibility sgwt
    //<
    addFloatingMarker : function (range, style, type, inFront) {
        // Replace range start/end objects with anchors
        range.setStart(this.getDocument().createAnchor(range.getStart()));
        range.setEnd(this.getDocument().createAnchor(range.getEnd()));

        return this.session.addMarker(range.getAceObj(), style, type, inFront);
    },

    //> @method aceEditSession.removeMarker()
    // Removes the marker with the specified ID.
    //
    // @param id (Integer) marker to remove
    // @visibility sgwt
    //<
    removeMarker : function (id) {
        this.session.removeMarker(id);
    },

    //> @method aceEditSession.getMarker()
    // Returns marker for specified id, if defined.
    //
    // @param id (Integer) marker id
    // @return (AceMarker) the marker
    // @visibility sgwt
    //<
    getMarker : function (id) {
        var markers = this.session.getMarkers(),
            marker = (markers ? markers[id] : null)
        ;
        // Wrap marker for SmartClient
        return (marker ? isc.AceMarker.create({ marker: marker }) : null);
    },

    //> @method aceEditSession.getMarkers()
    // Returns an array of all markers.
    //
    // @param [inFront] (Boolean) true for front markers, false for back
    // @return (Array of AceMarker) the markers
    // @visibility sgwt
    //<
    getMarkers : function (inFront) {
        var rawMarkers = this.session.getMarkers(inFront),
            markers = []
        ;

        for (var id in rawMarkers) {
            markers.push(isc.AceMarker.create({ marker: rawMarkers[id] }));
        }

        return markers;
    },

    //> @method aceEditSession.getTextRange()
    // Returns the text within the specified range.
    //
    // @param range (AceRange) the range
    // @return (String) the text in the range
    // @visibility sgwt
    //<
    getTextRange : function (range) {
        return this.session.getTextRange(range.getAceObj());
    }
});
isc.AceEditSession.addProperties({
    //> @attr aceEditSession.tabSize    (Integer : 4 : IRW)
    // Sets the number of spaces that define a soft tab.
    //
    // @setter setTabSize()
    // @visibility sgwt
    //<

    //> @method aceEditSession.setTabSize()
    // Sets the number of spaces that define a soft tab.
    //
    // @param tabSize (Integer) tab size
    // @visibility sgwt
    //<
    setTabSize : function (tabSize) {
        this.tabSize = tabSize;
        this.session.setTabSize(tabSize);
    }
});

//> @class AceUndoManager
// Ace Editor undo manager.
//
// @treeLocation Client Reference/Control/AceEditor
// @visibility sgwt
//<
isc.ClassFactory.defineClass("AceUndoManager");
isc.AceUndoManager.addMethods({
    init : function () {
        if (!isc.AceUndoManager._aceObjConstructor) {
            isc.AceUndoManager._aceObjConstructor = ace.require("./undomanager").UndoManager;
        }

        this.undoManager = new isc.AceUndoManager._aceObjConstructor();
        this.undoManager.scRef = this;
    },

    getAceObj : function () {
        return this.undoManager;
    },

    //> @method aceUndoManager.hasRedo()
    // Returns true if there are redo operations left to perform.
    //
    // @return (boolean) redo operations pending status
    // @visibility sgwt
    //<
    hasRedo : function () {
        return this.undoManager.hasRedo();
    },

    //> @method aceUndoManager.hasUndo()
    // Returns true if there are undo operations left to perform.
    //
    // @return (boolean) undo operations pending status
    // @visibility sgwt
    //<
    hasUndo : function () {
        return this.undoManager.hasUndo();
    },

    //> @method aceUndoManager.redo()
    // Perform a redo operation on the document, reimplementing the last change.
    //
    // @param dontSelect (Boolean) if true, doesn't select the range of where the change
    //                             occurred
    // @visibility sgwt
    //<
    redo : function (dontSelect) {
        return this.undoManager.redo(dontSelect || false);
    },

    //> @method aceUndoManager.undo()
    // Perform a undo operation on the document, reverting the last change.
    //
    // @param dontSelect (Boolean) if true, doesn't select the range of where the change
    //                             occurred
    // @visibility sgwt
    //<
    undo : function (dontSelect) {
        return this.undoManager.undo(dontSelect || false);
    },

    //> @method aceUndoManager.reset()
    // Destroys the stack of undo and redo redo operations.
    //
    // @visibility sgwt
    //<
    reset : function () {
        return this.undoManager.reset();
    }
});

//> @class AceRange
// Ace Editor range.
//
// @treeLocation Client Reference/Control/AceEditor
// @visibility sgwt
//<
isc.ClassFactory.defineClass("AceRange");
isc.AceRange.addMethods({
    init : function () {
        if (!isc.AceRange._aceObjConstructor) {
            isc.AceRange._aceObjConstructor = ace.require("./range").Range;
        }

        if (!this.range) {
            var start = this.start || {},
                end = this.end || {}
            ;
            this.range = new isc.AceRange._aceObjConstructor(start.row || 0, start.column || 0,
                                                             end.row   || 0, end.column   || 0);
        }
        this.range.scRef = this;
    },

    getAceObj : function () {
        return this.range;
    },

    //> @method aceRange.clone()
    // Returns a duplicate of the calling range.
    //
    // @return (AceRange) copy of range
    // @visibility sgwt
    //<
    clone : function () {
        return isc.AceRange.create({ range: this.range.clone() });
    },

    //> @method aceRange.compare()
    // Compares the row and column points to the range returing a sort-convenient result.
    //
    // @param row (Integer) row
    // @param column (Integer) column
    // @return (Integer) 0 if point is within range (inclusive), -1 if point &lt; range start,
    //                   1 if point &gt; range end
    // @visibility sgwt
    //<
    compare : function (row, column) {
        return this.range.compare(row, column);
    },

    //> @method aceRange.compareInside()
    // Compares the row and column points to the range returing a sort-convenient result.
    //
    // @param row (Integer) row
    // @param column (Integer) column
    // @return (Integer) 0 if point is inside range (exclusive), -1 if point &lt;= range start,
    //                   1 if point &gt;= range end
    // @visibility sgwt
    //<
    compare : function (row, column) {
        return this.range.compare(row, column);
    },

    //> @method aceRange.comparePosition()
    // Compares the row and column of the position to the range returing a sort-convenient result.
    //
    // @param position (AcePosition) position
    // @return (Integer) 0 if position is within range (inclusive), -1 if position &lt;
    //                   range start, 1 if position &gt; range end
    // @visibility sgwt
    //<
    comparePosition : function (position) {
        return this.range.comparePoint(position);
    },

    //> @method aceRange.comparePoint()
    // Synonym of +link{comparePosition}.
    //
    // @param position (AcePosition) position
    // @return (Integer) 0 if position is within range (inclusive), -1 if position &lt;
    //                   range start, 1 if position &gt; range end
    // @visibility sgwt
    //<
    comparePoint : function (position) {
        return this.range.comparePoint(position);
    },

    //> @method aceRange.compareRange()
    // Compares this range with another range.
    //
    // @param range (AceRange) the other range
    // @return (Integer) 0 if other range is within this range (inclusive), -1 if other range
    //                   &lt; range start, 1 if position &gt; range end
    // @visibility sgwt
    //<
    compareRange : function (range) {
        return this.range.compareRange(range.getAceObj());
    },

    //> @method aceRange.contains()
    // Returns true if the row and column provided are within the given range.
    //
    // @param row (Integer) row
    // @param column (Integer) column
    // @return (boolean) true if row/column is within range
    // @visibility sgwt
    //<
    contains : function (row, column) {
        return this.range.contains(row, column);
    },

    //> @method aceRange.containsRange()
    // Returns true if the range is contained within the caller's range.
    //
    // @param range (AceRange) range
    // @return (boolean) true if range is within this range
    // @visibility sgwt
    //<
    containsRange : function (range) {
        return this.range.containsRange(range.getAceObj());
    },

    //> @method aceRange.extend()
    // Changes the row and column points for the calling range to
    // extend the range to include the new point.
    //
    // @param row (Integer) row
    // @param column (Integer) column
    // @return (AceRange) the original range if point is within; otherwise a new range including
    //                    the point
    // @visibility sgwt
    //<
    extend : function (row, column) {
        var range = this.range.extend(row, column);
        if (this.range === range) {
            return this;
        }
        return isc.AceRange.create({ range: range });
    },

    //> @method aceRange.fromPoints()
    // Synonym for +link{fromPosition}.
    //
    // @param startPosition (AcePosition) starting position
    // @param endPosition (AcePosition) ending position
    // @return (AceRange) the new range
    // @visibility sgwt
    //<
    fromPoints : function (startPosition, endPosition) {
        return isc.AceRange.create({ range: this.range.fromPoints(startPosition, endPosition) });
    },

    //> @method aceRange.fromPosition()
    // Creates a new +link{AceRange} based on the row and column of the positions.
    //
    // @param startPosition (AcePosition) starting position
    // @param endPosition (AcePosition) ending position
    // @return (AceRange) the new range
    // @visibility sgwt
    //<
    fromPosition : function (startPosition, endPosition) {
        return isc.AceRange.create({
            range: this.range.fromPoints(startPosition, endPosition)
        });
    },

    //> @method aceRange.inside()
    // Returns <code>true</code> if the row and column are within the given range (exclusive).
    //
    // @param row (Integer) row
    // @param column (Integer) column
    // @return (Boolean) true is point is inside range (exclusive)
    // @visibility sgwt
    //<
    inside : function (row, column) {
        return this.range.inside(row, column);
    },

    //> @method aceRange.isEqual()
    // Returns true if the starting row/column and ending row/column
    // are equivalent to those given by <code>range</code>.
    //
    // @param range (AceRange) range
    // @return (boolean) true if ranges are equal
    // @visibility sgwt
    //<
    isEqual : function (range) {
        return this.range.isEqual(range.getAceObj());
    },

    //> @method aceRange.isMultiLine()
    // Returns true if the range spans across multiple lines.
    //
    // @return (boolean) true if range spans lines
    // @visibility sgwt
    //<
    isMultiLine : function () {
        return this.range.isMultiLine();
    },

    //> @method aceRange.toString()
    // Returns a string containing the range's row and column information.
    //
    // @return (String) the range as a string
    // @visibility sgwt
    //<
    toString : function () {
        return this.range.toString();
    }

});
isc.AceRange.addProperties({
    //> @attr aceRange.start    (AcePosition : null : IRW)
    // Start position of range.
    //
    // @setter setStart()
    // @getter getStart()
    // @visibility sgwt
    //<

    //> @method aceRange.setStart()
    // Sets the range starting position.
    //
    // @param position (AcePosition) start position
    // @visibility sgwt
    //<
    setStart : function (position) {
        if (position.$onChange) {
            this.range.start = position;
        } else if (isc.isAn.AceAnchor(position)) {
            this.range.start = position.getAceObj();
        } else {
            this.range.setStart(position.row, position.column);
        }
    },

    //> @method aceRange.getStart()
    // Returns the range starting position.
    //
    // @return (AcePosition) the start position
    // @visibility sgwt
    //<
    getStart : function () {
        return this.range.start;
    },

    //> @attr aceRange.end    (AcePosition : null : IRW)
    // End position of range.
    //
    // @setter setEnd()
    // @getter getEnd()
    // @visibility sgwt
    //<

    //> @method aceRange.setEnd()
    // Sets the range ending position.
    //
    // @param position (AcePosition) end position
    // @visibility sgwt
    //<
    setEnd : function (position) {
        if (position.$onChange) {
            this.range.end = position;
        } else if (isc.isAn.AceAnchor(position)) {
            this.range.end = position.getAceObj();
        } else {
            this.range.setEnd(position.row, position.column);
        }
    },

    //> @method aceRange.getEnd()
    // Returns the range ending position.
    //
    // @return (AcePosition) the end position
    // @visibility sgwt
    //<
    getEnd : function () {
        return this.range.end;
    }
});

//> @class AceAnchor
// Ace Editor anchor.
//
// @treeLocation Client Reference/Control/AceEditor
// @visibility sgwt
//<
isc.ClassFactory.defineClass("AceAnchor");
isc.AceAnchor.addMethods({
    init : function () {
        if (!isc.AceAnchor._aceObjConstructor) {
            isc.AceAnchor._aceObjConstructor = ace.require("./anchor").Anchor;
        }

        if (!this.anchor) {
            var doc = (this.document ? this.document.getAceObj() : null);
            this.anchor = new isc.AceAnchor._aceObjConstructor(doc, this.row, this.column);
            this.anchor.scRef = this;
        }
    },

    getAceObj : function () {
        return this.anchor;
    },

    //> @method aceAnchor.getPosition
    // Returns the position of the anchor.
    //
    // @return (AcePosition) the anchor position
    // @visibility internal
    //<
    getPosition : function () {
        return this.anchor.getPosition();
    },

    //> @method aceAnchor.setPosition
    // Sets the anchor position to the specified row and column.
    // If <code>noClip</code> is <code>true</code>, the position is not clipped.
    //
    // @param row (Integer) the new row
    // @param column (Integer) the new column
    // @param noClip (Boolean) true to not clip the position
    // @visibility internal
    //<
    setPosition : function (row, column, noClip) {
        return this.anchor.setPosition(row, column, noClip);
    }
});
isc.AceAnchor.addProperties({
    //> @attr aceAnchor.document (AceDocument : null : IR)
    // The initial document of this anchor.
    //
    // @getter getDocument
    // @visibility sgwt
    //<

    //> @method aceAnchor.getDocument
    // Returns the document.
    //
    // @return (AceDocument) the document
    // @visibility internal
    //<
    getDocument : function () {
        return (this.anchor.document ? this.anchor.document.scRef : null);
    },

    //> @attr aceAnchor.row (int : null : IR)
    // The initial row coordinate of this anchor.
    //
    // @getter getRow
    // @visibility sgwt
    //<

    //> @method aceAnchor.getRow()
    // Returns the row position.
    //
    // @return (Integer) the row
    // @visibility sgwt
    //<
    getRow : function () {
        return this.getPosition().row;
    },

    //> @attr aceAnchor.column (int : null : IR)
    // The initial column coordinate of this anchor.
    //
    // @visibility sgwt
    //<

    //> @method aceAnchor.getColumn()
    // Returns the column position.
    //
    // @return (Integer) the column
    // @visibility sgwt
    //<
    getColumn : function () {
        return this.getPosition().column;
    }
});

//> @class AceMarker
// Ace Editor marker.
//
// @treeLocation Client Reference/Control/AceEditor
// @visibility sgwt
//<
isc.ClassFactory.defineClass("AceMarker");
isc.AceMarker.addMethods({
    init : function () {
        // Make sure there is a marker object
        this.marker = this.marker || {};
    },

    getAceObj : function () {
        return this.marker;
    }
});
isc.AceMarker.addProperties({
    //> @attr aceMarker.markerId    (Integer : null : R)
    // The marker id.
    //
    // @getter getMarkerId
    // @visibility internal
    //<

    //> @method aceMarker.getMarkerId()
    // Returns the marker id.
    //
    // @return (Integer) id
    // @visibility sgwt
    //<
    getMarkerId : function () {
        return (this.marker ? this.marker.id : null);
    },

    //> @attr aceMarker.range    (AceRange : null : R)
    // Marker range.
    //
    // @getter getRange
    // @visibility internal
    //<

    //> @method aceMarker.getRange()
    // Returns the marker range.
    //
    // @return (AceRange) range
    // @visibility sgwt
    //<
    getRange : function () {
        // Hold onto the wrapped range
        if (!this._range && this.marker && this.marker.range) {
            this._range = isc.AceRange.create({ range: this.marker.range });
        }
        return this._range;
    },

    //> @attr aceMarker.type    (String : null : R)
    // Marker type.
    //
    // @getter getType
    // @visibility internal
    //<

    //> @method aceMarker.getType()
    // Returns the marker type.
    //
    // @return (String) type
    // @visibility sgwt
    //<
    getType : function () {
        return (this.marker ? this.marker.type : null);
    },

    //> @attr aceMarker.style    (CSSStyleName : null : R)
    // The CSS class applied to this marker.
    //
    // @getter getStyle
    // @visibility internal
    //<

    //> @method aceMarker.getStyle()
    // Returns the marker class name.
    //
    // @return (String) class name
    // @visibility sgwt
    //<
    getStyle : function () {
        return (this.marker ? this.marker.clazz : null);
    },

    //> @attr aceMarker.inFront    (Boolean : null : R)
    // This attribute indicates the marker level: either "in front" or not.
    //
    // @getter getInFront
    // @visibility internal
    //<

    //> @method aceMarker.getInFront()
    // Returns true if marker is in front.
    //
    // @return (Boolean) in front
    // @visibility sgwt
    //<
    getInFront : function () {
        return (this.marker ? this.marker.inFront : null);
    }
});


//> @object AcePosition
// row/column position
//
// @treeLocation Client Reference/Control/AceEditor
// @visibility sgwt
//<

//> @attr acePosition.row (int : 0 : IRW)
// The row coordinate of this position.
//
// @visibility sgwt
//<
//> @attr acePosition.column (int : 0 : IRW)
// The column coordinate of this position.
//
// @visibility sgwt
//<
isc._debugModules = (isc._debugModules != null ? isc._debugModules : []);isc._debugModules.push('AceEditor');isc.checkForDebugAndNonDebugModules();isc._moduleEnd=isc._AceEditor_end=(isc.timestamp?isc.timestamp():new Date().getTime());if(isc.Log&&isc.Log.logIsInfoEnabled('loadTime'))isc.Log.logInfo('AceEditor module init time: ' + (isc._moduleEnd-isc._moduleStart) + 'ms','loadTime');delete isc.definingFramework;if (isc.Page) isc.Page.handleEvent(null, "moduleLoaded", { moduleName: 'AceEditor', loadTime: (isc._moduleEnd-isc._moduleStart)});}else{if(window.isc && isc.Log && isc.Log.logWarn)isc.Log.logWarn("Duplicate load of module 'AceEditor'.");}
/*

  SmartClient Ajax RIA system
  Version v12.0p_2019-08-11/LGPL Deployment (2019-08-11)

  Copyright 2000 and beyond Isomorphic Software, Inc. All rights reserved.
  "SmartClient" is a trademark of Isomorphic Software, Inc.

  LICENSE NOTICE
     INSTALLATION OR USE OF THIS SOFTWARE INDICATES YOUR ACCEPTANCE OF
     ISOMORPHIC SOFTWARE LICENSE TERMS. If you have received this file
     without an accompanying Isomorphic Software license file, please
     contact licensing@isomorphic.com for details. Unauthorized copying and
     use of this software is a violation of international copyright law.

  DEVELOPMENT ONLY - DO NOT DEPLOY
     This software is provided for evaluation, training, and development
     purposes only. It may include supplementary components that are not
     licensed for deployment. The separate DEPLOY package for this release
     contains SmartClient components that are licensed for deployment.

  PROPRIETARY & PROTECTED MATERIAL
     This software contains proprietary materials that are protected by
     contract and intellectual property law. You are expressly prohibited
     from attempting to reverse engineer this software or modify this
     software for human readability.

  CONTACT ISOMORPHIC
     For more information regarding license rights and restrictions, or to
     report possible license violations, please contact Isomorphic Software
     by email (licensing@isomorphic.com) or web (www.isomorphic.com).

*/

