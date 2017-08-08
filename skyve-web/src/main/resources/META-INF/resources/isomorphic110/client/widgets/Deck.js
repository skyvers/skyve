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
//> @class Deck
// A simple container that implements the policy that at most one of its contained components
// is visible at any given time.
// <p>
// The set of mutually exclusive components is specified by +link{deck.panes}, and whichever
// component is visible fills the space of the <code>Deck</code> automatically.
// <p>
// To switch to a new pane, call +link{deck.setCurrentPane()}, or simply call
// +link{canvas.show,show()} on the pane directly - the <code>Deck</code> will notice that you
// have shown a different pane and hide other panes automatically.
// <p>
// +link{Canvas.children,Deck.children} may also be used; any components that are specified as children are
// unmanaged by the <code>Deck</code> and so can place themselves arbitrarily.
// <p>
// <code>Deck</code> achieves its mutually-exclusive display behavior by using the superclass
// +link{Layout.members} property, which means that properties such as +link{layout.layoutMargin}
// and +link{layout.vPolicy} do apply to deck.  However, trying to manipulate
// <code>deck.members</code> with APIs such as +link{layout.addMember()} is not supported and
// will have undefined results.
// @treeLocation Client Reference/Layout
// @visibility external
//<

isc.ClassFactory.defineClass("Deck", "Layout"); 

// add default properties
isc.Deck.addProperties({

    _dontCopyChildrenToMembers: true,

    //> @attr deck.panes (Array of Canvas : null : IRW)
    // Set of mutually exclusive panes displayed in this <code>Deck</code>.
    // <p>
    // If +link{Deck.currentPane} is not set, when the <code>Deck</code> is first drawn, the
    // first pane in this array becomes the <code>currentPane</code>.
    // @visibility external
    //<

    //> @attr deck.currentPane (Canvas : null : IRW)
    // The pane currently shown in this <code>Deck</code>.  All other panes are hidden.
    // @visibility external
    //<

    //> @method deck.setCurrentPane()
    // Change the +link{currentPane}.
    // <p>
    // If the passed pane is not contained in this <code>Deck</code>, logs a warning and does
    // nothing.
    // @param pane (identifier | Canvas) the pane to show, as either a <code>Canvas</code> or
    // the +link{Canvas.ID}
    // @visibility external
    //<
    setCurrentPane : function (pane) {
        if (this.currentPane != null && (this.currentPane === pane || this.currentPane.ID == pane)) {
            
            return;
        }
        var paneFound = false,
            panes = this.panes;
        for (var i = 0, numPanes = panes.length; i < numPanes; ++i) {
            if (panes[i] === pane || panes[i].ID == pane) {
                pane = panes[i];
                

                pane.setVisibility(isc.Canvas.INHERIT);
                

                paneFound = true;
                break;
            }
        }
        if (!paneFound) {
            this.logWarn("setCurrentPane() failed: pane " + (isc.isA.Canvas(pane) ? pane.getID() : pane) + " was not found in the Deck.");
        }
    },

    //> @method deck.hideCurrentPane()
    // Hides the current pane, without showing any other pane.
    // @visibility external
    //<
    hideCurrentPane : function () {
        if (this.currentPane != null) this.currentPane.setVisibility(isc.Canvas.HIDDEN);
        
    },

    setPanes : function (panes) {
        if (panes == null) panes = [];
        else {
            var currentPane = this.currentPane;
            for (var i = 0, numPanes = panes.length; i < numPanes; ++i) {
                var pane = panes[i];
                pane.setVisibility(pane === currentPane ? isc.Canvas.INHERIT : isc.Canvas.HIDDEN);
            }
        }
        this.panes = panes;

        this.setMembers(panes);

        
    },

    // this method is used by VisualBuilder to add panes, so we should set the first pane to
    // currentPane automatically and show it
    addPane : function (pane, index) {
        if (pane == null) return;

        var existingIndex = this.panes.indexOf(pane);
        if (existingIndex >= 0) {
            var newPosition = index == null ? this.panes.length : index;
            this.panes.slideRange(existingIndex, existingIndex + 1, newPosition);
            this.reorderMembers(existingIndex, existingIndex + 1, newPosition);
        } else {
            if (index == null) {
                this.panes.add(pane);
            } else {
                this.panes.addAt(pane, index);
            }
            pane.setVisibility(isc.Canvas.HIDDEN);
            this.addMember(pane, index);
        }
    },

    removePane : function (pane) {
        if (pane == null) return;

        this.panes.remove(pane);
        this.removeMember(pane);
        
    },

    initWidget : function () {
        this.Super("initWidget", arguments);
        if (this.currentPane != null && this.panes != null && !this.panes.contains(this.currentPane)) {
            this.currentPane = null;
        }
        this.setPanes(this.panes);
    },

    childVisibilityChanged : function (child, newVisibility) {
        if (this.panes.contains(child)) this.paneVisibilityChanged(child, newVisibility);
        this.Super("childVisibilityChanged", arguments);
    },

    paneVisibilityChanged : function (pane, newVisibility) {
        if (newVisibility === isc.Canvas.HIDDEN) {
            if (pane === this.currentPane) {
                this.currentPane = null;
                if (this.currentPaneChanged != null) this.currentPaneChanged(this.currentPane);
            }
        } else {
            var currentPane = this.currentPane;
            if (currentPane == null || pane !== currentPane) {
                this.currentPane = pane;
                if (currentPane != null) {
                    currentPane.setVisibility(isc.Canvas.HIDDEN);
                }
                if (this.currentPaneChanged != null) this.currentPaneChanged(this.currentPane);
            }
        }
    },

    draw : function () {
        var undef;
        if (!this._notFirstDraw && this.currentPane === undef && this.panes.length > 0) {
            this.setCurrentPane(this.panes[0]);
            this._notFirstDraw = true;
        }
        this.Super("draw", arguments);
    },

    childRemoved : function (child, name) {
        this.panes.remove(child);
        if (child === this.currentPane) {
            this.currentPane = null;
            if (this.panes.length > 0) this.panes[0].setVisibility(isc.Canvas.INHERIT);
            else if (this.currentPaneChanged != null) this.currentPaneChanged(this.currentPane);
        }
        
    }
});

isc.Deck.registerStringMethods({
    //> @method deck.currentPaneChanged()
    // Notification fired when the <code>Deck</code>'s +link{Deck.currentPane,currentPane} is
    // changed.
    // @param currentPane (Canvas) the new <code>currentPane</code>, or null if no pane is
    // currently visible.
    //<
    currentPaneChanged : "currentPane"
});
