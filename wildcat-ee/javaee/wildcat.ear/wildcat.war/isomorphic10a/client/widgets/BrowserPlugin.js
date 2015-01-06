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

 




//>	@class	BrowserPlugin
//  
//  Container for a Browser Plugin.
//
//  @treeLocation Client Reference/Client Bridges
//  @requiresModules PluginBridges
//  @visibility PluginBridges
//<



isc.ClassFactory.defineClass("BrowserPlugin", "Canvas");

isc.BrowserPlugin.addClassProperties({
    instances: [], // all instances of browserPlugin

    // future extension point - called by EH handleDragMove.  
    handleDragMoveNotify : function () { }
});

isc.BrowserPlugin.addProperties({
    // the plugin src
    src: "",
    extraHTML: "",       // arbitrary additional html (added to the embed tag)
    installPlugin: true, // trigger the plugin auto-installer if supported?
    // avoid automatic redrawing, which would recreate the plugin instance.  We don't want to do
    // this for any automatic reason; only if redraw() is explicitly called.
    redrawOnResize:false,
    _redrawWithMaster:false,
    _redrawWithParent:false,

    // This flag controls whether we register the component as a maskable item with the
    // EventHandler (ultimately controls whether _showDragMask()/_hideDragMask() get called on
    // this component on dragStart.
    useDragMask: true,

    dragMaskType: "hidePlugin",

    // A placeholder dragMask means that we hide the plugin and show a "Dragging..." or similar
    // Label placeholder.  This is appropriate for plugins where no masking mechanism exists to
    // allow us to capture events.
    //
    // Moz Plugins cause uncorrectable burnthrough, but events can be captured with a standard
    // dragMask.  This setting is a tradeoff on Moz between hiding the plugin and the ability
    // to see what you're dragging over it
    usePlaceholderDragMask: !isc.Browser.isMoz,

initWidget : function () {
    isc.BrowserPlugin.instances.add(this);
},

destroy : function () {
    isc.BrowserPlugin.instances.remove(this);
    this.Super("destroy", arguments);
},

draw : function () {
    this.Super("draw", arguments);

    // if the backmask that we typically use via useBackMask will burn through this component,
    // then it we should disable it for any ancestors, otherwise this component won't show up.
    // The correct solution to this is too complex to implement at the moment - need to
    // disable/re-enable the backmask based on this component being cleared, reparented,
    // destroyed, etc and we don't want to impact the critical path code in Canvas.draw() to
    // check any children.
    //
    // For now, we simply solve the typical case of a plugin that gets burned through by a
    // backmask being placed inside a component that sets useBackMask: true by disabling all
    // such masks up the parent chain and making no effort to re-enable them.
    if (this.backMaskCausesBurnThrough) {
        var applet = this;
        this.getParentElements().map(function (ancestor) {
            if (ancestor.useBackMask) {
                applet.logInfo("Suppressing backmask of ancestor: " + ancestor.getID());
                if (ancestor._backMask) {
                    // backmask exists, suppress and hide it
                    ancestor._backMask.suppressed = true;
                    ancestor._backMask.hide();
                } else {
                    // backmask will exist after onload, suppress
                    if (!ancestor._deferredBackMaskProps) ancestor._deferredBackMaskProps={};
                    ancestor._deferredBackMaskProps.suppressed = true;
                }
            }
        });
    }
}

});
