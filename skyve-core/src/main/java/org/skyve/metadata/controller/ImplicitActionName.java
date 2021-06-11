package org.skyve.metadata.controller;

import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.util.Util;

/**
 * Actions implicit to skyve.
 */
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
public enum ImplicitActionName {
	/**
	 * Produce default button for the view type
	 */
	DEFAULTS("resources.implicitActionName.defaults"),
	
	/**
	 * OK on edit view
	 */
	OK("resources.implicitActionName.ok"),
	
	/**
	 * Save on edit view
	 */
	Save("resources.implicitActionName.save"),
	
	/**
	 * Delete on edit view
	 */
	Delete("resources.implicitActionName.delete"),
	
	/**
	 * Add on child edit view
	 */
	Add("resources.implicitActionName.add"),
	
	/**
	 * Change on child edit view
	 */
	ZoomOut("resources.implicitActionName.zoomOut"),
	
	/**
	 * Cancel on edit view and child edit view
	 */
	Cancel("resources.implicitActionName.cancel"),
	
	/**
	 * Remove on child edit view
	 */
	Remove("resources.implicitActionName.remove"),
	
	/**
	 * Add new item in list view
	 */
	New("resources.implicitActionName.new"),
	
	/**
	 * Edit an item in list view
	 */
	Edit("resources.implicitActionName.edit"),
	
	/**
	 * Fire up a report dialog from a button
	 */
	Report("resources.implicitActionName.report"),
	
	/**
	 * Navigate to a binding within a conversation
	 */
	Navigate("resources.implicitActionName.navigate"),
	
	/**
	 * Import data using the BizPort capability
	 */
	BizImport("resources.implicitActionName.import"),
	
	/**
	 * Export data using the BizPort capability
	 */
	BizExport("resources.implicitActionName.export"),
	
	/**
	 * Create and stream a file for Download
	 */
	Download("resources.implicitActionName.download"),

	/**
	 * Upload and process a file
	 */
	Upload("resources.implicitActionName.upload"),

	/**
	 * Prints the current view
	 */
	Print("resources.implicitActionName.print");
	
	private String displayName;

	/**
	 * 
	 * @param displayName
	 */
	private ImplicitActionName(String displayName) {
		this.displayName = displayName;
	}

	/**
	 * 
	 * @return
	 */
	public String getDisplayName() {
		return displayName;
	}
	
	public String getLocalisedDisplayName() {
		return Util.i18n(displayName);
	}
}
