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
	DEFAULTS("Defaults"),
	
	/**
	 * OK on edit view
	 */
	OK("OK"),
	
	/**
	 * Save on edit view
	 */
	Save("Save"),
	
	/**
	 * Delete on edit view
	 */
	Delete("Delete"),
	
	/**
	 * Add on child edit view
	 */
	Add("Add"),
	
	/**
	 * Change on child edit view
	 */
	ZoomOut("Zoom Out"),
	
	/**
	 * Cancel on edit view and child edit view
	 */
	Cancel("Cancel"),
	
	/**
	 * Remove on child edit view
	 */
	Remove("Remove"),
	
	/**
	 * Add new item in list view
	 */
	New("New"),
	
	/**
	 * Edit an item in list view
	 */
	Edit("Edit"),
	
	/**
	 * Fire up a report dialog from a button
	 */
	Report("Report"),
	
	/**
	 * Navigate to a binding within a conversation
	 */
	Navigate("Navigate"),
	
	/**
	 * Import data using the BizPort capability
	 */
	BizImport("Import"),
	
	/**
	 * Export data using the BizPort capability
	 */
	BizExport("Export"),
	
	/**
	 * Create and stream a file for Download
	 */
	Download("Download"),

	/**
	 * Upload and process a file
	 */
	Upload("Upload"),

	/**
	 * Prints the current view
	 */
	Print("Print");
	
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
