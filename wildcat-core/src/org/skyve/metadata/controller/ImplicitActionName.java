package org.skyve.metadata.controller;

import javax.xml.bind.annotation.XmlType;

import org.skyve.wildcat.util.XMLUtil;

/**
 * Actions implicit to WILDCAT.
 */
@XmlType(namespace = XMLUtil.VIEW_NAMESPACE)
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
	 * Upload and process a file
	 */
	Upload("Upload");
	
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
}
