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
	DEFAULTS("resources.implicitActionName.defaults", false),
	
	/**
	 * OK on edit view
	 */
	OK("resources.implicitActionName.ok", true),
	
	/**
	 * Save on edit view
	 */
	Save("resources.implicitActionName.save", true),
	
	/**
	 * Delete on edit view
	 */
	Delete("resources.implicitActionName.delete", true),
	
	/**
	 * Add on child edit view
	 */
	Add("resources.implicitActionName.add", false),
	
	/**
	 * Change on child edit view
	 */
	ZoomOut("resources.implicitActionName.zoomOut", true),
	
	/**
	 * Cancel on edit view and child edit view
	 */
	Cancel("resources.implicitActionName.cancel", false),
	
	/**
	 * Remove on child edit view
	 */
	Remove("resources.implicitActionName.remove", false),
	
	/**
	 * Add new item in list view
	 */
	New("resources.implicitActionName.new", false),
	
	/**
	 * Edit an item in list view
	 */
	Edit("resources.implicitActionName.edit", true),
	
	/**
	 * Fire up a report dialog from a button
	 */
	Report("resources.implicitActionName.report", false),
	
	/**
	 * Navigate to a binding within a conversation
	 */
	Navigate("resources.implicitActionName.navigate", true),
	
	/**
	 * Import data using the BizPort capability
	 */
	BizImport("resources.implicitActionName.import", true),
	
	/**
	 * Export data using the BizPort capability
	 */
	BizExport("resources.implicitActionName.export", true),
	
	/**
	 * Create and stream a file for Download
	 */
	Download("resources.implicitActionName.download", true),

	/**
	 * Upload and process a file
	 */
	Upload("resources.implicitActionName.upload", true),

	/**
	 * Prints the current view
	 */
	Print("resources.implicitActionName.print", true);
	
	private String displayName;
	private boolean validatable;

	/**
	 * 
	 * @param displayName
	 */
	private ImplicitActionName(String displayName, boolean validatable) {
		this.displayName = displayName;
		this.validatable = validatable;
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
	
	/**
	 * Indicates if this action type is validatable or not (has the clientValidation XML attribute)
	 * @return
	 */
	public boolean isValidatable() {
		return validatable;
	}
}
