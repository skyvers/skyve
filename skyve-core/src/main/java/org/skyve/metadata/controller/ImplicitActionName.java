package org.skyve.metadata.controller;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.util.Util;

import jakarta.xml.bind.annotation.XmlType;

/**
 * Actions implicit to skyve.
 */
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
@SuppressWarnings("java:S115") // Suppress "Constant names should comply with a naming convention" as these are not constants but enum values
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
	 * Initialises an implicit action constant with its i18n display name key and validatability flag.
	 *
	 * @param displayName   the i18n resource key for the human-readable action name
	 * @param validatable   {@code true} if the action supports a {@code clientValidation} attribute in view XML
	 */
	private ImplicitActionName(String displayName, boolean validatable) {
		this.displayName = displayName;
		this.validatable = validatable;
	}

	/**
	 * Returns the i18n resource key for the human-readable display name of this action.
	 *
	 * @return the i18n key; never {@code null}
	 * @see #getLocalisedDisplayName()
	 */
	public String getDisplayName() {
		return displayName;
	}
	
	/**
	 * Returns the localisedDisplayName.
	 * @return the result
	 */
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
