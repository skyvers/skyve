package org.skyve.report;

import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlType;

/**
 * 
 */
@XmlType(namespace = XMLMetaData.COMMON_NAMESPACE)
public enum ReportFormat {
	/**
	 * comma separated values
	 */
	csv,
	
	/**
	 * doc type HTML
	 */
	html,
	
	/**
	 * Adobe Portable Document Format
	 */
	pdf,
	
	/**
	 * excel <= 2003
	 */
	xls, 
	
	/**
	 * Mircosoft Rich Text Format
	 */
	rtf, 
	
	/**
	 * jrpxml format 
	 */
	xml,
	
	/**
	 * open document format
	 */
	odt,
	
	/**
	 * open document spreadsheet format
	 */
	ods,
	
	/**
	 * word doc >= 2007
	 */
	docx,
	
	/**
	 * excel >= 2007
	 */
	xlsx,
	
	/**
	 * powerpoint >= 2007
	 */
	pptx,
	
	/**
	 * text
	 */
	txt
}
