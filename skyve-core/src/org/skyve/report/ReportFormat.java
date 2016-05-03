package org.skyve.report;

import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.XMLUtil;

/**
 * 
 */
@XmlType(namespace = XMLUtil.COMMON_NAMESPACE)
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
	 * schema validated HTML
	 */
	xhtml,
	
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
	 * jrxml format 
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
