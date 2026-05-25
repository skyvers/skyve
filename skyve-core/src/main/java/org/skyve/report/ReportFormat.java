package org.skyve.report;

import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlType;

/**
 * Output formats supported by the Skyve reporting engine.
 *
 * <p>Each constant corresponds to a file format that JasperReports (and the
 * {@link org.skyve.report.ReportService} abstraction) can produce. The format
 * determines both the MIME type of the HTTP response and the file extension of
 * any download offered to the user.
 *
 * <p>Typical usage: pass a format constant to
 * {@link org.skyve.report.ReportService} methods to control the output type.
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
