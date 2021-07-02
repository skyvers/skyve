package org.skyve.content;

import org.pf4j.ExtensionPoint;

/**
 * The interface for any class implementing a plugable text extractor.
 * This class extends ExtensionPoint so that it can be packaged as a plug in.
 * @author mike
 */
public interface TextExtractor extends ExtensionPoint {
	/**
	 * Extract text from various SGML markup languages derivatives like XML or HTML etc.
	 * @param markup	The markup
	 * @return Plain text
	 */
	public String extractTextFromMarkup(String markup);
	
	/**
	 * Extract text from various content types like PDFs and office documents etc.
	 * @param content	The attachment
	 * @return	Plain text
	 */
	public String extractTextFromContent(AttachmentContent content);
	
	/**
	 * Determine and populate AttachmentContent.contentType.
	 * @param attachment	To populate
	 */
	public void sniffContentType(AttachmentContent attachment);
	
	/**
	 * The ISO 639-1 language code (plus optional country code) or null if nothing is detected
	 */
	public String sniffLanguage(String text);
}
