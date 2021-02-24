package org.skyve.content;

import org.pf4j.ExtensionPoint;

public interface TextExtractor extends ExtensionPoint {
	public String extractTextFromMarkup(String markup);
	public String extractTextFromContent(AttachmentContent content);
	public void sniffContentType(AttachmentContent attachment);
	/**
	 * The ISO 639-1 language code (plus optional country code) or null if nothing is detected
	 */
	public String sniffLanguage(String text);
}
