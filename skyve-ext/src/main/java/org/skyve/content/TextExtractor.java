package org.skyve.content;

import org.pf4j.ExtensionPoint;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

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
	public @Nullable String extractTextFromMarkup(@Nullable String markup);
	
	/**
	 * Extract text from various content types like PDFs and office documents etc.
	 * @param content	The attachment
	 * @return	Plain text
	 */
	public @Nullable String extractTextFromContent(@Nonnull AttachmentContent content);
	
	/**
	 * Determine and populate AttachmentContent.contentType.
	 * @param attachment	To populate
	 */
	public void sniffContentType(@Nonnull AttachmentContent attachment);
	
	/**
	 * The ISO 639-1 language code (plus optional country code) or null if nothing is detected
	 */
	public @Nullable String sniffLanguage(@Nullable String text);
}
