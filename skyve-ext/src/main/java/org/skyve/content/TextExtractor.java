package org.skyve.content;

import org.pf4j.ExtensionPoint;

public interface TextExtractor extends ExtensionPoint {
	public String extractTextFromMarkup(String markup);
	public String extractTextFromContent(AttachmentContent content);
	public void sniffContentType(AttachmentContent attachment);
}
