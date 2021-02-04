package org.skyve.impl.content;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.util.logging.Level;

import org.pf4j.Extension;
import org.apache.tika.Tika;
import org.apache.tika.exception.TikaException;
import org.apache.tika.metadata.Metadata;
import org.apache.tika.metadata.Office;
import org.apache.tika.metadata.TikaCoreProperties;
import org.skyve.content.AttachmentContent;
import org.skyve.content.TextExtractor;
import org.skyve.impl.util.UtilImpl;
import org.skyve.util.Util;

@Extension(points = {TextExtractor.class})
public class TextExtractorImpl implements TextExtractor {
	private static final Tika TIKA = new Tika();
	
	@Override
	public String extractTextFromMarkup(String markup) throws Exception {
		return TIKA.parseToString(new ByteArrayInputStream(markup.getBytes()));
	}
	
	@Override
	public String extractTextFromContent(AttachmentContent content) throws Exception {
		StringBuilder result = new StringBuilder(102400);

		try (InputStream contentStream = content.getContentStream()) {
			Metadata metadata = new Metadata();
			try {
				// Set the maximum length of strings returned by the parseToString method, -1 sets no limit
				String text = Util.processStringValue(TIKA.parseToString(contentStream, metadata, 100000));
				if (text != null) {
					result.append(text);
				}
				
				// Meta data
				// Title
				String title = metadata.get(TikaCoreProperties.TITLE);
				if (title == null) {
					if (result.length() > 0) {
						result.append(". ");
					}
					result.append(title);
				}
				// Author
				String author = metadata.get(Office.AUTHOR);
				if (author != null) {
					if (result.length() > 0) {
						result.append(". ");
					}
					result.append(author);
				}
				// Keywords
				String keywords = metadata.get(TikaCoreProperties.KEYWORDS);
				if (keywords != null) {
					if (result.length() > 0) {
						result.append(". ");
					}
					result.append(keywords);
				}
			}
			catch (TikaException e) {
				UtilImpl.LOGGER.log(Level.SEVERE, 
										"ElasticContentManager.put(): Attachment could not be parsed by TIKA and so has not been textually indexed",
										e);
			}
		}
		
		return result.toString();
	}
	
	@Override
	public void sniffContentType(AttachmentContent attachment) throws Exception {
		// Sniff content type if necessary
		String contentType = attachment.getContentType();
		if (contentType == null) {
			byte[] content = attachment.getContentBytes();
			String fileName = attachment.getFileName();
			if (fileName == null) {
				contentType = TIKA.detect(content);
			}
			else {
				contentType = TIKA.detect(content, fileName);
			}
			attachment.setContentType(contentType);
		}
	}
	
	
}
