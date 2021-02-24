package org.skyve.impl.content;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.util.logging.Level;

import org.pf4j.Extension;
import org.apache.tika.Tika;
import org.apache.tika.language.detect.LanguageDetector;
import org.apache.tika.language.detect.LanguageResult;
import org.apache.tika.metadata.Metadata;
import org.apache.tika.metadata.Office;
import org.apache.tika.metadata.TikaCoreProperties;
import org.skyve.content.AttachmentContent;
import org.skyve.content.TextExtractor;
import org.skyve.impl.util.UtilImpl;

@Extension(points = {TextExtractor.class})
public class TextExtractorImpl implements TextExtractor {
	private static final Tika TIKA = new Tika();
	
	@Override
	public String extractTextFromMarkup(String markup) {
		String result = null;
		String processedMarkup = UtilImpl.processStringValue(markup);
		if (processedMarkup != null) {
			try {
				result = UtilImpl.processStringValue(TIKA.parseToString(new ByteArrayInputStream(processedMarkup.getBytes())));
			}
			catch (Exception e) {
				UtilImpl.LOGGER.log(Level.SEVERE, 
										"TextExtractorImpl.extractTextFromMarkup(): Markup could not be extracted by TIKA",
										e);
			}
		}
		return result;
	}
	
	@Override
	public String extractTextFromContent(AttachmentContent content) {
		StringBuilder result = new StringBuilder(102400);

		try {
			try (InputStream contentStream = content.getContentStream()) {
				Metadata metadata = new Metadata();

				// Set the maximum length of strings returned by the parseToString method, -1 sets no limit
				String text = UtilImpl.processStringValue(TIKA.parseToString(contentStream, metadata, 100000));
				if (text != null) {
					result.append(text);
				}
				
				// Meta data
				// Title
				String title = metadata.get(TikaCoreProperties.TITLE);
				if (title != null) {
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
		}
		catch (Exception e) {
			UtilImpl.LOGGER.log(Level.SEVERE, 
									"TextExtractorImpl.extractTextFromContent(): Attachment could not be extracted by TIKA",
									e);
		}
		
		return (result.length() == 0) ? null : result.toString();
	}
	
	@Override
	public void sniffContentType(AttachmentContent attachment) {
		// Sniff content type if necessary
		String contentType = attachment.getContentType();
		if (contentType == null) {
			try {
				byte[] content = attachment.getContentBytes();
				String fileName = attachment.getFileName();
				if (content != null) {
					if (fileName == null) {
						contentType = TIKA.detect(content);
					}
					else {
						contentType = TIKA.detect(content, fileName);
					}
					attachment.setContentType(contentType);
				}
			}
			catch (Exception e) {
				UtilImpl.LOGGER.log(Level.SEVERE, 
										"TextExtractorImpl.sniffContentType(): Attachment could not be sniffed by TIKA",
										e);
			}
		}
	}
	
	@Override
	public String sniffLanguage(String text) {
		LanguageResult result = LanguageDetector.getDefaultLanguageDetector().detect(text);
		if (result != null) {
			return result.getLanguage();
		}
		return null;
	}
}
