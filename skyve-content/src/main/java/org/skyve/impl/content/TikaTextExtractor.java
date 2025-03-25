package org.skyve.impl.content;

import java.io.ByteArrayInputStream;
import java.io.InputStream;

import org.apache.tika.Tika;
import org.apache.tika.language.detect.LanguageDetector;
import org.apache.tika.language.detect.LanguageResult;
import org.apache.tika.metadata.Metadata;
import org.apache.tika.metadata.Office;
import org.apache.tika.metadata.TikaCoreProperties;
import org.pf4j.Extension;
import org.skyve.content.AttachmentContent;
import org.skyve.content.TextExtractor;
import org.skyve.impl.util.UtilImpl;
import org.skyve.util.Util;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@Extension(points = {TextExtractor.class})
public class TikaTextExtractor implements TextExtractor {

    private static final Logger LOGGER = LoggerFactory.getLogger(TikaTextExtractor.class);

	private static final Tika TIKA = new Tika();
	
	@Override
	public String extractTextFromMarkup(String markup) {
		String result = null;
		String processedMarkup = UtilImpl.processStringValue(markup);
		if (processedMarkup != null) {
			try {
				result = UtilImpl.processStringValue(TIKA.parseToString(new ByteArrayInputStream(processedMarkup.getBytes())));
			}
			catch (Throwable t) { // include Errors like NoClassDefFound
				LOGGER.error("TextExtractorImpl.extractTextFromMarkup(): Markup could not be extracted by TIKA", t);
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
				// Subject and keywords (if present)
				String subject = metadata.get(TikaCoreProperties.SUBJECT);
				if (subject != null) {
					if (result.length() > 0) {
						result.append(". ");
					}
					result.append(subject);
				}
			}
			
			// Any markup text nodes
			String markup = content.getMarkup();
			if (markup != null) {
				try (InputStream markupStream = new ByteArrayInputStream(markup.getBytes(Util.UTF8))) {
					markup = UtilImpl.processStringValue(TIKA.parseToString(markupStream, new Metadata(), 100000));
					if (markup != null) {
						if (result.length() > 0) {
							result.append(". ");
						}
						result.append(markup);
					}
				}
			}
		}
		catch (Throwable t) { // include Errors like NoClassDefFound
			LOGGER.error("TextExtractorImpl.extractTextFromContent(): Attachment could not be extracted by TIKA", t);
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
			catch (Throwable t) { // include Errors like NoClassDefFound
				LOGGER.error("TextExtractorImpl.sniffContentType(): Attachment could not be sniffed by TIKA", t);
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
