package org.skyve.impl.content;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;

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
import org.slf4j.Logger;
import org.skyve.util.logging.SkyveLoggerFactory;

/**
 * Extracts searchable text and metadata from markup and binary attachments using Apache Tika.
 *
 * <p>Threading: thread-safe for current usage because it maintains no mutable instance state.
 */
@Extension(points = {TextExtractor.class})
public class TikaTextExtractor implements TextExtractor {
    private static final Logger LOGGER = SkyveLoggerFactory.getLogger(TikaTextExtractor.class);

    private static final String METADATA_SEPARATOR = ". ";

	private static final Tika TIKA = new Tika();
	
	/**
	 * Extracts plain text from a markup fragment.
	 *
	 * @param markup the markup to parse
	 * @return normalised text, or {@code null} when no meaningful text is available
	 */
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
	
	/**
	 * Extracts searchable text and selected metadata from attachment content.
	 *
	 * <p>Side effects: reads attachment streams and logs extraction failures.
	 *
	 * @param content the attachment to parse
	 * @return extracted text, or {@code null} when extraction yields no content
	 */
	@Override
	@SuppressWarnings("java:S3776")
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
					if (! result.isEmpty()) {
						result.append(METADATA_SEPARATOR);
					}
					result.append(title);
				}
				// Author
				String author = metadata.get(Office.AUTHOR);
				if (author != null) {
					if (! result.isEmpty()) {
						result.append(METADATA_SEPARATOR);
					}
					result.append(author);
				}
				// Subject and keywords (if present)
				String subject = metadata.get(TikaCoreProperties.SUBJECT);
				if (subject != null) {
					if (! result.isEmpty()) {
						result.append(METADATA_SEPARATOR);
					}
					result.append(subject);
				}
			}
			
			// Any markup text nodes
			String markup = content.getMarkup();
			if (markup != null) {
				try (InputStream markupStream = new ByteArrayInputStream(markup.getBytes(StandardCharsets.UTF_8))) {
					markup = UtilImpl.processStringValue(TIKA.parseToString(markupStream, new Metadata(), 100000));
					if (markup != null) {
						if (! result.isEmpty()) {
							result.append(METADATA_SEPARATOR);
						}
						result.append(markup);
					}
				}
			}
		}
		catch (Throwable t) { // include Errors like NoClassDefFound
			LOGGER.error("TextExtractorImpl.extractTextFromContent(): Attachment could not be extracted by TIKA", t);
		}
		
		return result.isEmpty() ? null : result.toString();
	}
	
	/**
	 * Detects and sets the attachment MIME type when one has not already been supplied.
	 *
	 * @param attachment the attachment whose content type should be detected
	 */
	@Override
	public void sniffContentType(AttachmentContent attachment) {
		// Sniff content type if necessary
		String contentType = attachment.getContentType();
		if (contentType == null) {
			try (InputStream content = attachment.getContentStream()) {
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
	
	/**
	 * Detects the language code for the supplied text.
	 *
	 * @param text the text to classify
	 * @return the detected language code, or {@code null} when no language can be inferred
	 */
	@Override
	public String sniffLanguage(String text) {
		LanguageResult result = LanguageDetector.getDefaultLanguageDetector().detect(text);
		if (result != null) {
			return result.getLanguage();
		}
		return null;
	}
}
