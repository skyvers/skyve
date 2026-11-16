package org.skyve.impl.content;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringReader;
import java.nio.charset.StandardCharsets;

import javax.xml.parsers.ParserConfigurationException;

import org.apache.tika.Tika;
import org.apache.tika.exception.TikaException;
import org.apache.tika.language.detect.LanguageDetector;
import org.apache.tika.language.detect.LanguageResult;
import org.apache.tika.metadata.Metadata;
import org.apache.tika.metadata.Office;
import org.apache.tika.metadata.TikaCoreProperties;
import org.dom4j.Document;
import org.dom4j.DocumentException;
import org.dom4j.Element;
import org.pf4j.Extension;
import org.skyve.content.AttachmentContent;
import org.skyve.content.TextExtractor;
import org.skyve.impl.util.SecureDom4j;
import org.skyve.impl.util.UtilImpl;
import org.skyve.util.logging.SkyveLoggerFactory;
import org.slf4j.Logger;
import org.xml.sax.SAXException;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Extracts searchable text and metadata from markup and binary attachments using Apache Tika.
 *
 * <p>For SVG overlays on attachments, only text contained in SVG {@code text} elements is
 * searchable. Styling, metadata, embedded editor payloads, and graphical element attributes are
 * excluded. Text and selected metadata extracted from the underlying attachment remain searchable.
 * Non-SVG markup retains Tika's general-purpose extraction behaviour.
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
	@SuppressWarnings("java:S1181") // include Errors like NoClassDefFound
	public @Nullable String extractTextFromMarkup(@Nullable String markup) {
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
	@SuppressWarnings({"java:S3776", "java:S1181"}) // include Errors like NoClassDefFound
	public @Nullable String extractTextFromContent(@Nonnull AttachmentContent content) {
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
			String markup = UtilImpl.processStringValue(content.getMarkup());
			if (markup != null) {
				markup = extractTextFromAttachmentMarkup(markup);
				if (markup != null) {
					if (! result.isEmpty()) {
						result.append(METADATA_SEPARATOR);
					}
					result.append(markup);
				}
			}
		}
		catch (Throwable t) { // include Errors like NoClassDefFound
			LOGGER.error("TextExtractorImpl.extractTextFromContent(): Attachment could not be extracted by TIKA", t);
		}
		
		return result.isEmpty() ? null : result.toString();
	}

	private static @Nullable String extractTextFromAttachmentMarkup(@Nonnull String markup)
	throws SAXException, ParserConfigurationException, IOException, TikaException {
		try {
			Document document = SecureDom4j.newSAXReader().read(new StringReader(markup));
			Element root = document.getRootElement();
			if ("svg".equals(root.getName())) {
				StringBuilder result = new StringBuilder(256);
				appendSvgText(root, result);
				return UtilImpl.processStringValue(result.toString());
			}
		}
		catch (@SuppressWarnings("unused") DocumentException e) {
			// Tika also supports non-well-formed markup, so preserve its existing fallback behaviour.
		}

		try (InputStream markupStream = new ByteArrayInputStream(markup.getBytes(StandardCharsets.UTF_8))) {
			return UtilImpl.processStringValue(TIKA.parseToString(markupStream, new Metadata(), 100000));
		}
	}

	private static void appendSvgText(@Nonnull Element element, @Nonnull StringBuilder result) {
		if ("text".equals(element.getName())) {
			String text = UtilImpl.processStringValue(element.getStringValue());
			if (text != null) {
				if (! result.isEmpty()) {
					result.append(METADATA_SEPARATOR);
				}
				result.append(text);
			}
			return;
		}

		for (Element child : element.elements()) {
			appendSvgText(child, result);
		}
	}
	
	/**
	 * Detects and sets the attachment MIME type when one has not already been supplied.
	 *
	 * @param attachment the attachment whose content type should be detected
	 */
	@Override
	@SuppressWarnings("java:S1181") // include Errors like NoClassDefFound
	public void sniffContentType(@Nonnull AttachmentContent attachment) {
		// Sniff content type if necessary
		String contentType = attachment.getContentType();
		if (contentType == null) {
			try (InputStream content = attachment.getContentStream()) {
				String fileName = attachment.getFileName();
				if (fileName == null) {
					contentType = TIKA.detect(content);
				}
				else {
					contentType = TIKA.detect(content, fileName);
				}
				attachment.setContentType(contentType);
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
	@SuppressWarnings("java:S1181") // include Errors like NoClassDefFound
	public @Nullable String sniffLanguage(@Nullable String text) {
		if (text == null || text.isBlank()) {
			return null;
		}
		try {
			LanguageResult result = LanguageDetector.getDefaultLanguageDetector().detect(text);
			if (result != null) {
				String lang = result.getLanguage();
				if (lang != null && !lang.equalsIgnoreCase("unknown")) {
					return lang;
				}
			}
		}
		catch (Throwable t) { // include Errors like NoClassDefFound
			LOGGER.error("TextExtractorImpl.sniffLanguage(): Language could not be detected by TIKA", t);
		}
		return null;
	}
}
