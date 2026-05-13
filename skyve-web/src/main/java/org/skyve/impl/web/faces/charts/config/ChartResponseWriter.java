package org.skyve.impl.web.faces.charts.config;

import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
import java.nio.charset.StandardCharsets;

import jakarta.faces.component.UIComponent;
import jakarta.faces.context.ResponseWriter;

/**
 * ResponseWriter implementation used for chart components that emit JSON.
 * <p>
 * This writer captures raw text output (expected to be JSON) into an internal
 * {@link StringWriter}, while all element/attribute-related methods are treated
 * as no-ops. The content type is reported as {@code application/json} and the
 * character encoding as UTF-8.
 * </p>
 * <p>
 * Typical usage is to write JSON directly via {@link #writeText(Object, String)}
 * or {@link #write(char[], int, int)} and then retrieve the accumulated output
 * using {@link #getStringWriter()}.
 * </p>
 */
public class ChartResponseWriter extends ResponseWriter {
	private StringWriter writer = new StringWriter(2048);
	
	/**
	 * Returns the content type for chart responses.
	 *
	 * @return {@code "application/json"}
	 */
	@Override
	public String getContentType() {
		return "application/json";
	}

	/**
	 * Returns the character encoding used by this writer.
	 *
	 * @return UTF-8 name as a string
	 */
	@Override
	public String getCharacterEncoding() {
		return StandardCharsets.UTF_8.name();
	}

	/**
	 * Flushes the underlying {@link StringWriter} buffer.
	 *
	 * @throws IOException if an I/O error occurs
	 */
	@Override
	public void flush() throws IOException {
		writer.flush();
	}

	/**
	 * No-op: JSON output does not require a document start.
	 *
	 * @throws IOException never thrown
	 */
	@Override
	public void startDocument() throws IOException {
		// no-op
	}

	/**
	 * No-op: JSON output does not require a document end.
	 *
	 * @throws IOException never thrown
	 */
	@Override
	public void endDocument() throws IOException {
		// no-op
	}

	/**
	 * No-op: element boundaries are not used for JSON writing.
	 *
	 * @param name the element name
	 * @param component the UI component
	 * @throws IOException never thrown
	 */
	@Override
	public void startElement(String name, UIComponent component) throws IOException {
		// no-op
	}

	/**
	 * No-op: element boundaries are not used for JSON writing.
	 *
	 * @param name the element name
	 * @throws IOException never thrown
	 */
	@Override
	public void endElement(String name) throws IOException {
		// no-op
	}

	/**
	 * No-op: attributes are not used for JSON writing.
	 *
	 * @param name the attribute name
	 * @param value the attribute value
	 * @param property the property name
	 * @throws IOException never thrown
	 */
	@Override
	public void writeAttribute(String name, Object value, String property) throws IOException {
		// no-op
	}

	/**
	 * No-op: URI attributes are not used for JSON writing.
	 *
	 * @param name the attribute name
	 * @param value the attribute value
	 * @param property the property name
	 * @throws IOException never thrown
	 */
	@Override
	public void writeURIAttribute(String name, Object value, String property) throws IOException {
		// no-op
	}

	/**
	 * No-op: comments are not emitted for JSON output.
	 *
	 * @param comment the comment object
	 * @throws IOException never thrown
	 */
	@Override
	public void writeComment(Object comment) throws IOException {
		// no-op
	}

	/**
	 * Writes arbitrary text (typically JSON) to the internal buffer.
	 *
	 * @param text the text to write
	 * @param property the related property (unused)
	 * @throws IOException if an I/O error occurs
	 */
	@Override
	public void writeText(Object text, String property) throws IOException {
		writer.write(text.toString());
	}

	/**
	 * Writes a char array segment to the internal buffer.
	 *
	 * @param text the characters to write
	 * @param off the start offset
	 * @param len the number of characters to write
	 * @throws IOException if an I/O error occurs
	 */
	@Override
	public void writeText(char[] text, int off, int len) throws IOException {
		writer.write(text, off, len);
	}

	/**
	 * Cloning is not supported for this writer implementation.
	 *
	 * @param writer the target writer
	 * @return always {@code null}
	 */
	@Override
	public jakarta.faces.context.ResponseWriter cloneWithWriter(@SuppressWarnings("hiding") Writer writer) {
		return null;
	}

	/**
	 * Writes a char array segment to the internal buffer.
	 *
	 * @param cbuf the buffer
	 * @param off the start offset
	 * @param len the number of characters to write
	 * @throws IOException if an I/O error occurs
	 */
	@Override
	public void write(char[] cbuf, int off, int len) throws IOException {
		writer.write(cbuf, off, len);
	}

	/**
	 * Closes the underlying {@link StringWriter}.
	 *
	 * @throws IOException if an I/O error occurs
	 */
	@Override
	public void close() throws IOException {
		writer.close();
	}
	
	/**
	 * Provides access to the underlying {@link StringWriter} used to accumulate output.
	 *
	 * @return the internal StringWriter buffer
	 */
	public StringWriter getStringWriter() {
		return writer;
	}
}