package org.skyve.impl.web.faces.charts.config;

import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;

import javax.faces.component.UIComponent;

import org.skyve.util.Util;

public class ResponseWriter extends javax.faces.context.ResponseWriter {
	private StringWriter writer = new StringWriter(2048);
	
	@Override
	public String getContentType() {
		return "application/json";
	}

	@Override
	public String getCharacterEncoding() {
		return Util.UTF8;
	}

	@Override
	public void flush() throws IOException {
		writer.flush();
	}

	@Override
	public void startDocument() throws IOException {
		// no-op
	}

	@Override
	public void endDocument() throws IOException {
		// no-op
	}

	@Override
	public void startElement(String name, UIComponent component) throws IOException {
		// no-op
	}

	@Override
	public void endElement(String name) throws IOException {
		// no-op
	}

	@Override
	public void writeAttribute(String name, Object value, String property) throws IOException {
		// no-op
	}

	@Override
	public void writeURIAttribute(String name, Object value, String property) throws IOException {
		// no-op
	}

	@Override
	public void writeComment(Object comment) throws IOException {
		// no-op
	}

	@Override
	public void writeText(Object text, String property) throws IOException {
		writer.write(text.toString());
	}

	@Override
	public void writeText(char[] text, int off, int len) throws IOException {
		writer.write(text, off, len);
	}

	@Override
	public javax.faces.context.ResponseWriter cloneWithWriter(@SuppressWarnings("hiding") Writer writer) {
		return null;
	}

	@Override
	public void write(char[] cbuf, int off, int len) throws IOException {
		writer.write(cbuf, off, len);
	}

	@Override
	public void close() throws IOException {
		writer.close();
	}
	
	public StringWriter getStringWriter() {
		return writer;
	}
}
