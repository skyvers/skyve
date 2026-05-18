package org.skyve.impl.bizport;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import org.junit.Test;

/**
 * Tests for {@link DataFileExportField}.
 */
@SuppressWarnings("static-method")
public class DataFileExportFieldTest {

	@Test
	public void constructorStoresFieldTitleAndBinding() {
		DataFileExportField field = new DataFileExportField("My Title", "myBinding");
		assertEquals("My Title", field.getFieldTitle());
		assertEquals("myBinding", field.getBindingExpression());
	}

	@Test
	public void constructorWithNullBinding() {
		DataFileExportField field = new DataFileExportField("Title", null);
		assertEquals("Title", field.getFieldTitle());
		assertNull(field.getBindingExpression());
	}

	@Test
	public void setFieldTitleUpdatesTitle() {
		DataFileExportField field = new DataFileExportField("Old", "binding");
		field.setFieldTitle("New Title");
		assertEquals("New Title", field.getFieldTitle());
	}

	@Test
	public void setBindingExpressionWithCurlyBracesStripsThemOff() {
		DataFileExportField field = new DataFileExportField("Title", "original");
		field.setBindingExpression("{myExpression}");
		assertEquals("myExpression", field.getBindingExpression());
	}

	@Test
	public void setBindingExpressionWithOnlyOpenBraceDoesNotStrip() {
		DataFileExportField field = new DataFileExportField("Title", "original");
		field.setBindingExpression("{noBrace");
		assertEquals("{noBrace", field.getBindingExpression());
	}

	@Test
	public void setBindingExpressionWithOnlyCloseBraceDoesNotStrip() {
		DataFileExportField field = new DataFileExportField("Title", "original");
		field.setBindingExpression("noBrace}");
		assertEquals("noBrace}", field.getBindingExpression());
	}

	@Test
	public void setBindingExpressionPlainStringStoredAsIs() {
		DataFileExportField field = new DataFileExportField("Title", "original");
		field.setBindingExpression("plainBinding");
		assertEquals("plainBinding", field.getBindingExpression());
	}

	@Test
	public void setBindingExpressionNullStoresNull() {
		DataFileExportField field = new DataFileExportField("Title", "original");
		field.setBindingExpression(null);
		assertNull(field.getBindingExpression());
	}

	@Test
	public void setBindingExpressionEmptyCurlyBracesStripsToEmpty() {
		DataFileExportField field = new DataFileExportField("Title", "original");
		field.setBindingExpression("{}");
		assertEquals("", field.getBindingExpression());
	}
}
