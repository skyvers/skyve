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
	public void setBindingExpressionStoresExpectedValue() {
		String[][] cases = {
				{"{myExpression}", "myExpression"},
				{"{noBrace", "{noBrace"},
				{"noBrace}", "noBrace}"},
				{"plainBinding", "plainBinding"},
				{"{}", ""}
		};
		for (String[] testCase : cases) {
			DataFileExportField field = new DataFileExportField("Title", "original");
			field.setBindingExpression(testCase[0]);
			assertEquals(testCase[1], field.getBindingExpression());
		}
	}

	@Test
	public void setBindingExpressionNullStoresNull() {
		DataFileExportField field = new DataFileExportField("Title", "original");
		field.setBindingExpression(null);
		assertNull(field.getBindingExpression());
	}

}
