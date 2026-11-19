package org.skyve.impl.bizport;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

/**
 * Tests for {@link DataFileExportField}.
 */
@SuppressWarnings("static-method")
class DataFileExportFieldTest {

	@Test
	void constructorStoresFieldTitleAndBinding() {
		DataFileExportField field = new DataFileExportField("My Title", "myBinding");
		assertEquals("My Title", field.getFieldTitle());
		assertEquals("myBinding", field.getBindingExpression());
	}

	@Test
	void constructorWithNullBinding() {
		DataFileExportField field = new DataFileExportField("Title", null);
		assertEquals("Title", field.getFieldTitle());
		assertNull(field.getBindingExpression());
	}

	@Test
	void setFieldTitleUpdatesTitle() {
		DataFileExportField field = new DataFileExportField("Old", "binding");
		field.setFieldTitle("New Title");
		assertEquals("New Title", field.getFieldTitle());
	}

	@Test
	void setBindingExpressionStoresExpectedValue() {
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
	void setBindingExpressionNullStoresNull() {
		DataFileExportField field = new DataFileExportField("Title", "original");
		field.setBindingExpression(null);
		assertNull(field.getBindingExpression());
	}

}
