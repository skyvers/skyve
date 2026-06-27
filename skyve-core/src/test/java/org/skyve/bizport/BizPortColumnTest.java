package org.skyve.bizport;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.model.Attribute.AttributeType;

class BizPortColumnTest {

	@Test
	@SuppressWarnings("static-method")
	void threeArgConstructorSetsFields() {
		BizPortColumn col = new BizPortColumn("Name", "The name field", AttributeType.text);
		assertThat(col.getTitle(), is("Name"));
		assertThat(col.getComment(), is("The name field"));
		assertThat(col.getType(), is(AttributeType.text));
	}

	@Test
	@SuppressWarnings("static-method")
	void twoArgConstructorSetsFields() {
		BizPortColumn col = new BizPortColumn("Name", "The name field");
		assertThat(col.getTitle(), is("Name"));
		assertThat(col.getComment(), is("The name field"));
		assertNull(col.getType());
	}

	@Test
	@SuppressWarnings("static-method")
	void setTitleRoundtrip() {
		BizPortColumn col = new BizPortColumn("Old", null);
		col.setTitle("New");
		assertThat(col.getTitle(), is("New"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setCommentRoundtrip() {
		BizPortColumn col = new BizPortColumn(null, null);
		col.setComment("A comment");
		assertThat(col.getComment(), is("A comment"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setTypeRoundtrip() {
		BizPortColumn col = new BizPortColumn("Col", null);
		col.setType(AttributeType.integer);
		assertThat(col.getType(), is(AttributeType.integer));
	}

	@Test
	@SuppressWarnings("static-method")
	void setIndexRoundtrip() {
		BizPortColumn col = new BizPortColumn("Col", null, AttributeType.text);
		col.setIndex(5);
		assertEquals(5, col.getIndex());
	}

	@Test
	@SuppressWarnings("static-method")
	void setMinValueRoundtrip() {
		BizPortColumn col = new BizPortColumn("Col", null, AttributeType.integer);
		col.setMinValue(Integer.valueOf(0));
		assertThat(col.getMinValue(), is(Integer.valueOf(0)));
	}

	@Test
	@SuppressWarnings("static-method")
	void setMaxValueRoundtrip() {
		BizPortColumn col = new BizPortColumn("Col", null, AttributeType.integer);
		col.setMaxValue(Integer.valueOf(100));
		assertThat(col.getMaxValue(), is(Integer.valueOf(100)));
	}

	@Test
	@SuppressWarnings("static-method")
	void setRangeValuesRoundtrip() {
		BizPortColumn col = new BizPortColumn("Col", null, AttributeType.text);
		String[] ranges = {"optionA", "optionB"};
		col.setRangeValues(ranges);
		assertThat(col.getRangeValues(), is(ranges));
	}

	@Test
	@SuppressWarnings("static-method")
	void setReferencedSheetRoundtrip() {
		BizPortColumn col = new BizPortColumn("FK", null, AttributeType.text);
		SheetKey sheetKey = new SheetKey("admin", "User");
		col.setReferencedSheet(sheetKey);
		assertThat(col.getReferencedSheet(), is(sheetKey));
	}
}
