package org.skyve.metadata.view.model.comparison;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.widget.bound.input.TextField;

public class ComparisonPropertyTest {

	@Test
	@SuppressWarnings("static-method")
	public void defaultConstructorHasNullFields() {
		ComparisonProperty p = new ComparisonProperty();
		assertNull(p.getName());
		assertNull(p.getTitle());
		assertNull(p.getWidget());
		assertNull(p.getNewValue());
		assertNull(p.getOldValue());
	}

	@Test
	@SuppressWarnings("static-method")
	public void fullConstructorSetsAllFields() {
		ComparisonProperty p = new ComparisonProperty("fieldName", "Field Title", null, "newVal", "oldVal");
		assertThat(p.getName(), is("fieldName"));
		assertThat(p.getTitle(), is("Field Title"));
		assertNull(p.getWidget());
		assertThat(p.getNewValue(), is("newVal"));
		assertThat(p.getOldValue(), is("oldVal"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void setNameRoundtrip() {
		ComparisonProperty p = new ComparisonProperty();
		p.setName("myField");
		assertThat(p.getName(), is("myField"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void setTitleRoundtrip() {
		ComparisonProperty p = new ComparisonProperty();
		p.setTitle("My Field");
		assertThat(p.getTitle(), is("My Field"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void setNewValueRoundtrip() {
		ComparisonProperty p = new ComparisonProperty();
		p.setNewValue("newValue");
		assertThat(p.getNewValue(), is("newValue"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void setOldValueRoundtrip() {
		ComparisonProperty p = new ComparisonProperty();
		p.setOldValue("oldValue");
		assertThat(p.getOldValue(), is("oldValue"));
	}

	// ---- isDirty tests ----

	@Test
	@SuppressWarnings("static-method")
	public void isDirtyFalseWhenBothNull() {
		ComparisonProperty p = new ComparisonProperty();
		assertFalse(p.isDirty());
	}

	@Test
	@SuppressWarnings("static-method")
	public void isDirtyFalseWhenBothSame() {
		ComparisonProperty p = new ComparisonProperty("f", "t", null, "value", "value");
		assertFalse(p.isDirty());
	}

	@Test
	@SuppressWarnings("static-method")
	public void isDirtyTrueWhenOldNonNullNewDiffers() {
		ComparisonProperty p = new ComparisonProperty("f", "t", null, "newValue", "oldValue");
		assertTrue(p.isDirty());
	}

	@Test
	@SuppressWarnings("static-method")
	public void isDirtyTrueWhenOldNullNewNonNull() {
		ComparisonProperty p = new ComparisonProperty("f", "t", null, "newValue", null);
		assertTrue(p.isDirty());
	}

	@Test
	@SuppressWarnings("static-method")
	public void isDirtyTrueWhenOldNonNullNewNull() {
		ComparisonProperty p = new ComparisonProperty("f", "t", null, null, "oldValue");
		assertTrue(p.isDirty());
	}

	@Test
	@SuppressWarnings("static-method")
	public void setWidgetRoundtrip() {
		ComparisonProperty p = new ComparisonProperty();
		TextField widget = new TextField();
		p.setWidget(widget);
		assertNotNull(p.getWidget());
	}
}
