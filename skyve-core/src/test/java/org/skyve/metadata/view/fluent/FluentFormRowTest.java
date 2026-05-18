package org.skyve.metadata.view.fluent;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.container.form.FormRow;
import org.skyve.impl.metadata.view.widget.bound.input.TextField;

/**
 * Tests for {@link FluentFormRow} item management.
 */
@SuppressWarnings("static-method")
class FluentFormRowTest {

	@Test
	void defaultConstructorCreatesInstance() {
		assertThat(new FluentFormRow().get(), is(notNullValue()));
	}

	@Test
	void wrappingConstructorPreservesInstance() {
		FormRow row = new FormRow();
		assertThat(new FluentFormRow(row).get(), is(row));
	}

	@Test
	void addItemAppendsItem() {
		FluentFormRow row = new FluentFormRow();
		row.addItem(new FluentFormItem().textField(new FluentTextField()));
		assertEquals(1, row.get().getItems().size());
	}

	@Test
	void addItemAtIndexInsertsAtPosition() {
		FluentFormRow row = new FluentFormRow();
		row.addItem(new FluentFormItem().textField(new FluentTextField()));
		row.addItem(new FluentFormItem().textField(new FluentTextField()));
		FluentFormItem middle = new FluentFormItem().label("middle");
		row.addItem(1, middle);
		assertEquals(3, row.get().getItems().size());
		assertThat(row.get().getItems().get(1), is(middle.get()));
	}

	@Test
	void getItemReturnsWrappedItem() {
		FluentFormItem item = new FluentFormItem().label("email");
		FluentFormRow row = new FluentFormRow();
		row.addItem(item);
		assertThat(row.getItem(0).get(), is(item.get()));
	}

	@Test
	void removeItemRemovesAtIndex() {
		FluentFormRow row = new FluentFormRow();
		row.addItem(new FluentFormItem());
		row.addItem(new FluentFormItem());
		row.removeItem(0);
		assertEquals(1, row.get().getItems().size());
	}

	@Test
	void clearItemsRemovesAll() {
		FluentFormRow row = new FluentFormRow();
		row.addItem(new FluentFormItem());
		row.addItem(new FluentFormItem());
		row.clearItems();
		assertEquals(0, row.get().getItems().size());
	}

	@Test
	void fromCopiesItems() {
		FormRow source = new FormRow();
		org.skyve.impl.metadata.view.container.form.FormItem fi = new org.skyve.impl.metadata.view.container.form.FormItem();
		fi.setWidget(new TextField());
		source.getItems().add(fi);
		FluentFormRow result = new FluentFormRow().from(source);
		assertEquals(1, result.get().getItems().size());
	}

	@Test
	void formRowGetPropertiesIsNotNull() {
		FormRow row = new FormRow();
		assertThat(row.getProperties(), is(notNullValue()));
	}
}
