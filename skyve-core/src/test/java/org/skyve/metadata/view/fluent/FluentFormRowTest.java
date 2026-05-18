package org.skyve.metadata.view.fluent;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;

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
		assertThat(row.get().getItems().size(), is(1));
	}

	@Test
	void addItemAtIndexInsertsAtPosition() {
		FluentFormRow row = new FluentFormRow();
		row.addItem(new FluentFormItem().textField(new FluentTextField()));
		row.addItem(new FluentFormItem().textField(new FluentTextField()));
		FluentFormItem middle = new FluentFormItem().label("middle");
		row.addItem(1, middle);
		assertThat(row.get().getItems().size(), is(3));
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
		assertThat(row.get().getItems().size(), is(1));
	}

	@Test
	void clearItemsRemovesAll() {
		FluentFormRow row = new FluentFormRow();
		row.addItem(new FluentFormItem());
		row.addItem(new FluentFormItem());
		row.clearItems();
		assertThat(row.get().getItems().size(), is(0));
	}

	@Test
	void fromCopiesItems() {
		FormRow source = new FormRow();
		org.skyve.impl.metadata.view.container.form.FormItem fi = new org.skyve.impl.metadata.view.container.form.FormItem();
		fi.setWidget(new TextField());
		source.getItems().add(fi);
		FluentFormRow result = new FluentFormRow().from(source);
		assertThat(result.get().getItems().size(), is(1));
	}

	@Test
	void formRowGetPropertiesIsNotNull() {
		FormRow row = new FormRow();
		assertThat(row.getProperties(), is(notNullValue()));
	}
}
