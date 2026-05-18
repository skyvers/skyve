package org.skyve.metadata.view.fluent;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.impl.metadata.view.container.Collapsible;
import org.skyve.impl.metadata.view.container.form.Form;
import org.skyve.impl.metadata.view.container.form.FormLabelLayout;

@SuppressWarnings("static-method")
class FluentFormTest {

	@Test
	void defaultConstructorCreatesForm() {
		assertThat(new FluentForm().get(), is(notNullValue()));
	}

	@Test
	void wrappingConstructorUsesForm() {
		Form f = new Form();
		assertEquals(f, new FluentForm(f).get());
	}

	@Test
	void widgetIdSetsValue() {
		assertThat(new FluentForm().widgetId("wid").get().getWidgetId(), is("wid"));
	}

	@Test
	void borderSetsTrue() {
		assertThat(new FluentForm().border(true).get().getBorder(), is(Boolean.TRUE));
	}

	@Test
	void borderSetsFalse() {
		assertThat(new FluentForm().border(false).get().getBorder(), is(Boolean.FALSE));
	}

	@Test
	void borderTitleSetsValue() {
		assertThat(new FluentForm().borderTitle("My Form").get().getBorderTitle(), is("My Form"));
	}

	@Test
	void pixelWidthSetsValue() {
		assertEquals(300, new FluentForm().pixelWidth(300).get().getPixelWidth().intValue());
	}

	@Test
	void responsiveWidthSetsValue() {
		assertEquals(6, new FluentForm().responsiveWidth(6).get().getResponsiveWidth().intValue());
	}

	@Test
	void smSetsValue() {
		assertEquals(2, new FluentForm().sm(2).get().getSm().intValue());
	}

	@Test
	void mdSetsValue() {
		assertEquals(4, new FluentForm().md(4).get().getMd().intValue());
	}

	@Test
	void lgSetsValue() {
		assertEquals(6, new FluentForm().lg(6).get().getLg().intValue());
	}

	@Test
	void xlSetsValue() {
		assertEquals(8, new FluentForm().xl(8).get().getXl().intValue());
	}

	@Test
	void percentageWidthSetsValue() {
		assertEquals(50, new FluentForm().percentageWidth(50).get().getPercentageWidth().intValue());
	}

	@Test
	void minPixelWidthSetsValue() {
		assertEquals(100, new FluentForm().minPixelWidth(100).get().getMinPixelWidth().intValue());
	}

	@Test
	void maxPixelWidthSetsValue() {
		assertEquals(800, new FluentForm().maxPixelWidth(800).get().getMaxPixelWidth().intValue());
	}

	@Test
	void pixelHeightSetsValue() {
		assertEquals(200, new FluentForm().pixelHeight(200).get().getPixelHeight().intValue());
	}

	@Test
	void percentageHeightSetsValue() {
		assertEquals(75, new FluentForm().percentageHeight(75).get().getPercentageHeight().intValue());
	}

	@Test
	void minPixelHeightSetsValue() {
		assertEquals(50, new FluentForm().minPixelHeight(50).get().getMinPixelHeight().intValue());
	}

	@Test
	void maxPixelHeightSetsValue() {
		assertEquals(500, new FluentForm().maxPixelHeight(500).get().getMaxPixelHeight().intValue());
	}

	@Test
	void labelDefaultHorizontalAlignmentSetsValue() {
		assertThat(new FluentForm().labelDefaultHorizontalAlignment(HorizontalAlignment.right).get().getLabelDefaultHorizontalAlignment(),
				is(HorizontalAlignment.right));
	}

	@Test
	void collapsibleSetsValue() {
		assertThat(new FluentForm().collapsible(Collapsible.closed).get().getCollapsible(), is(Collapsible.closed));
	}

	@Test
	void labelLayoutSetsValue() {
		assertThat(new FluentForm().labelLayout(FormLabelLayout.top).get().getLabelLayout(), is(FormLabelLayout.top));
	}

	@Test
	void disabledConditionNameSetsValue() {
		assertThat(new FluentForm().disabledConditionName("disabled").get().getDisabledConditionName(), is("disabled"));
	}

	@Test
	void invisibleConditionNameSetsValue() {
		assertThat(new FluentForm().invisibleConditionName("invisible").get().getInvisibleConditionName(), is("invisible"));
	}

	@Test
	void addColumnAddsColumn() {
		FluentForm form = new FluentForm().addColumn(new FluentFormColumn());
		assertEquals(1, form.get().getColumns().size());
	}

	@Test
	void addColumnAtIndexInserts() {
		FluentForm form = new FluentForm()
				.addColumn(new FluentFormColumn())
				.addColumn(0, new FluentFormColumn());
		assertEquals(2, form.get().getColumns().size());
	}

	@Test
	void getColumnReturnsColumn() {
		FluentForm form = new FluentForm().addColumn(new FluentFormColumn());
		assertThat(form.getColumn(0), is(notNullValue()));
	}

	@Test
	void removeColumnRemovesColumn() {
		FluentForm form = new FluentForm().addColumn(new FluentFormColumn()).removeColumn(0);
		assertEquals(0, form.get().getColumns().size());
	}

	@Test
	void clearColumnsRemovesAll() {
		FluentForm form = new FluentForm()
				.addColumn(new FluentFormColumn())
				.addColumn(new FluentFormColumn())
				.clearColumns();
		assertEquals(0, form.get().getColumns().size());
	}

	@Test
	void addRowAddsRow() {
		FluentForm form = new FluentForm().addRow(new FluentFormRow());
		assertEquals(1, form.get().getRows().size());
	}

	@Test
	void addRowAtIndexInserts() {
		FluentForm form = new FluentForm()
				.addRow(new FluentFormRow())
				.addRow(0, new FluentFormRow());
		assertEquals(2, form.get().getRows().size());
	}

	@Test
	void getRowReturnsRow() {
		FluentForm form = new FluentForm().addRow(new FluentFormRow());
		assertThat(form.getRow(0), is(notNullValue()));
	}

	@Test
	void removeRowRemovesRow() {
		FluentForm form = new FluentForm().addRow(new FluentFormRow()).removeRow(0);
		assertEquals(0, form.get().getRows().size());
	}

	@Test
	void clearRowsRemovesAll() {
		FluentForm form = new FluentForm()
				.addRow(new FluentFormRow())
				.addRow(new FluentFormRow())
				.clearRows();
		assertEquals(0, form.get().getRows().size());
	}

	@Test
	void findItemsReturnsEmptyWhenNoRows() {
		assertTrue(new FluentForm().findItems("binding").isEmpty());
	}

	@Test
	void findItemsReturnsMatchingBoundItems() {
		FluentFormItem item = new FluentFormItem().textField(new FluentTextField().binding("myField"));
		FluentForm form = new FluentForm().addRow(new FluentFormRow().addItem(item));
		assertFalse(form.findItems("myField").isEmpty());
		assertTrue(form.findItems("other").isEmpty());
	}

	@Test
	void fromCopiesWidgetId() {
		Form src = new Form();
		src.setWidgetId("wid");
		assertThat(new FluentForm().from(src).get().getWidgetId(), is("wid"));
	}

	@Test
	void fromCopiesBorder() {
		Form src = new Form();
		src.setBorder(Boolean.TRUE);
		assertThat(new FluentForm().from(src).get().getBorder(), is(Boolean.TRUE));
	}

	@Test
	void fromCopiesBorderTitle() {
		Form src = new Form();
		src.setBorderTitle("Title");
		assertThat(new FluentForm().from(src).get().getBorderTitle(), is("Title"));
	}

	@Test
	void fromCopiesDisabledConditionName() {
		Form src = new Form();
		src.setDisabledConditionName("disabled");
		assertThat(new FluentForm().from(src).get().getDisabledConditionName(), is("disabled"));
	}

	@Test
	void fromCopiesInvisibleConditionName() {
		Form src = new Form();
		src.setInvisibleConditionName("invisible");
		assertThat(new FluentForm().from(src).get().getInvisibleConditionName(), is("invisible"));
	}

	@Test
	void fromCopiesLabelLayout() {
		Form src = new Form();
		src.setLabelLayout(FormLabelLayout.top);
		assertThat(new FluentForm().from(src).get().getLabelLayout(), is(FormLabelLayout.top));
	}

	@Test
	void fromCopiesColumns() {
		Form src = new Form();
		src.getColumns().add(new FluentFormColumn().get());
		assertEquals(1, new FluentForm().from(src).get().getColumns().size());
	}

	@Test
	void fromCopiesRows() {
		Form src = new Form();
		src.getRows().add(new FluentFormRow().get());
		assertEquals(1, new FluentForm().from(src).get().getRows().size());
	}

	@Test
	void fromBorderFalseWhenNotSet() {
		Form src = new Form();
		src.setBorder(null);
		assertFalse(Boolean.TRUE.equals(new FluentForm().from(src).get().getBorder()));
	}

        @Test
        void fromCopiesSizingFields() {
                Form src = new Form();
                src.setPixelWidth(Integer.valueOf(300));
                src.setPixelHeight(Integer.valueOf(200));
                src.setPercentageWidth(Integer.valueOf(50));
                src.setPercentageHeight(Integer.valueOf(75));
                src.setMinPixelWidth(Integer.valueOf(100));
                src.setMaxPixelWidth(Integer.valueOf(800));
                src.setMinPixelHeight(Integer.valueOf(50));
                src.setMaxPixelHeight(Integer.valueOf(500));
                src.setResponsiveWidth(Integer.valueOf(6));
                src.setSm(Integer.valueOf(2));
                src.setMd(Integer.valueOf(4));
                src.setLg(Integer.valueOf(6));
                src.setXl(Integer.valueOf(8));
                Form result = new FluentForm().from(src).get();
                assertEquals(300, result.getPixelWidth().intValue());
                assertEquals(200, result.getPixelHeight().intValue());
                assertEquals(50, result.getPercentageWidth().intValue());
                assertEquals(75, result.getPercentageHeight().intValue());
                assertEquals(100, result.getMinPixelWidth().intValue());
                assertEquals(800, result.getMaxPixelWidth().intValue());
                assertEquals(50, result.getMinPixelHeight().intValue());
                assertEquals(500, result.getMaxPixelHeight().intValue());
                assertEquals(6, result.getResponsiveWidth().intValue());
                assertEquals(2, result.getSm().intValue());
                assertEquals(4, result.getMd().intValue());
                assertEquals(6, result.getLg().intValue());
                assertEquals(8, result.getXl().intValue());
        }
}
