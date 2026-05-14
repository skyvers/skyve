package org.skyve.metadata.module.fluent;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.repository.module.MetaDataQueryProjectedColumnMetaData;
import org.skyve.metadata.FormatterName;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.view.TextOutput.Sanitisation;

/**
 * Tests for {@link FluentMetaDataQueryProjectedColumn} setters and inherited setters.
 */
@SuppressWarnings("static-method")
class FluentMetaDataQueryProjectedColumnTest {

	private static MetaDataQueryProjectedColumnMetaData get(FluentMetaDataQueryProjectedColumn col) {
		return (MetaDataQueryProjectedColumnMetaData) col.get();
	}

	@Test
	void defaultConstructorCreatesInstance() {
		assertThat(new FluentMetaDataQueryProjectedColumn().get(), is(notNullValue()));
	}

	@Test
	void wrappingConstructorPreservesInstance() {
		MetaDataQueryProjectedColumnMetaData col = new MetaDataQueryProjectedColumnMetaData();
		assertThat(new FluentMetaDataQueryProjectedColumn(col).get(), is(col));
	}

	// ---- inherited column setters ----

	@Test
	void nameSetsValue() {
		assertThat(new FluentMetaDataQueryProjectedColumn().name("col1").get().getName(), is("col1"));
	}

	@Test
	void bindingSetsValue() {
		assertThat(new FluentMetaDataQueryProjectedColumn().binding("email").get().getBinding(), is("email"));
	}

	@Test
	void displayNameSetsValue() {
		assertThat(new FluentMetaDataQueryProjectedColumn().displayName("Email Address").get().getDisplayName(),
				is("Email Address"));
	}

	@Test
	void sortOrderSetsValue() {
		assertThat(new FluentMetaDataQueryProjectedColumn().sortOrder(SortDirection.descending).get().getSortOrder(),
				is(SortDirection.descending));
	}

	@Test
	void hiddenSetTrue() {
		assertThat(get(new FluentMetaDataQueryProjectedColumn().hidden(true)).getHidden(), is(Boolean.TRUE));
	}

	@Test
	void pixelWidthSetsValue() {
		assertThat(new FluentMetaDataQueryProjectedColumn().pixelWidth(120).get().getPixelWidth(),
				is(Integer.valueOf(120)));
	}

	// ---- projected column setters ----

	@Test
	void projectedSetsTrue() {
		assertThat(get(new FluentMetaDataQueryProjectedColumn().projected(true)).getProjected(), is(Boolean.TRUE));
	}

	@Test
	void projectedSetsFalse() {
		assertThat(get(new FluentMetaDataQueryProjectedColumn().projected(false)).getProjected(), is(Boolean.FALSE));
	}

	@Test
	void expressionSetsValue() {
		assertThat(get(new FluentMetaDataQueryProjectedColumn().expression("bean.fullName")).getExpression(),
				is("bean.fullName"));
	}

	@Test
	void sortableSetsTrue() {
		assertThat(get(new FluentMetaDataQueryProjectedColumn().sortable(true)).getSortable(), is(Boolean.TRUE));
	}

	@Test
	void filterableSetsTrue() {
		assertThat(get(new FluentMetaDataQueryProjectedColumn().filterable(true)).getFilterable(), is(Boolean.TRUE));
	}

	@Test
	void editableSetsTrue() {
		assertThat(get(new FluentMetaDataQueryProjectedColumn().editable(true)).getEditable(), is(Boolean.TRUE));
	}

	@Test
	void escapeSetsTrue() {
		assertThat(get(new FluentMetaDataQueryProjectedColumn().escape(true)).getEscape(), is(Boolean.TRUE));
	}

	@Test
	void sanitiseSetsValue() {
		assertThat(get(new FluentMetaDataQueryProjectedColumn().sanitise(Sanitisation.basic)).getSanitise(),
				is(Sanitisation.basic));
	}

	@Test
	void formatterSetsValue() {
		assertThat(get(new FluentMetaDataQueryProjectedColumn().formatter(FormatterName.DD_MM_YYYY)).getFormatterName(),
				is(FormatterName.DD_MM_YYYY));
	}

	@Test
	void customFormatterSetsValue() {
		assertThat(get(new FluentMetaDataQueryProjectedColumn().customFormatter("myFmt")).getCustomFormatterName(),
				is("myFmt"));
	}

	@Test
	void fromCopiesAllFields() {
		org.skyve.impl.metadata.module.query.MetaDataQueryProjectedColumnImpl source = new org.skyve.impl.metadata.module.query.MetaDataQueryProjectedColumnImpl();
		source.setName("myCol");
		source.setBinding("myBinding");
		source.setDisplayName("My Col");
		source.setSelected(true);
		source.setExpression("expr");
		source.setSortable(true);
		source.setFilterable(true);
		source.setEditable(true);
		source.setEscape(true);
		source.setSanitise(Sanitisation.basic);
		source.setFormatterName(FormatterName.DD_MM_YYYY);
		source.setCustomFormatterName("myFmt");

		FluentMetaDataQueryProjectedColumn fluent = new FluentMetaDataQueryProjectedColumn().from(source);
		MetaDataQueryProjectedColumnMetaData result = get(fluent);

		assertThat(result.getName(), is("myCol"));
		assertThat(result.getBinding(), is("myBinding"));
		assertThat(result.getDisplayName(), is("My Col"));
		assertThat(result.getProjected(), is(Boolean.TRUE));
		assertThat(result.getExpression(), is("expr"));
		assertThat(result.getSortable(), is(Boolean.TRUE));
		assertThat(result.getFilterable(), is(Boolean.TRUE));
		assertThat(result.getEditable(), is(Boolean.TRUE));
		assertThat(result.getEscape(), is(Boolean.TRUE));
		assertThat(result.getSanitise(), is(Sanitisation.basic));
		assertThat(result.getFormatterName(), is(FormatterName.DD_MM_YYYY));
		assertThat(result.getCustomFormatterName(), is("myFmt"));
	}
}
