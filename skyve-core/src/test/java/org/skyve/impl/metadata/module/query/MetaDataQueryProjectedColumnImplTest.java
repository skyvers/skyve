package org.skyve.impl.metadata.module.query;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.FormatterName;
import org.skyve.metadata.view.TextOutput.Sanitisation;

public class MetaDataQueryProjectedColumnImplTest {

	@Test
	@SuppressWarnings("static-method")
	public void defaultProjectedIsTrue() {
		MetaDataQueryProjectedColumnImpl col = new MetaDataQueryProjectedColumnImpl();
		assertTrue(col.isProjected());
	}

	@Test
	@SuppressWarnings("static-method")
	public void defaultSortableIsTrue() {
		MetaDataQueryProjectedColumnImpl col = new MetaDataQueryProjectedColumnImpl();
		assertTrue(col.isSortable());
	}

	@Test
	@SuppressWarnings("static-method")
	public void defaultFilterableIsTrue() {
		MetaDataQueryProjectedColumnImpl col = new MetaDataQueryProjectedColumnImpl();
		assertTrue(col.isFilterable());
	}

	@Test
	@SuppressWarnings("static-method")
	public void defaultEditableIsTrue() {
		MetaDataQueryProjectedColumnImpl col = new MetaDataQueryProjectedColumnImpl();
		assertTrue(col.isEditable());
	}

	@Test
	@SuppressWarnings("static-method")
	public void defaultEscapeIsTrue() {
		MetaDataQueryProjectedColumnImpl col = new MetaDataQueryProjectedColumnImpl();
		assertTrue(col.isEscape());
	}

	@Test
	@SuppressWarnings("static-method")
	public void defaultSanitiseIsRelaxed() {
		MetaDataQueryProjectedColumnImpl col = new MetaDataQueryProjectedColumnImpl();
		assertThat(col.getSanitise(), is(Sanitisation.relaxed));
	}

	@Test
	@SuppressWarnings("static-method")
	public void setExpressionRoundtrip() {
		MetaDataQueryProjectedColumnImpl col = new MetaDataQueryProjectedColumnImpl();
		col.setExpression("bean.name");
		assertThat(col.getExpression(), is("bean.name"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void setSortableRoundtrip() {
		MetaDataQueryProjectedColumnImpl col = new MetaDataQueryProjectedColumnImpl();
		col.setSortable(false);
		assertFalse(col.isSortable());
	}

	@Test
	@SuppressWarnings("static-method")
	public void setFilterableRoundtrip() {
		MetaDataQueryProjectedColumnImpl col = new MetaDataQueryProjectedColumnImpl();
		col.setFilterable(false);
		assertFalse(col.isFilterable());
	}

	@Test
	@SuppressWarnings("static-method")
	public void setEditableRoundtrip() {
		MetaDataQueryProjectedColumnImpl col = new MetaDataQueryProjectedColumnImpl();
		col.setEditable(false);
		assertFalse(col.isEditable());
	}

	@Test
	@SuppressWarnings("static-method")
	public void setEscapeRoundtrip() {
		MetaDataQueryProjectedColumnImpl col = new MetaDataQueryProjectedColumnImpl();
		col.setEscape(false);
		assertFalse(col.isEscape());
	}

	@Test
	@SuppressWarnings("static-method")
	public void setSanitiseRoundtrip() {
		MetaDataQueryProjectedColumnImpl col = new MetaDataQueryProjectedColumnImpl();
		col.setSanitise(Sanitisation.none);
		assertThat(col.getSanitise(), is(Sanitisation.none));
	}

	@Test
	@SuppressWarnings("static-method")
	public void setFormatterNameRoundtrip() {
		MetaDataQueryProjectedColumnImpl col = new MetaDataQueryProjectedColumnImpl();
		col.setFormatterName(FormatterName.DD_MM_YYYY);
		assertThat(col.getFormatterName(), is(FormatterName.DD_MM_YYYY));
	}

	@Test
	@SuppressWarnings("static-method")
	public void setCustomFormatterNameRoundtrip() {
		MetaDataQueryProjectedColumnImpl col = new MetaDataQueryProjectedColumnImpl();
		col.setCustomFormatterName("myFormatter");
		assertThat(col.getCustomFormatterName(), is("myFormatter"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void defaultFormatterNameIsNull() {
		MetaDataQueryProjectedColumnImpl col = new MetaDataQueryProjectedColumnImpl();
		assertNull(col.getFormatterName());
	}

	@Test
	@SuppressWarnings("static-method")
	public void propertiesInitiallyEmpty() {
		MetaDataQueryProjectedColumnImpl col = new MetaDataQueryProjectedColumnImpl();
		assertTrue(col.getProperties().isEmpty());
	}
}
