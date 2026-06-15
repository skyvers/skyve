package org.skyve.impl.metadata.module.query;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.metadata.FilterOperator;
import org.skyve.metadata.SortDirection;

class AbstractMetaDataQueryColumnTest {

	@Test
	@SuppressWarnings("static-method")
	void setNameRoundtrip() {
		MetaDataQueryProjectedColumnImpl col = new MetaDataQueryProjectedColumnImpl();
		col.setName("myCol");
		assertThat(col.getName(), is("myCol"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setBindingRoundtrip() {
		MetaDataQueryProjectedColumnImpl col = new MetaDataQueryProjectedColumnImpl();
		col.setBinding("contact.name");
		assertThat(col.getBinding(), is("contact.name"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setDisplayNameRoundtrip() {
		MetaDataQueryProjectedColumnImpl col = new MetaDataQueryProjectedColumnImpl();
		col.setDisplayName("Full Name");
		assertThat(col.getDisplayName(), is("Full Name"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setFilterOperatorRoundtrip() {
		MetaDataQueryProjectedColumnImpl col = new MetaDataQueryProjectedColumnImpl();
		col.setFilterOperator(FilterOperator.equal);
		assertThat(col.getFilterOperator(), is(FilterOperator.equal));
	}

	@Test
	@SuppressWarnings("static-method")
	void setFilterExpressionRoundtrip() {
		MetaDataQueryProjectedColumnImpl col = new MetaDataQueryProjectedColumnImpl();
		col.setFilterExpression("foo");
		assertThat(col.getFilterExpression(), is("foo"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setSortOrderRoundtrip() {
		MetaDataQueryProjectedColumnImpl col = new MetaDataQueryProjectedColumnImpl();
		col.setSortOrder(SortDirection.ascending);
		assertThat(col.getSortOrder(), is(SortDirection.ascending));
	}

	@Test
	@SuppressWarnings("static-method")
	void defaultHiddenIsFalse() {
		MetaDataQueryProjectedColumnImpl col = new MetaDataQueryProjectedColumnImpl();
		assertFalse(col.isHidden());
	}

	@Test
	@SuppressWarnings("static-method")
	void setHiddenRoundtrip() {
		MetaDataQueryProjectedColumnImpl col = new MetaDataQueryProjectedColumnImpl();
		col.setHidden(true);
		assertTrue(col.isHidden());
	}

	@Test
	@SuppressWarnings("static-method")
	void setPixelWidthRoundtrip() {
		MetaDataQueryProjectedColumnImpl col = new MetaDataQueryProjectedColumnImpl();
		col.setPixelWidth(Integer.valueOf(150));
		assertThat(col.getPixelWidth(), is(Integer.valueOf(150)));
	}

	@Test
	@SuppressWarnings("static-method")
	void setAlignmentRoundtrip() {
		MetaDataQueryProjectedColumnImpl col = new MetaDataQueryProjectedColumnImpl();
		col.setAlignment(HorizontalAlignment.centre);
		assertThat(col.getAlignment(), is(HorizontalAlignment.centre));
	}

	@Test
	@SuppressWarnings("static-method")
	void defaultAlignmentIsNull() {
		MetaDataQueryProjectedColumnImpl col = new MetaDataQueryProjectedColumnImpl();
		assertNull(col.getAlignment());
	}

        @Test
        @SuppressWarnings("static-method")
        void getLocalisedDisplayNameWithNullDisplayNameReturnsNull() {
                MetaDataQueryProjectedColumnImpl col = new MetaDataQueryProjectedColumnImpl();
                // displayName not set, so getLocalisedDisplayName() calls Util.i18n(null) which returns null
                assertNull(col.getLocalisedDisplayName());
        }
}
