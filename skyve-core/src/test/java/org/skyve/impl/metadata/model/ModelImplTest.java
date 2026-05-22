package org.skyve.impl.metadata.model;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.model.document.field.Text;
import org.skyve.metadata.model.Dynamic;

/**
 * Tests for {@link ModelImpl} getter/setter methods, exercised via the
 * concrete {@link DocumentImpl} subclass (which does not require a live runtime).
 */
@SuppressWarnings("static-method")
class ModelImplTest {

	@Test
	void owningModuleNameRoundtrip() {
		DocumentImpl d = new DocumentImpl();
		d.setOwningModuleName("admin");
		assertThat(d.getOwningModuleName(), is("admin"));
	}

	@Test
	void pluralAliasRoundtrip() {
		DocumentImpl d = new DocumentImpl();
		d.setPluralAlias("Contacts");
		assertThat(d.getPluralAlias(), is("Contacts"));
	}

	@Test
	void singularAliasRoundtrip() {
		DocumentImpl d = new DocumentImpl();
		d.setSingularAlias("Contact");
		assertThat(d.getSingularAlias(), is("Contact"));
	}

	@Test
	void icon16x16RoundTrip() {
		DocumentImpl d = new DocumentImpl();
		d.setIcon16x16RelativeFileName("icons/icon.png");
		assertThat(d.getIcon16x16RelativeFileName(), is("icons/icon.png"));
	}

	@Test
	void icon32x32RoundTrip() {
		DocumentImpl d = new DocumentImpl();
		d.setIcon32x32RelativeFileName("icons/icon32.png");
		assertThat(d.getIcon32x32RelativeFileName(), is("icons/icon32.png"));
	}

	@Test
	void iconStyleClassRoundTrip() {
		DocumentImpl d = new DocumentImpl();
		d.setIconStyleClass("fa fa-user");
		assertThat(d.getIconStyleClass(), is("fa fa-user"));
	}

	@Test
	void abstractClassRoundTrip() {
		DocumentImpl d = new DocumentImpl();
		d.setAbstract(true);
		assertTrue(d.isAbstract());
	}

	@Test
	void auditedDefaultIsTrue() {
		DocumentImpl d = new DocumentImpl();
		assertTrue(d.isAudited());
	}

	@Test
	void auditedRoundTrip() {
		DocumentImpl d = new DocumentImpl();
		d.setAudited(false);
		assertFalse(d.isAudited());
	}

	@Test
	void nameRoundTrip() {
		DocumentImpl d = new DocumentImpl();
		d.setName("Contact");
		assertThat(d.getName(), is("Contact"));
	}

	@Test
	void descriptionRoundTrip() {
		DocumentImpl d = new DocumentImpl();
		d.setDescription("A contact document");
		assertThat(d.getDescription(), is("A contact document"));
	}

	@Test
	void dynamismIsNullByDefault() {
		DocumentImpl d = new DocumentImpl();
		assertThat(d.getDynamism(), is(nullValue()));
	}

	@Test
	void isDynamicFalseWhenNoDynamism() {
		DocumentImpl d = new DocumentImpl();
		assertFalse(d.isDynamic());
	}

	@Test
	void setDynamismMakesIsDynamicTrue() {
		DocumentImpl d = new DocumentImpl();
		d.setDynamism(new Dynamic());
		assertTrue(d.isDynamic());
		assertNotNull(d.getDynamism());
	}

	@Test
	void putAttributeAddsToList() {
		DocumentImpl d = new DocumentImpl();
		Text field = new Text();
		field.setName("firstName");
		d.putAttribute(field);
		assertEquals(1, d.getAttributes().size());
	}

	@Test
	void getAttributeByNameReturnsMatch() {
		DocumentImpl d = new DocumentImpl();
		Text field = new Text();
		field.setName("firstName");
		d.putAttribute(field);
		assertThat(d.getAttribute("firstName"), is(notNullValue()));
	}

	@Test
	void getAttributeByNameReturnsNullWhenMissing() {
		DocumentImpl d = new DocumentImpl();
		assertThat(d.getAttribute("missing"), is(nullValue()));
	}

	@Test
	void getInterfacesInitiallyEmpty() {
		DocumentImpl d = new DocumentImpl();
		assertTrue(d.getInterfaces().isEmpty());
	}

	@Test
	void toStringContainsName() {
		DocumentImpl d = new DocumentImpl();
		d.setName("Contact");
		assertTrue(d.toString().contains("Contact"));
	}

	@Test
	void clearHasDynamicDoesNotThrow() {
		DocumentImpl d = new DocumentImpl();
		// clearHasDynamic resets the cached hasDynamic field to null
		assertDoesNotThrow(d::clearHasDynamic);
	}
}
