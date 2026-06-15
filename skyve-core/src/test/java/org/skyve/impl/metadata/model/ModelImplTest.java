package org.skyve.impl.metadata.model;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

import java.util.List;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.model.document.field.Text;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
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

	@Test
	void getAllAttributesWithNoInheritanceReturnsOwnAttributes() {
		DocumentImpl d = new DocumentImpl();
		Customer customer = mock(Customer.class);
		Text attr1 = new Text();
		attr1.setName("firstName");
		Text attr2 = new Text();
		attr2.setName("lastName");
		d.putAttribute(attr1);
		d.putAttribute(attr2);
		// No inheritance (inherits is null), so Customer parameter is not used
		List<? extends Attribute> all = d.getAllAttributes(customer);
		assertEquals(2, all.size());
		assertEquals("firstName", all.get(0).getName());
		assertEquals("lastName", all.get(1).getName());
	}

	@Test
	void getAllAttributesReturnsEmptyListWhenNoAttributes() {
		DocumentImpl d = new DocumentImpl();
		Customer customer = mock(Customer.class);
		List<? extends Attribute> all = d.getAllAttributes(customer);
		assertNotNull(all);
		assertTrue(all.isEmpty());
	}

	@Test
	void getPolymorphicAttributeReturnsNullWhenNoInheritanceAndNotFound() {
		DocumentImpl d = new DocumentImpl();
		Customer customer = mock(Customer.class);
		Text attr = new Text();
		attr.setName("name");
		d.putAttribute(attr);
		// "missing" does not exist, no inheritance
		Attribute result = d.getPolymorphicAttribute(customer, "missing");
		assertNull(result);
	}

	@Test
	void getPolymorphicAttributeReturnsDirectMatchWithoutCustomer() {
		DocumentImpl d = new DocumentImpl();
		Customer customer = mock(Customer.class);
		Text attr = new Text();
		attr.setName("name");
		d.putAttribute(attr);
		// Found directly — no inheritance lookup needed
		Attribute result = d.getPolymorphicAttribute(customer, "name");
		assertNotNull(result);
		assertEquals("name", result.getName());
	}

	@Test
	void putInterfaceAddsToInterfacesList() {
		DocumentImpl d = new DocumentImpl();
		assertTrue(d.getInterfaces().isEmpty());
		InterfaceImpl iface = new InterfaceImpl();
		iface.setInterfaceName("com.example.MyInterface");
		d.putInterface(iface);
		assertEquals(1, d.getInterfaces().size());
		assertEquals("com.example.MyInterface", d.getInterfaces().get(0).getInterfaceName());
	}

	@Test
	void putMultipleInterfacesAddsAllToList() {
		DocumentImpl d = new DocumentImpl();
		InterfaceImpl iface1 = new InterfaceImpl();
		iface1.setInterfaceName("com.example.InterfaceOne");
		InterfaceImpl iface2 = new InterfaceImpl();
		iface2.setInterfaceName("com.example.InterfaceTwo");
		d.putInterface(iface1);
		d.putInterface(iface2);
		assertEquals(2, d.getInterfaces().size());
	}
}
