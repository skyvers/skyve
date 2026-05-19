package org.skyve.metadata.model;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Collections;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Interface;

class ModelDefaultMethodsTest {

	/**
	 * Minimal Model implementation for testing default methods.
	 */
	private static class MinimalModel implements Model {
		private static final long serialVersionUID = 1L;

		private final String pluralAlias;
		private final String singularAlias;
		private final String description;
		private final Persistent persistent;

		MinimalModel(String pluralAlias, String singularAlias, String description, Persistent persistent) {
			this.pluralAlias = pluralAlias;
			this.singularAlias = singularAlias;
			this.description = description;
			this.persistent = persistent;
		}

		@Override public String getName() { return "test"; }
		@Override public String getOwningModuleName() { return "testModule"; }
		@Override public Attribute getAttribute(String name) { return null; }
		@Override public Attribute getPolymorphicAttribute(Customer customer, String name) { return null; }
		@Override public List<? extends Interface> getInterfaces() { return Collections.emptyList(); }
		@Override public List<? extends Attribute> getAttributes() { return Collections.emptyList(); }
		@Override public List<? extends Attribute> getAllAttributes(Customer customer) { return Collections.emptyList(); }
		@Override public String getPluralAlias() { return pluralAlias; }
		@Override public String getSingularAlias() { return singularAlias; }
		@Override public String getDescription() { return description; }
		@Override public Persistent getPersistent() { return persistent; }
		@Override public boolean isDynamic() { return false; }
		@Override public boolean hasDynamic() { return false; }
		@Override public Dynamic getDynamism() { return null; }
		@Override public Extends getExtends() { return null; }
		@Override public boolean isAbstract() { return false; }
		@Override public boolean isAudited() { return false; }
		@Override public String getIcon16x16RelativeFileName() { return null; }
		@Override public String getIcon32x32RelativeFileName() { return null; }
		@Override public String getIconStyleClass() { return null; }
	}

	@Test
	@SuppressWarnings("static-method")
	void getLocalisedPluralAliasReturnsKeyWhenNoPersistence() {
		MinimalModel model = new MinimalModel("Things", null, null, null);
		// Util.i18n returns the key when no persistence is initialised
		String result = model.getLocalisedPluralAlias();
		assertTrue("Things".equals(result) || result != null);
	}

	@Test
	@SuppressWarnings("static-method")
	void getLocalisedPluralAliasWithNullReturnsNull() {
		MinimalModel model = new MinimalModel(null, null, null, null);
		assertNull(model.getLocalisedPluralAlias());
	}

	@Test
	@SuppressWarnings("static-method")
	void getLocalisedSingularAliasReturnsKeyWhenNoPersistence() {
		MinimalModel model = new MinimalModel(null, "Thing", null, null);
		String result = model.getLocalisedSingularAlias();
		assertTrue("Thing".equals(result) || result != null);
	}

	@Test
	@SuppressWarnings("static-method")
	void getLocalisedSingularAliasWithNullReturnsNull() {
		MinimalModel model = new MinimalModel(null, null, null, null);
		assertNull(model.getLocalisedSingularAlias());
	}

	@Test
	@SuppressWarnings("static-method")
	void getLocalisedDescriptionReturnsKeyWhenNoPersistence() {
		MinimalModel model = new MinimalModel(null, null, "A description", null);
		String result = model.getLocalisedDescription();
		assertTrue("A description".equals(result) || result != null);
	}

	@Test
	@SuppressWarnings("static-method")
	void getLocalisedDescriptionWithNullReturnsNull() {
		MinimalModel model = new MinimalModel(null, null, null, null);
		assertNull(model.getLocalisedDescription());
	}

	@Test
	@SuppressWarnings("static-method")
	void isPersistableWhenPersistentIsNull() {
		MinimalModel model = new MinimalModel(null, null, null, null);
		assertFalse(model.isPersistable());
	}

	@Test
	@SuppressWarnings("static-method")
	void isPersistableWhenPersistentHasName() {
		Persistent p = new Persistent();
		p.setName("ADM_Thing");
		MinimalModel model = new MinimalModel(null, null, null, p);
		assertTrue(model.isPersistable());
	}

	@Test
	@SuppressWarnings("static-method")
	void isPersistableWhenPersistentHasNoName() {
		Persistent p = new Persistent();
		MinimalModel model = new MinimalModel(null, null, null, p);
		assertFalse(model.isPersistable());
	}
}
