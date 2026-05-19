package org.skyve.metadata.model;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.model.Persistent.ExtensionStrategy;

class PersistentTest {

	// ---- determinePersistentIdentifier (static) ----

	@Test
	@SuppressWarnings("static-method")
	void determineIdentifierNullNameReturnsNull() {
		assertNull(Persistent.determinePersistentIdentifier(null, null, null));
	}

	@Test
	@SuppressWarnings("static-method")
	void determineIdentifierNameOnlyReturnName() {
		assertThat(Persistent.determinePersistentIdentifier("ADM_User", null, null), is("ADM_User"));
	}

	@Test
	@SuppressWarnings("static-method")
	void determineIdentifierWithSchemaOnly() {
		assertThat(Persistent.determinePersistentIdentifier("ADM_User", null, "dbo"), is("dbo.ADM_User"));
	}

	@Test
	@SuppressWarnings("static-method")
	void determineIdentifierWithCatalogOnly() {
		assertThat(Persistent.determinePersistentIdentifier("ADM_User", "mycat", null), is("mycat.ADM_User"));
	}

	@Test
	@SuppressWarnings("static-method")
	void determineIdentifierWithCatalogAndSchema() {
		assertThat(Persistent.determinePersistentIdentifier("ADM_User", "mycat", "dbo"), is("mycat.dbo.ADM_User"));
	}

	// ---- getters/setters ----

	@Test
	@SuppressWarnings("static-method")
	void defaultConstructorHasNullFields() {
		Persistent p = new Persistent();
		assertNull(p.getName());
		assertNull(p.getSchema());
		assertNull(p.getCatalog());
		assertNull(p.getStrategy());
		assertNull(p.getDiscriminator());
		assertNull(p.getCacheName());
	}

	@Test
	@SuppressWarnings("static-method")
	void setSchemaAndGetSchema() {
		Persistent p = new Persistent();
		p.setSchema("dbo");
		assertThat(p.getSchema(), is("dbo"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setCatalogAndGetCatalog() {
		Persistent p = new Persistent();
		p.setCatalog("mycat");
		assertThat(p.getCatalog(), is("mycat"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setStrategyAndGetStrategy() {
		Persistent p = new Persistent();
		p.setStrategy(ExtensionStrategy.joined);
		assertThat(p.getStrategy(), is(ExtensionStrategy.joined));
	}

	@Test
	@SuppressWarnings("static-method")
	void setDiscriminatorAndGetDiscriminator() {
		Persistent p = new Persistent();
		p.setDiscriminator("DISC");
		assertThat(p.getDiscriminator(), is("DISC"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setCacheNameAndGetCacheName() {
		Persistent p = new Persistent();
		p.setCacheName("region1");
		assertThat(p.getCacheName(), is("region1"));
	}

	// ---- isPolymorphicallyMapped ----

	@Test
	@SuppressWarnings("static-method")
	void isPolymorphicallyMappedFalseByDefault() {
		Persistent p = new Persistent();
		assertFalse(p.isPolymorphicallyMapped());
	}

	@Test
	@SuppressWarnings("static-method")
	void isPolymorphicallyMappedFalseWhenMappedStrategyButHasName() {
		Persistent p = new Persistent();
		p.setName("SomeName");
		p.setStrategy(ExtensionStrategy.mapped);
		assertFalse(p.isPolymorphicallyMapped());
	}

	@Test
	@SuppressWarnings("static-method")
	void isPolymorphicallyMappedTrueWhenMappedStrategyAndNullName() {
		Persistent p = new Persistent();
		p.setStrategy(ExtensionStrategy.mapped);
		// name is null by default → polymorphic
		assertTrue(p.isPolymorphicallyMapped());
	}

	@Test
	@SuppressWarnings("static-method")
	void isPolymorphicallyMappedFalseForSingleStrategy() {
		Persistent p = new Persistent();
		p.setStrategy(ExtensionStrategy.single);
		assertFalse(p.isPolymorphicallyMapped());
	}

	@Test
	@SuppressWarnings("static-method")
	void isPolymorphicallyMappedFalseForJoinedStrategy() {
		Persistent p = new Persistent();
		p.setStrategy(ExtensionStrategy.joined);
		assertFalse(p.isPolymorphicallyMapped());
	}

	// ---- getPersistentIdentifier ----

	@Test
	@SuppressWarnings("static-method")
	void persistentIdentifierNullWhenNoName() {
		Persistent p = new Persistent();
		assertNull(p.getPersistentIdentifier());
	}

	@Test
	@SuppressWarnings("static-method")
	void persistentIdentifierNameOnlyWhenNoCatalogOrSchema() {
		Persistent p = new Persistent();
		p.setName("ADM_User");
		// UtilImpl.CATALOG and UtilImpl.SCHEMA default to null
		assertThat(p.getPersistentIdentifier(), is("ADM_User"));
	}

	// ---- getAgnosticIdentifier ----

	@Test
	@SuppressWarnings("static-method")
	void agnosticIdentifierNullWhenNoName() {
		Persistent p = new Persistent();
		assertNull(p.getAgnosticIdentifier());
	}

	@Test
	@SuppressWarnings("static-method")
	void agnosticIdentifierUsesOnlyMetadataSchemaNotUtilImpl() {
		Persistent p = new Persistent();
		p.setName("ADM_User");
		// no schema/catalog set → should just be the name (not UtilImpl.SCHEMA)
		assertThat(p.getAgnosticIdentifier(), is("ADM_User"));
	}

	@Test
	@SuppressWarnings("static-method")
	void agnosticIdentifierWithExplicitSchema() {
		Persistent p = new Persistent();
		p.setName("ADM_User");
		p.setSchema("audit");
		assertThat(p.getAgnosticIdentifier(), is("audit.ADM_User"));
	}

	// ---- ExtensionStrategy enum ----

	@Test
	@SuppressWarnings("static-method")
	void extensionStrategyHasThreeValues() {
		assertEquals(3, ExtensionStrategy.values().length);
	}

	private static void assertEquals(int expected, int actual) {
		org.junit.jupiter.api.Assertions.assertEquals(expected, actual);
	}
}
