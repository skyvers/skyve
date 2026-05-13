package org.skyve.metadata.model;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.Attribute.UsageType;
import org.skyve.metadata.model.Persistent.ExtensionStrategy;

@SuppressWarnings("static-method")
class ModelMetaDataTest {

	// ---- Extends ----

	@Test
	void extendsGetSetDocumentName() {
		Extends e = new Extends();
		e.setDocumentName("BaseDocument");
		assertThat(e.getDocumentName(), is("BaseDocument"));
	}

	@Test
	void extendsDocumentNameNullByDefault() {
		Extends e = new Extends();
		assertThat(e.getDocumentName(), is(nullValue()));
	}

	@Test
	void extendsSetDocumentNameTrimsBlankToNull() {
		Extends e = new Extends();
		e.setDocumentName("   ");
		assertThat(e.getDocumentName(), is(nullValue()));
	}

	// ---- Persistent ----

	@Test
	void persistentDefaultConstructor() {
		Persistent p = new Persistent();
		assertThat(p, is(notNullValue()));
	}

	@Test
	void persistentGetSetSchema() {
		Persistent p = new Persistent();
		p.setSchema("public");
		assertThat(p.getSchema(), is("public"));
	}

	@Test
	void persistentSchemaNullByDefault() {
		Persistent p = new Persistent();
		assertThat(p.getSchema(), is(nullValue()));
	}

	@Test
	void persistentSetSchemaBlankBecomesNull() {
		Persistent p = new Persistent();
		p.setSchema("  ");
		assertThat(p.getSchema(), is(nullValue()));
	}

	@Test
	void persistentGetSetCatalog() {
		Persistent p = new Persistent();
		p.setCatalog("myCatalog");
		assertThat(p.getCatalog(), is("myCatalog"));
	}

	@Test
	void persistentCatalogNullByDefault() {
		Persistent p = new Persistent();
		assertThat(p.getCatalog(), is(nullValue()));
	}

	@Test
	void persistentGetSetStrategy() {
		Persistent p = new Persistent();
		p.setStrategy(ExtensionStrategy.joined);
		assertThat(p.getStrategy(), is(ExtensionStrategy.joined));
	}

	@Test
	void persistentStrategyNullByDefault() {
		Persistent p = new Persistent();
		assertThat(p.getStrategy(), is(nullValue()));
	}

	@Test
	void persistentGetSetDiscriminator() {
		Persistent p = new Persistent();
		p.setDiscriminator("type");
		assertThat(p.getDiscriminator(), is("type"));
	}

	@Test
	void persistentDiscriminatorNullByDefault() {
		Persistent p = new Persistent();
		assertThat(p.getDiscriminator(), is(nullValue()));
	}

	@Test
	void persistentExtensionStrategyValues() {
		assertThat(ExtensionStrategy.values().length > 0, is(true));
	}

	@Test
	void persistentExtensionStrategySingle() {
		assertThat(ExtensionStrategy.single, is(notNullValue()));
	}

	@Test
	void persistentExtensionStrategyMapped() {
		assertThat(ExtensionStrategy.mapped, is(notNullValue()));
	}

	// ---- AttributeType ----

	@Test
	void attributeTypeTextNotNull() {
		assertThat(AttributeType.text, is(notNullValue()));
	}

	@Test
	void attributeTypeDateNotNull() {
		assertThat(AttributeType.date, is(notNullValue()));
	}

	@Test
	void attributeTypeBoolNotNull() {
		assertThat(AttributeType.bool, is(notNullValue()));
	}

	@Test
	void attributeTypeEnumerationNotNull() {
		assertThat(AttributeType.enumeration, is(notNullValue()));
	}

	@Test
	void attributeTypeAssociationNotNull() {
		assertThat(AttributeType.association, is(notNullValue()));
	}

	@Test
	void attributeTypeValuesNotEmpty() {
		assertThat(AttributeType.values().length > 0, is(true));
	}

	// ---- UsageType ----

	@Test
	void usageTypeDomainNotNull() {
		assertThat(UsageType.domain, is(notNullValue()));
	}

	@Test
	void usageTypeViewNotNull() {
		assertThat(UsageType.view, is(notNullValue()));
	}

	@Test
	void usageTypeBothNotNull() {
		assertThat(UsageType.both, is(notNullValue()));
	}
}
