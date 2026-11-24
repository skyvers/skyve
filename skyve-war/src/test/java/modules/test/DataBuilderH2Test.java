package modules.test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.test.domain.AllAttributesPersistent;
import modules.test.domain.TestDocument;

@SuppressWarnings("static-method")
class DataBuilderH2Test extends AbstractSkyveTest {

	@Test
	void testDefaultCrudFixtureBuildReturnsBean() {
		AllAttributesPersistent bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(TestDocument.ALL_ATTRIBUTES_PERSISTENT);

		assertNotNull(bean);
	}

	@Test
	void testSeedAndSailFixtureBuildsReturnBean() {
		AllAttributesPersistent seedBean = new DataBuilder()
				.fixture(FixtureType.seed)
				.build(TestDocument.ALL_ATTRIBUTES_PERSISTENT);
		AllAttributesPersistent sailBean = new DataBuilder()
				.fixture(FixtureType.sail)
				.build(TestDocument.ALL_ATTRIBUTES_PERSISTENT);

		assertNotNull(seedBean);
		assertNotNull(sailBean);
	}

	@Test
	void testChainedConfigurationBuildsWithConfiguredCardinality() {
		AllAttributesPersistent bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.required(true, true)
				.optional(true, true)
				.depth(2)
				.depth(AllAttributesPersistent.aggregatedCollectionPropertyName, 1)
				.name(AllAttributesPersistent.textPropertyName, false)
				.type(AttributeType.bool, false)
				.cardinality(AllAttributesPersistent.aggregatedCollectionPropertyName, 2)
				.build(TestDocument.ALL_ATTRIBUTES_PERSISTENT);

		assertNotNull(bean);
		assertNotNull(bean.getAggregatedCollection());
		assertEquals(2, bean.getAggregatedCollection().size());
	}
}
