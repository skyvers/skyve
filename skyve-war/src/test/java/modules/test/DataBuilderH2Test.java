package modules.test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.test.domain.AllAttributesPersistent;

public class DataBuilderH2Test extends AbstractSkyveTest {

	@Test
	public void testDefaultCrudFixtureBuildReturnsBean() {
		AllAttributesPersistent bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME);

		assertNotNull(bean);
	}

	@Test
	public void testSeedAndSailFixtureBuildsReturnBean() {
		AllAttributesPersistent seedBean = new DataBuilder()
				.fixture(FixtureType.seed)
				.build(AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME);
		AllAttributesPersistent sailBean = new DataBuilder()
				.fixture(FixtureType.sail)
				.build(AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME);

		assertNotNull(seedBean);
		assertNotNull(sailBean);
	}

	@Test
	public void testChainedConfigurationBuildsWithConfiguredCardinality() {
		AllAttributesPersistent bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.required(true, true)
				.optional(true, true)
				.depth(2)
				.depth(AllAttributesPersistent.aggregatedCollectionPropertyName, 1)
				.name(AllAttributesPersistent.textPropertyName, false)
				.type(AttributeType.bool, false)
				.cardinality(AllAttributesPersistent.aggregatedCollectionPropertyName, 2)
				.build(AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME);

		assertNotNull(bean);
		assertNotNull(bean.getAggregatedCollection());
		assertEquals(2, bean.getAggregatedCollection().size());
	}
}
