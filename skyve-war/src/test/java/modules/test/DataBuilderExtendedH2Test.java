package modules.test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.test.domain.AllAttributesPersistent;
import modules.test.domain.AllAttributesRequiredPersistent;

/**
 * Additional H2-backed tests for {@link DataBuilder} covering the API surface
 * not exercised by {@link DataBuilderH2Test}.
 */
@SuppressWarnings("static-method")
class DataBuilderExtendedH2Test extends AbstractSkyveTest {

	// ---- fixture overloads -----------------------------------------------

	@Test
	void fixtureStringBuildsBean() {
		AllAttributesPersistent bean = new DataBuilder()
				.fixture("crud")
				.build(AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME);
		assertNotNull(bean);
	}

	// ---- build overloads --------------------------------------------------

	@Test
	void buildModuleDocumentOverloadReturnsBean() {
		AllAttributesPersistent bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(m, aapd);
		assertNotNull(bean);
	}

	@Test
	void buildDocumentOnlyOverloadReturnsBean() {
		AllAttributesPersistent bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(aapd);
		assertNotNull(bean);
	}

	@Test
	void factoryBuildStringStringReturnsBean() {
		AllAttributesPersistent bean = new DataBuilder()
				.factoryBuild(AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME);
		assertNotNull(bean);
	}

	@Test
	void factoryBuildModuleDocumentReturnsBean() {
		AllAttributesPersistent bean = new DataBuilder()
				.factoryBuild(m, aapd);
		assertNotNull(bean);
	}

	@Test
	void factoryBuildDocumentReturnsBean() {
		AllAttributesPersistent bean = new DataBuilder()
				.factoryBuild(aapd);
		assertNotNull(bean);
	}

	// ---- attribute inclusion flags ----------------------------------------

	@Test
	void persistentFalseExcludesAttributesBuildStillReturnsBean() {
		AllAttributesPersistent bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.persistent(false)
				.build(AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME);
		assertNotNull(bean);
	}

	@Test
	void persistentTrueIncludesAttributesBuildStillReturnsBean() {
		AllAttributesPersistent bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.persistent(true)
				.build(AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME);
		assertNotNull(bean);
	}

	@Test
	void transientsFalseExcludesTransientsBuildStillReturnsBean() {
		AllAttributesPersistent bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.transients(false)
				.build(AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME);
		assertNotNull(bean);
	}

	@Test
	void viewFalseExcludesViewAttributesBuildStillReturnsBean() {
		AllAttributesPersistent bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.view(false)
				.build(AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME);
		assertNotNull(bean);
	}

	@Test
	void domainFalseExcludesDomainAttributesBuildStillReturnsBean() {
		AllAttributesPersistent bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.domain(false)
				.build(AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME);
		assertNotNull(bean);
	}

	@Test
	void deprecatedFalseExcludesDeprecatedAttributesBuildStillReturnsBean() {
		AllAttributesPersistent bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.deprecated(false)
				.build(AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME);
		assertNotNull(bean);
	}

	// ---- required / optional overloads ------------------------------------

	@Test
	void requiredSingleBoolBuildReturnsBean() {
		AllAttributesPersistent bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.required(true)
				.build(AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME);
		assertNotNull(bean);
	}

	@Test
	void optionalSingleBoolBuildReturnsBean() {
		AllAttributesPersistent bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.required(true)
				.optional(true)
				.depth(1)
				.build(AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME);
		assertNotNull(bean);
	}

	@Test
	void requiredTwoArgBuildReturnsBean() {
		AllAttributesPersistent bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.required(true, false)
				.build(AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME);
		assertNotNull(bean);
	}

	@Test
	void optionalTwoArgBuildReturnsBean() {
		AllAttributesPersistent bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.required(true, true)
				.optional(false, true)
				.depth(1)
				.build(AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME);
		assertNotNull(bean);
	}

	// ---- type filtering --------------------------------------------------

	@Test
	void typeFilterExcludesBoolReturnsBean() {
		AllAttributesPersistent bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.type(AttributeType.bool, false)
				.build(AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME);
		assertNotNull(bean);
		// boolean attribute excluded — should be null/false default
		assertNotEquals(Boolean.TRUE, bean.getBooleanFlag());
	}

	@Test
	void typeFilterExcludesTextReturnsBean() {
		AllAttributesPersistent bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.type(AttributeType.text, false)
				.build(AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME);
		assertNotNull(bean);
		assertNull(bean.getText());
	}

	// ---- name filtering --------------------------------------------------

	@Test
	void nameFilterExcludesTextAttributeReturnsBean() {
		AllAttributesPersistent bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.name(AllAttributesPersistent.textPropertyName, false)
				.build(AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME);
		assertNotNull(bean);
		assertNull(bean.getText());
	}

	// ---- cardinality / depth ---------------------------------------------

	@Test
	void cardinalityThreeBuildsCorrectCollectionSize() {
		AllAttributesPersistent bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.required(true, true)
				.optional(true, true)
				.depth(2)
				.depth(AllAttributesPersistent.aggregatedCollectionPropertyName, 1)
				.cardinality(AllAttributesPersistent.aggregatedCollectionPropertyName, 3)
				.build(AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME);
		assertNotNull(bean);
		assertNotNull(bean.getAggregatedCollection());
		assertEquals(3, bean.getAggregatedCollection().size());
	}

	@Test
	void depthZeroStillBuildsBean() {
		AllAttributesPersistent bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.depth(0)
				.build(AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME);
		assertNotNull(bean);
	}

	@Test
	void bindingDepthZeroStillBuildsBean() {
		AllAttributesPersistent bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.depth(AllAttributesPersistent.aggregatedCollectionPropertyName, 0)
				.build(AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME);
		assertNotNull(bean);
	}

	// ---- setTrace does not throw -----------------------------------------

	@Test
	void setTraceDoesNotThrow() {
		DataBuilder.setTrace(true);
		AllAttributesPersistent bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME);
		DataBuilder.setTrace(false);
		assertNotNull(bean);
	}

	// ---- required document -----------------------------------------------

	@Test
	void buildAllRequiredAttributesDocumentReturnsBean() {
		AllAttributesRequiredPersistent bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.required(true, true)
				.optional(true, true)
				.depth(1)
				.build(AllAttributesRequiredPersistent.MODULE_NAME, AllAttributesRequiredPersistent.DOCUMENT_NAME);
		assertNotNull(bean);
	}
}
