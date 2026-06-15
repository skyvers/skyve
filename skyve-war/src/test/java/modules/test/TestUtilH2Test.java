package modules.test;

import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.util.List;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.model.Attribute;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.TestUtil;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.test.domain.AllAttributesPersistent;
import modules.test.domain.AllAttributesRequiredPersistent;

/**
 * H2-backed tests for {@link TestUtil} methods that need a live Skyve session.
 */
class TestUtilH2Test extends AbstractSkyveTest {

	// ---- constructRandomInstance -----------------------------------------

	@Test
	void constructRandomInstanceReturnsNonNullBean() throws Exception {
		AllAttributesPersistent bean = TestUtil.constructRandomInstance(u, m, aapd, 1);
		assertNotNull(bean);
	}

	@Test
	void constructRandomInstanceDepthTwoReturnsBean() throws Exception {
		AllAttributesPersistent bean = TestUtil.constructRandomInstance(u, m, aapd, 2);
		assertNotNull(bean);
	}

	@Test
	void constructRandomInstanceForRequiredDocumentReturnsBean() throws Exception {
		AllAttributesRequiredPersistent bean = TestUtil.constructRandomInstance(u, m, aarpd, 1);
		assertNotNull(bean);
	}

	// ---- updateAttribute -------------------------------------------------

	@Test
	void updateAttributeTextAttributeChangesValue() throws Exception {
		AllAttributesPersistent bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME);
		assertNotNull(bean);

		// Find the 'text' attribute from the document
		Attribute textAttr = aapd.getAttributes().stream()
				.filter(a -> AllAttributesPersistent.textPropertyName.equals(a.getName()))
				.findFirst()
				.orElse(null);
		assertNotNull(textAttr);

		AllAttributesPersistent updated = TestUtil.updateAttribute(bean, textAttr);
		assertNotNull(updated);
	}

	@Test
	void updateAttributeWithModuleDocumentDoesNotThrow() throws Exception {
		AllAttributesPersistent bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME);
		assertNotNull(bean);

		Attribute textAttr = aapd.getAttributes().stream()
				.filter(a -> AllAttributesPersistent.textPropertyName.equals(a.getName()))
				.findFirst()
				.orElse(null);
		assertNotNull(textAttr);

		AllAttributesPersistent updated = TestUtil.updateAttribute(m, aapd, bean, textAttr);
		assertNotNull(updated);
	}

	@SuppressWarnings("static-method")
	@Test
	void updateAttributeNullAttributeReturnsOriginalBean() throws Exception {
		AllAttributesPersistent bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME);
		assertNotNull(bean);

		AllAttributesPersistent result = TestUtil.updateAttribute(bean, null);
		assertNotNull(result);
	}

	// ---- retrieveExcludedUpdateAttributes --------------------------------

	@Test
	void retrieveExcludedUpdateAttributesReturnsListForDocumentWithNoFactory() {
		// AllAttributesPersistent has no SkyveFactory annotation
		List<String> excluded = TestUtil.retrieveExcludedUpdateAttributes(m, aapd);
		assertNotNull(excluded);
	}

	@Test
	void retrieveExcludedUpdateAttributesDoesNotThrowForUnknownDocument() {
		// Using aarpd (AllAttributesRequiredPersistent) which also has no @SkyveFactory
		List<String> excluded = TestUtil.retrieveExcludedUpdateAttributes(m, aarpd);
		assertNotNull(excluded);
	}
}
