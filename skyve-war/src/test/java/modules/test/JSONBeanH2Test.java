package modules.test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.util.JSON;
import org.skyve.util.Util;

import modules.test.domain.AllAttributesPersistent;
import modules.test.domain.AllAttributesRequiredPersistent;

/**
 * H2-backed tests for {@link JSON} marshalling and unmarshalling
 * via {@link org.skyve.impl.util.json.JSONWriter} and {@link org.skyve.impl.util.json.JSONReader}.
 *
 * <p>These tests exercise the full {@code document()} write path (which requires a Customer)
 * and the {@code bean} read mode (which requires a User with a Customer).
 */
class JSONBeanH2Test extends AbstractSkyveTest {

	// ---- marshall ----------------------------------------------------------------

	@Test
	void marshallBeanProducesBizModuleKey() throws Exception {
		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 1);
		String json = JSON.marshall(c, bean);
		assertNotNull(json);
		assertTrue(json.contains("\"bizModule\""), "JSON must contain bizModule key");
	}

	@Test
	void marshallBeanProducesBizDocumentKey() throws Exception {
		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 1);
		String json = JSON.marshall(c, bean);
		assertTrue(json.contains("\"bizDocument\""), "JSON must contain bizDocument key");
	}

	@Test
	void marshallBeanProducesModuleName() throws Exception {
		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 1);
		String json = JSON.marshall(c, bean);
		assertTrue(json.contains(AllAttributesPersistent.MODULE_NAME));
	}

	@Test
	void marshallBeanProducesDocumentName() throws Exception {
		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 1);
		String json = JSON.marshall(c, bean);
		assertTrue(json.contains(AllAttributesPersistent.DOCUMENT_NAME));
	}

	@Test
	void marshallBeanContainsBizId() throws Exception {
		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 1);
		String json = JSON.marshall(c, bean);
		assertTrue(json.contains(bean.getBizId()));
	}

	@Test
	void marshallBeanProducesValidJsonStructure() throws Exception {
		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 1);
		String json = JSON.marshall(c, bean);
		assertTrue(json.startsWith("{") && json.endsWith("}"),
				"JSON should be a JSON object");
	}

	@Test
	void marshallRequiredAttributesBeanProducesJson() throws Exception {
		AllAttributesRequiredPersistent bean = Util.constructRandomInstance(u, m, aarpd, 1);
		String json = JSON.marshall(c, bean);
		assertNotNull(json);
		assertTrue(json.contains("\"bizModule\""));
	}

	// ---- roundtrip ---------------------------------------------------------------

	@Test
	void unmarshallMarshalledBeanReturnsBeanWithSameDocument() throws Exception {
		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 1);
		String json = JSON.marshall(c, bean);

		Object result = JSON.unmarshall(u, json);
		assertNotNull(result);
		assertTrue(result instanceof Bean, "Unmarshalled result should be a Bean");
		Bean resultBean = (Bean) result;
		assertEquals(AllAttributesPersistent.DOCUMENT_NAME, resultBean.getBizDocument());
	}

	@Test
	void unmarshallMarshalledBeanReturnsBeanWithSameModule() throws Exception {
		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 1);
		String json = JSON.marshall(c, bean);

		Bean resultBean = (Bean) JSON.unmarshall(u, json);
		assertEquals(AllAttributesPersistent.MODULE_NAME, resultBean.getBizModule());
	}

	@Test
	void unmarshallMarshalledBeanReturnsBeanOfCorrectType() throws Exception {
		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 1);
		String json = JSON.marshall(c, bean);

		Object result = JSON.unmarshall(u, json);
		assertTrue(result instanceof AllAttributesPersistent);
	}

	@Test
	void unmarshallMarshalledBeanPreservesBizCustomer() throws Exception {
		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 1);
		String json = JSON.marshall(c, bean);

		Bean resultBean = (Bean) JSON.unmarshall(u, json);
		assertNotNull(resultBean.getBizCustomer());
	}
}
