package modules.admin.DocumentCreator;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

import modules.admin.domain.DocumentCreator;

@SuppressWarnings("static-method")
class DocumentCreatorBizletTest {

	private static final DocumentCreatorBizlet bizlet = new DocumentCreatorBizlet();

	@Test
	void newInstanceReturnsBeanWithoutError() throws Exception {
		DocumentCreator bean = new DocumentCreator();
		DocumentCreator result = bizlet.newInstance(bean);
		assertNotNull(result);
	}

	@Test
	void getVariantDomainValuesForUnknownAttributeReturnsNull() throws Exception {
		assertNull(bizlet.getVariantDomainValues("unknownAttribute"));
	}
}
