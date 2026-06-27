package modules.test;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.skyve.util.Util;

import modules.test.InjectedDocument.InjectedDocumentExtension;
import modules.test.domain.InjectedDocument;

@SuppressWarnings("java:S1130")
class InjectionTest extends AbstractSkyveTest {

	@Test
	void testInjectedOnCreateAfterSaveAndAfterLoad() throws Exception {
		InjectedDocumentExtension test = Util.constructRandomInstance(u, m, id, 1);
		Assertions.assertNotNull(test.p);
		Assertions.assertNotNull(test.c);
		Assertions.assertNotNull(test.u);
		Assertions.assertNotNull(test.s);
		Assertions.assertNotNull(test.r);
		
		// Test the proxy classes resolve
		test.p.getUser();
		test.c.getName();
		test.u.getName();
		test.s.keySet();
		test.r.getRouter();
		
		test = test.p.save(test);
		Assertions.assertNotNull(test.p);
		Assertions.assertNotNull(test.c);
		Assertions.assertNotNull(test.u);
		Assertions.assertNotNull(test.s);
		Assertions.assertNotNull(test.r);

		// Test the proxy classes resolve
		test.p.getUser();
		test.c.getName();
		test.u.getName();
		test.s.keySet();
		test.r.getRouter();

		test.p.evictAllCached();
		test = test.p.retrieve(InjectedDocument.MODULE_NAME, InjectedDocument.DOCUMENT_NAME, test.getBizId());
		Assertions.assertNotNull(test);
		Assertions.assertNotNull(test.p);
		Assertions.assertNotNull(test.c);
		Assertions.assertNotNull(test.u);
		Assertions.assertNotNull(test.s);
		Assertions.assertNotNull(test.r);

		// Test the proxy classes resolve
		test.p.getUser();
		test.c.getName();
		test.u.getName();
		test.s.keySet();
		test.r.getRouter();
	}
	
	@Test
	void testBizlet() throws Exception {
		Assertions.assertDoesNotThrow(() -> Util.constructRandomInstance(u, m, id, 1));
		// if this succeeds then Bizlet.newInstance() passed its asserts
	}
	
	@Test
	void testSerialzation() throws Exception {
		InjectedDocumentExtension test = Util.constructRandomInstance(u, m, id, 1);
		Assertions.assertNotNull(test.p);
		Assertions.assertNotNull(test.c);
		Assertions.assertNotNull(test.u);
		Assertions.assertNotNull(test.s);
		Assertions.assertNotNull(test.r);
		
		// Test the proxy classes resolve
		test.p.getUser();
		test.c.getName();
		test.u.getName();
		test.s.keySet();
		test.r.getRouter();

		InjectedDocumentExtension clone = Util.cloneBySerialization(test);

		// Check that test still has its injected properties after clone
		Assertions.assertNotNull(test.p);
		Assertions.assertNotNull(test.c);
		Assertions.assertNotNull(test.u);
		Assertions.assertNotNull(test.s);
		Assertions.assertNotNull(test.r);

		// Test the proxy classes resolve
		test.p.getUser();
		test.c.getName();
		test.u.getName();
		test.s.keySet();
		test.r.getRouter();

		// Check that the clone has injected properties via readResolve
		Assertions.assertNotNull(clone.p);
		Assertions.assertNotNull(clone.c);
		Assertions.assertNotNull(clone.u);
		Assertions.assertNotNull(clone.s);
		Assertions.assertNotNull(clone.r);

		// Test the proxy classes resolve
		clone.p.getUser();
		clone.c.getName();
		clone.u.getName();
		clone.s.keySet();
		clone.r.getRouter();
	}
}
