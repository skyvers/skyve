package modules.test;

import org.junit.Assert;
import org.junit.Test;
import org.skyve.util.Util;

import modules.test.InjectedDocument.InjectedDocumentExtension;
import modules.test.domain.InjectedDocument;

public class InjectionTest extends AbstractSkyveTest {
	@Test
	public void testInjectedOnCreateAfterSaveAndAfterLoad() throws Exception {
		InjectedDocumentExtension test = Util.constructRandomInstance(u, m, id, 1);
		Assert.assertNotNull(test.p);
		Assert.assertNotNull(test.c);
		Assert.assertNotNull(test.u);
		Assert.assertNotNull(test.s);
		Assert.assertNotNull(test.r);
		
		// Test the proxy classes resolve
		test.p.getUser();
		test.c.getName();
		test.u.getName();
		test.s.keySet();
		test.r.getRouter();
		
		test = test.p.save(test);
		Assert.assertNotNull(test.p);
		Assert.assertNotNull(test.c);
		Assert.assertNotNull(test.u);
		Assert.assertNotNull(test.s);
		Assert.assertNotNull(test.r);

		// Test the proxy classes resolve
		test.p.getUser();
		test.c.getName();
		test.u.getName();
		test.s.keySet();
		test.r.getRouter();

		test.p.evictAllCached();
		test = test.p.retrieve(InjectedDocument.MODULE_NAME, InjectedDocument.DOCUMENT_NAME, test.getBizId());
		Assert.assertNotNull(test.p);
		Assert.assertNotNull(test.c);
		Assert.assertNotNull(test.u);
		Assert.assertNotNull(test.s);
		Assert.assertNotNull(test.r);

		// Test the proxy classes resolve
		test.p.getUser();
		test.c.getName();
		test.u.getName();
		test.s.keySet();
		test.r.getRouter();
	}
	
	@Test
	public void testBizlet() throws Exception {
		Util.constructRandomInstance(u, m, id, 1);
		// if this succeeds then Bizlet.newInstance() passed its asserts
	}
	
	@Test
	public void testSerialzation() throws Exception {
		InjectedDocumentExtension test = Util.constructRandomInstance(u, m, id, 1);
		Assert.assertNotNull(test.p);
		Assert.assertNotNull(test.c);
		Assert.assertNotNull(test.u);
		Assert.assertNotNull(test.s);
		Assert.assertNotNull(test.r);
		
		// Test the proxy classes resolve
		test.p.getUser();
		test.c.getName();
		test.u.getName();
		test.s.keySet();
		test.r.getRouter();

		InjectedDocumentExtension clone = Util.cloneBySerialization(test);

		// Check that test still has its injected properties after clone
		Assert.assertNotNull(test.p);
		Assert.assertNotNull(test.c);
		Assert.assertNotNull(test.u);
		Assert.assertNotNull(test.s);
		Assert.assertNotNull(test.r);

		// Test the proxy classes resolve
		test.p.getUser();
		test.c.getName();
		test.u.getName();
		test.s.keySet();
		test.r.getRouter();

		// Check that the clone has injected properties via readResolve
		Assert.assertNotNull(clone.p);
		Assert.assertNotNull(clone.c);
		Assert.assertNotNull(clone.u);
		Assert.assertNotNull(clone.s);
		Assert.assertNotNull(clone.r);

		// Test the proxy classes resolve
		clone.p.getUser();
		clone.c.getName();
		clone.u.getName();
		clone.s.keySet();
		clone.r.getRouter();
	}
}
