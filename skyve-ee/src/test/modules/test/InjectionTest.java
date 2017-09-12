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
		Assert.assertNotNull(test.con);
		Assert.assertNotNull(test.cm);
		Assert.assertNotNull(test.sda);
		
		test = test.p.save(test);
		Assert.assertNotNull(test.p);
		Assert.assertNotNull(test.c);
		Assert.assertNotNull(test.u);
		Assert.assertNotNull(test.s);
		Assert.assertNotNull(test.r);
		Assert.assertNotNull(test.con);
		Assert.assertNotNull(test.cm);
		Assert.assertNotNull(test.sda);
		
		test.p.evictAllCached();
		test = test.p.retrieve(InjectedDocument.MODULE_NAME, InjectedDocument.DOCUMENT_NAME, test.getBizId(), false);
		Assert.assertNotNull(test.p);
		Assert.assertNotNull(test.c);
		Assert.assertNotNull(test.u);
		Assert.assertNotNull(test.s);
		Assert.assertNotNull(test.r);
		Assert.assertNotNull(test.con);
		Assert.assertNotNull(test.cm);
		Assert.assertNotNull(test.sda);
	}
	
	@Test
	public void testBizlet() throws Exception {
		Util.constructRandomInstance(u, m, id, 1);
		// if this succeeds then Bizlet.newInstance() passed its asserts
	}
}
