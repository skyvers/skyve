package modules.test;

import org.junit.Test;
import org.skyve.util.Util;

import junit.framework.Assert;
import modules.test.AllAttributesPersistent.AllAttributesPersistentExtension;
import modules.test.domain.AllAttributesPersistent;

public class InjectionTest extends AbstractSkyveTest {
	@Test
	public void testValidateBeanAgainstDocument() throws Exception {
		AllAttributesPersistentExtension test = Util.constructRandomInstance(u, m, aapd, 2);
		Assert.assertNotNull(test.getPersistence());
		test = test.getPersistence().save(test);
		Assert.assertNotNull(test.getPersistence());
	}
}
