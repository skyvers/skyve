package modules.test;

import org.junit.Test;
import org.skyve.metadata.view.model.comparison.DefaultBindingComparisonModel;
import org.skyve.util.Util;

import modules.test.domain.AllAttributesPersistent;
import util.AbstractSkyveTest;

public class DefaultBindingComparisonModelTests extends AbstractSkyveTest {
	@Test
	public void testSimpleGeneration() throws Exception {
		AllAttributesPersistent now = Util.constructRandomInstance(u, m, aapd, 1);
		AllAttributesPersistent then = Util.constructRandomInstance(u, m, aapd, 1);
		new DefaultBindingComparisonModel<>(c, aapd, then, null).getComparisonComposite(now);
	}

	@Test
	public void testNestedGeneration() throws Exception {
		AllAttributesPersistent now = Util.constructRandomInstance(u, m, aapd, 4);
		AllAttributesPersistent then = Util.constructRandomInstance(u, m, aapd, 4);
		new DefaultBindingComparisonModel<>(c, aapd, then, null).getComparisonComposite(now);
	}

	@Test
	public void testAsymmetricNestedGeneration() throws Exception {
		AllAttributesPersistent now = Util.constructRandomInstance(u, m, aapd, 4);
		AllAttributesPersistent then = Util.constructRandomInstance(u, m, aapd, 5);
		new DefaultBindingComparisonModel<>(c, aapd, then, null).getComparisonComposite(now);
	}
}
