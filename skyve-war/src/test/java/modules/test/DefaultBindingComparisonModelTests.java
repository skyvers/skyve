package modules.test;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Assertions;
import org.skyve.metadata.view.model.comparison.ComparisonModel;
import org.skyve.metadata.view.model.comparison.DefaultBindingComparisonModel;
import org.skyve.util.Util;

import modules.test.domain.AllAttributesPersistent;

@SuppressWarnings("java:S5976")
class DefaultBindingComparisonModelTests extends AbstractSkyveTest {
	@Test
	void testSimpleGeneration() throws Exception {
		AllAttributesPersistent now = Assertions.assertDoesNotThrow(() -> Util.constructRandomInstance(u, m, aapd, 1));
		AllAttributesPersistent then = Util.constructRandomInstance(u, m, aapd, 1);
		ComparisonModel<AllAttributesPersistent, AllAttributesPersistent> model = new DefaultBindingComparisonModel<>(aapd, then, null);
		model.postConstruct(c, true);
		model.getComparisonComposite(now);
	}

	@Test
	void testNestedGeneration() throws Exception {
		AllAttributesPersistent now = Assertions.assertDoesNotThrow(() -> Util.constructRandomInstance(u, m, aapd, 4));
		AllAttributesPersistent then = Util.constructRandomInstance(u, m, aapd, 4);
		ComparisonModel<AllAttributesPersistent, AllAttributesPersistent> model = new DefaultBindingComparisonModel<>(aapd, then, null);
		model.postConstruct(c, true);
		model.getComparisonComposite(now);
	}

	@Test
	void testAsymmetricNestedGeneration() throws Exception {
		AllAttributesPersistent now = Assertions.assertDoesNotThrow(() -> Util.constructRandomInstance(u, m, aapd, 4));
		AllAttributesPersistent then = Util.constructRandomInstance(u, m, aapd, 5);
		ComparisonModel<AllAttributesPersistent, AllAttributesPersistent> model = new DefaultBindingComparisonModel<>(aapd, then, null);
		model.postConstruct(c, true);
		model.getComparisonComposite(now);
	}
}
