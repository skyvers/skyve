package modules.test;

import org.junit.Assert;
import org.junit.Test;
import org.skyve.domain.HierarchicalBean;
import org.skyve.domain.PersistentBean;

import modules.test.MappedExtensionJoinedStrategy.MappedExtensionJoinedStrategyExtension;
import modules.test.MappedExtensionSingleStrategy.MappedExtensionSingleStrategyExtension;
import modules.test.domain.AllAttributesPersistent;
import modules.test.domain.Hierarchical;
import modules.test.domain.MappedBase;
import modules.test.domain.MappedExtensionJoinedStrategy;
import modules.test.domain.MappedExtensionSingleStrategy;
import modules.test.domain.MappedSubclassedJoinedStrategy;
import modules.test.domain.MappedSubclassedSingleStrategy;

public class GenerationTests extends AbstractH2Test {
	@Test
	@SuppressWarnings({"cast", "static-method"})
	public void testGenerateExtensionHierarchy() {
		Assert.assertTrue(new AllAttributesPersistent() instanceof PersistentBean);
		Assert.assertTrue(new Hierarchical() instanceof HierarchicalBean<?>);
		Assert.assertTrue(new MappedBase() instanceof PersistentBean);
		Assert.assertTrue(new MappedExtensionJoinedStrategy() instanceof MappedBase);
		Assert.assertTrue(new MappedExtensionSingleStrategy() instanceof MappedBase);
		Assert.assertTrue(new MappedSubclassedJoinedStrategy() instanceof MappedExtensionJoinedStrategyExtension);
		Assert.assertTrue(new MappedSubclassedSingleStrategy() instanceof MappedExtensionSingleStrategyExtension);
	}
}
