package modules.test;

import org.junit.Assert;
import org.junit.Test;
import org.skyve.domain.HierarchicalBean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.PolymorphicPersistentBean;

import modules.test.MappedExtensionJoinedStrategy.MappedExtensionJoinedStrategyExtension;
import modules.test.MappedExtensionSingleStrategy.MappedExtensionSingleStrategyExtension;
import modules.test.domain.AllAttributesPersistent;
import modules.test.domain.Hierarchical;
import modules.test.domain.MappedBase;
import modules.test.domain.MappedExtensionJoinedStrategy;
import modules.test.domain.MappedExtensionSingleStrategy;
import modules.test.domain.MappedSubclassedJoinedStrategy;
import modules.test.domain.MappedSubclassedSingleStrategy;

public class GenerationTests extends AbstractSkyveTest {
	@Test
	@SuppressWarnings({ "cast", "static-method" })
	public void testGenerateExtensionHierarchy() {
		MappedBase mb = new MappedBase();
		MappedExtensionSingleStrategy mess = MappedExtensionSingleStrategy.newInstance();
		MappedExtensionJoinedStrategy mejs = MappedExtensionJoinedStrategy.newInstance();
		MappedSubclassedSingleStrategy msss = new MappedSubclassedSingleStrategy();
		MappedSubclassedJoinedStrategy msjs = new MappedSubclassedJoinedStrategy();
		Assert.assertTrue(new AllAttributesPersistent() instanceof PersistentBean);
		Assert.assertTrue(new Hierarchical() instanceof HierarchicalBean<?>);
		Assert.assertTrue(mb instanceof PersistentBean);
		Assert.assertFalse(MappedBase.class.isAnnotationPresent(PolymorphicPersistentBean.class)); // mapped bean
		Assert.assertTrue(mejs instanceof MappedBase);
		Assert.assertTrue(MappedExtensionJoinedStrategy.class.isAnnotationPresent(PolymorphicPersistentBean.class)); // joined strategy
		Assert.assertTrue(mess instanceof MappedBase);
		Assert.assertTrue(MappedExtensionSingleStrategy.class.isAnnotationPresent(PolymorphicPersistentBean.class)); // single strategy
		Assert.assertTrue(msjs instanceof MappedExtensionJoinedStrategyExtension);
		Assert.assertFalse(MappedExtensionJoinedStrategyExtension.class.isAnnotationPresent(PolymorphicPersistentBean.class)); // no subclasses
		Assert.assertTrue(msss instanceof MappedExtensionSingleStrategyExtension);
		Assert.assertFalse(MappedExtensionSingleStrategyExtension.class.isAnnotationPresent(PolymorphicPersistentBean.class)); // no subclasses
	}
}
