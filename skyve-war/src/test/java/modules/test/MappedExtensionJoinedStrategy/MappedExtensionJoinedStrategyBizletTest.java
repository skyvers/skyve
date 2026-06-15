package modules.test.MappedExtensionJoinedStrategy;

import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class MappedExtensionJoinedStrategyBizletTest {

	private static final MappedExtensionJoinedStrategyBizlet bizlet = new MappedExtensionJoinedStrategyBizlet();

	@Test
	void preSaveSetsFlag() throws Exception {
		MappedExtensionJoinedStrategyExtension bean = new MappedExtensionJoinedStrategyExtension();
		bizlet.preSave(bean);
		assertTrue(bean.isPreSaveCalled());
	}

	@Test
	void postSaveSetsFlag() throws Exception {
		MappedExtensionJoinedStrategyExtension bean = new MappedExtensionJoinedStrategyExtension();
		bizlet.postSave(bean);
		assertTrue(bean.isPostSaveCalled());
	}

	@Test
	void preDeleteSetsFlag() throws Exception {
		MappedExtensionJoinedStrategyExtension bean = new MappedExtensionJoinedStrategyExtension();
		bizlet.preDelete(bean);
		assertTrue(bean.isPreDeleteCalled());
	}

	@Test
	void postDeleteSetsFlag() throws Exception {
		MappedExtensionJoinedStrategyExtension bean = new MappedExtensionJoinedStrategyExtension();
		bizlet.postDelete(bean);
		assertTrue(bean.isPostDeleteCalled());
	}
}
