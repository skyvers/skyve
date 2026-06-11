package modules.test.MappedExtensionJoinedStrategy;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class MappedExtensionJoinedStrategyExtensionTest {

	@Test
	void allFlagsDefaultToFalse() {
		MappedExtensionJoinedStrategyExtension bean = new MappedExtensionJoinedStrategyExtension();
		assertFalse(bean.isPreSaveCalled());
		assertFalse(bean.isPostSaveCalled());
		assertFalse(bean.isPreDeleteCalled());
		assertFalse(bean.isPostDeleteCalled());
	}

	@Test
	void setPreSaveCalledToTrue() {
		MappedExtensionJoinedStrategyExtension bean = new MappedExtensionJoinedStrategyExtension();
		bean.setPreSaveCalled(true);
		assertTrue(bean.isPreSaveCalled());
	}

	@Test
	void setPostSaveCalledToTrue() {
		MappedExtensionJoinedStrategyExtension bean = new MappedExtensionJoinedStrategyExtension();
		bean.setPostSaveCalled(true);
		assertTrue(bean.isPostSaveCalled());
	}

	@Test
	void setPreDeleteCalledToTrue() {
		MappedExtensionJoinedStrategyExtension bean = new MappedExtensionJoinedStrategyExtension();
		bean.setPreDeleteCalled(true);
		assertTrue(bean.isPreDeleteCalled());
	}

	@Test
	void setPostDeleteCalledToTrue() {
		MappedExtensionJoinedStrategyExtension bean = new MappedExtensionJoinedStrategyExtension();
		bean.setPostDeleteCalled(true);
		assertTrue(bean.isPostDeleteCalled());
	}
}
