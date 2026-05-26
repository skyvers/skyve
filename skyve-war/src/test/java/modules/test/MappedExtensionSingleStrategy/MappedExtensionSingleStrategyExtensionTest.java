package modules.test.MappedExtensionSingleStrategy;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
public class MappedExtensionSingleStrategyExtensionTest {

	@Test
	void allFlagsDefaultToFalse() {
		MappedExtensionSingleStrategyExtension bean = new MappedExtensionSingleStrategyExtension();
		assertFalse(bean.isPreSaveCalled());
		assertFalse(bean.isPostSaveCalled());
		assertFalse(bean.isPreDeleteCalled());
		assertFalse(bean.isPostDeleteCalled());
	}

	@Test
	void setPreSaveCalledToTrue() {
		MappedExtensionSingleStrategyExtension bean = new MappedExtensionSingleStrategyExtension();
		bean.setPreSaveCalled(true);
		assertTrue(bean.isPreSaveCalled());
	}

	@Test
	void setPostSaveCalledToTrue() {
		MappedExtensionSingleStrategyExtension bean = new MappedExtensionSingleStrategyExtension();
		bean.setPostSaveCalled(true);
		assertTrue(bean.isPostSaveCalled());
	}

	@Test
	void setPreDeleteCalledToTrue() {
		MappedExtensionSingleStrategyExtension bean = new MappedExtensionSingleStrategyExtension();
		bean.setPreDeleteCalled(true);
		assertTrue(bean.isPreDeleteCalled());
	}

	@Test
	void setPostDeleteCalledToTrue() {
		MappedExtensionSingleStrategyExtension bean = new MappedExtensionSingleStrategyExtension();
		bean.setPostDeleteCalled(true);
		assertTrue(bean.isPostDeleteCalled());
	}
}
