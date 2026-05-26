package modules.test.MappedExtensionSingleStrategy;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

import modules.test.MappedBase.MappedBaseBizlet;
import modules.test.domain.MappedBase;

@SuppressWarnings("static-method")
public class MappedExtensionSingleStrategyBizletTest {

	private static final MappedExtensionSingleStrategyBizlet bizlet = new MappedExtensionSingleStrategyBizlet();
	private static final MappedBaseBizlet baseBizlet = new MappedBaseBizlet();

	@Test
	void preSaveSetsFlag() throws Exception {
		MappedExtensionSingleStrategyExtension bean = new MappedExtensionSingleStrategyExtension();
		bizlet.preSave(bean);
		assertTrue(bean.isPreSaveCalled());
	}

	@Test
	void postSaveSetsFlag() throws Exception {
		MappedExtensionSingleStrategyExtension bean = new MappedExtensionSingleStrategyExtension();
		bizlet.postSave(bean);
		assertTrue(bean.isPostSaveCalled());
	}

	@Test
	void preDeleteSetsFlag() throws Exception {
		MappedExtensionSingleStrategyExtension bean = new MappedExtensionSingleStrategyExtension();
		bizlet.preDelete(bean);
		assertTrue(bean.isPreDeleteCalled());
	}

	@Test
	void postDeleteSetsFlag() throws Exception {
		MappedExtensionSingleStrategyExtension bean = new MappedExtensionSingleStrategyExtension();
		bizlet.postDelete(bean);
		assertTrue(bean.isPostDeleteCalled());
	}

	@Test
	void mappedBaseBizletPreSaveThrowsIllegalStateException() {
		MappedBase base = new MappedBase();
		assertThrows(IllegalStateException.class, () -> baseBizlet.preSave(base));
	}

	@Test
	void mappedBaseBizletPostSaveThrowsIllegalStateException() {
		MappedBase base = new MappedBase();
		assertThrows(IllegalStateException.class, () -> baseBizlet.postSave(base));
	}
}
