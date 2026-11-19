package modules.test.MappedExtensionSingleStrategy;

import org.skyve.metadata.model.document.Bizlet;

import jakarta.annotation.Nonnull;

/**
 * Records lifecycle callback execution for single-strategy mapped extension tests.
 */
public class MappedExtensionSingleStrategyBizlet extends Bizlet<MappedExtensionSingleStrategyExtension> {
	/**
	 * Marks that pre-save was invoked.
	 *
	 * @param bean the extension bean under save
	 * @throws Exception if callback handling fails
	 */
	@Override
	public void preSave(@Nonnull MappedExtensionSingleStrategyExtension bean) throws Exception {
		bean.setPreSaveCalled(true);
	}

	/**
	 * Marks that post-save was invoked.
	 *
	 * @param bean the extension bean that was saved
	 * @throws Exception if callback handling fails
	 */
	@Override
	public void postSave(@Nonnull MappedExtensionSingleStrategyExtension bean) throws Exception {
		bean.setPostSaveCalled(true);
	}

	/**
	 * Marks that pre-delete was invoked.
	 *
	 * @param bean the extension bean under deletion
	 * @throws Exception if callback handling fails
	 */
	@Override
	public void preDelete(@Nonnull MappedExtensionSingleStrategyExtension bean) throws Exception {
		bean.setPreDeleteCalled(true);
	}

	/**
	 * Marks that post-delete was invoked.
	 *
	 * @param bean the extension bean that was deleted
	 * @throws Exception if callback handling fails
	 */
	@Override
	public void postDelete(@Nonnull MappedExtensionSingleStrategyExtension bean) throws Exception {
		bean.setPostDeleteCalled(true);
	}
}
