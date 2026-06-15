package modules.test.MappedExtensionJoinedStrategy;

import org.skyve.metadata.model.document.Bizlet;

/**
 * Records lifecycle callback execution for joined-strategy mapped extension tests.
 */
public class MappedExtensionJoinedStrategyBizlet extends Bizlet<MappedExtensionJoinedStrategyExtension> {
	/**
	 * Marks that pre-save was invoked.
	 *
	 * @param bean the extension bean under save
	 * @throws Exception if callback handling fails
	 */
	@Override
	public void preSave(MappedExtensionJoinedStrategyExtension bean) throws Exception {
		bean.setPreSaveCalled(true);
	}

	/**
	 * Marks that post-save was invoked.
	 *
	 * @param bean the extension bean that was saved
	 * @throws Exception if callback handling fails
	 */
	@Override
	public void postSave(MappedExtensionJoinedStrategyExtension bean) throws Exception {
		bean.setPostSaveCalled(true);
	}

	/**
	 * Marks that pre-delete was invoked.
	 *
	 * @param bean the extension bean under deletion
	 * @throws Exception if callback handling fails
	 */
	@Override
	public void preDelete(MappedExtensionJoinedStrategyExtension bean) throws Exception {
		bean.setPreDeleteCalled(true);
	}

	/**
	 * Marks that post-delete was invoked.
	 *
	 * @param bean the extension bean that was deleted
	 * @throws Exception if callback handling fails
	 */
	@Override
	public void postDelete(MappedExtensionJoinedStrategyExtension bean) throws Exception {
		bean.setPostDeleteCalled(true);
	}
}
