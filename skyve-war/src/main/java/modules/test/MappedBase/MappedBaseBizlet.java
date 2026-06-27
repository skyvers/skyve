package modules.test.MappedBase;

import org.skyve.metadata.model.document.Bizlet;

import modules.test.domain.MappedBase;

/**
 * Guards mapped-base lifecycle callbacks that should be overridden by extensions.
 */
public class MappedBaseBizlet extends Bizlet<MappedBase> {
	/**
	 * Fails fast when the mapped-base pre-save callback is invoked unexpectedly.
	 *
	 * @param bean the mapped-base bean being saved
	 * @throws Exception always, because extension Bizlets should override this callback
	 */
	@Override
	public void preSave(MappedBase bean) throws Exception {
		throw new IllegalStateException("preSave() should never be called as it is overridden and NOT called in MappedExtension");
	}

	/**
	 * Fails fast when the mapped-base post-save callback is invoked unexpectedly.
	 *
	 * @param bean the mapped-base bean that was saved
	 * @throws Exception always, because extension Bizlets should override this callback
	 */
	@Override
	public void postSave(MappedBase bean) throws Exception {
		throw new IllegalStateException("postSave() should never be called as it is overridden and NOT called in MappedExtension");
	}
}
