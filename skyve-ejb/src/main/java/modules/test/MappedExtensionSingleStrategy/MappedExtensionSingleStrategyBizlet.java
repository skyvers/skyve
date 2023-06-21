package modules.test.MappedExtensionSingleStrategy;

import org.skyve.metadata.model.document.Bizlet;

public class MappedExtensionSingleStrategyBizlet extends Bizlet<MappedExtensionSingleStrategyExtension> {
	@Override
	public void preSave(MappedExtensionSingleStrategyExtension bean) throws Exception {
		bean.setPreSaveCalled(true);
	}

	@Override
	public void postSave(MappedExtensionSingleStrategyExtension bean) throws Exception {
		bean.setPostSaveCalled(true);
	}

	@Override
	public void preDelete(MappedExtensionSingleStrategyExtension bean) throws Exception {
		bean.setPreDeleteCalled(true);
	}

	@Override
	public void postDelete(MappedExtensionSingleStrategyExtension bean) throws Exception {
		bean.setPostDeleteCalled(true);
	}
}
