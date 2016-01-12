package modules.test.MappedExtensionSingleStrategy;

import org.skyve.metadata.model.document.Bizlet;

public class MappedExtensionSingleStrategyBizlet extends Bizlet<MappedExtensionSingleStrategyExtension> {
	private static final long serialVersionUID = 1326428889776642631L;

	@Override
	public void preSave(MappedExtensionSingleStrategyExtension bean) throws Exception {
		bean.setPreSaveCalled(true);
	}

	@Override
	public void postSave(MappedExtensionSingleStrategyExtension bean) throws Exception {
		bean.setPostSaveCalled(true);
	}
}
