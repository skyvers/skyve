package modules.test.MappedExtensionJoinedStrategy;

import org.skyve.metadata.model.document.Bizlet;

public class MappedExtensionJoinedStrategyBizlet extends Bizlet<MappedExtensionJoinedStrategyExtension> {
	private static final long serialVersionUID = 1326428889776642631L;

	@Override
	public void preSave(MappedExtensionJoinedStrategyExtension bean) throws Exception {
		bean.setPreSaveCalled(true);
	}

	@Override
	public void postSave(MappedExtensionJoinedStrategyExtension bean) throws Exception {
		bean.setPostSaveCalled(true);
	}
}
