package modules.test.MappedExtensionJoinedStrategy;

import org.skyve.metadata.model.document.Bizlet;

public class MappedExtensionJoinedStrategyBizlet extends Bizlet<MappedExtensionJoinedStrategyExtension> {
	@Override
	public void preSave(MappedExtensionJoinedStrategyExtension bean) throws Exception {
		bean.setPreSaveCalled(true);
	}

	@Override
	public void postSave(MappedExtensionJoinedStrategyExtension bean) throws Exception {
		bean.setPostSaveCalled(true);
	}

	@Override
	public void preDelete(MappedExtensionJoinedStrategyExtension bean) throws Exception {
		bean.setPreDeleteCalled(true);
	}

	@Override
	public void postDelete(MappedExtensionJoinedStrategyExtension bean) throws Exception {
		bean.setPostDeleteCalled(true);
	}
}
