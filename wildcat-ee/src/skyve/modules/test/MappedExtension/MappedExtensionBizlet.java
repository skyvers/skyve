package modules.test.MappedExtension;

import org.skyve.metadata.model.document.Bizlet;

public class MappedExtensionBizlet extends Bizlet<MappedExtensionExtension> {
	private static final long serialVersionUID = 1326428889776642631L;

	@Override
	public void preSave(MappedExtensionExtension bean) throws Exception {
		bean.setPreSaveCalled(true);
	}

	@Override
	public void postSave(MappedExtensionExtension bean) throws Exception {
		bean.setPostSaveCalled(true);
	}
}
