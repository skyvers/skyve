package modules.test.MappedBase;

import org.skyve.metadata.model.document.Bizlet;

import modules.test.domain.MappedBase;

public class MappedBaseBizlet extends Bizlet<MappedBase> {
	private static final long serialVersionUID = 6596395836902350544L;

	@Override
	public void preSave(MappedBase bean) throws Exception {
		throw new IllegalStateException("preSave() should never be called as it is overridden and NOT called in MappedExtension");
	}

	@Override
	public void postSave(MappedBase bean) throws Exception {
		throw new IllegalStateException("postSave() should never be called as it is overridden and NOT called in MappedExtension");
	}
}
