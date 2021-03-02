package modules.test.MappedExtensionSingleStrategy;

import modules.test.domain.MappedExtensionSingleStrategy;

public class MappedExtensionSingleStrategyExtension extends MappedExtensionSingleStrategy {
	private static final long serialVersionUID = 627637583737021757L;

	private boolean preSaveCalled = false;
	private boolean postSaveCalled = false;

	public boolean isPreSaveCalled() {
		return preSaveCalled;
	}
	public void setPreSaveCalled(boolean preSaveCalled) {
		this.preSaveCalled = preSaveCalled;
	}
	
	public boolean isPostSaveCalled() {
		return postSaveCalled;
	}
	public void setPostSaveCalled(boolean postSaveCalled) {
		this.postSaveCalled = postSaveCalled;
	}
}
