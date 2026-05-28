package modules.test.MappedExtensionJoinedStrategy;

import modules.test.domain.MappedExtensionJoinedStrategy;

/**
 * Tracks lifecycle-callback flags for joined-strategy mapped extension tests.
 */
public class MappedExtensionJoinedStrategyExtension extends MappedExtensionJoinedStrategy {
	private static final long serialVersionUID = 627637583737021757L;

	private boolean preSaveCalled = false;
	private boolean postSaveCalled = false;
	private boolean preDeleteCalled = false;
	private boolean postDeleteCalled = false;

	/**
	 * Returns whether pre-save has been invoked.
	 *
	 * @return {@code true} when pre-save was called
	 */
	public boolean isPreSaveCalled() {
		return preSaveCalled;
	}

	/**
	 * Records whether pre-save has been invoked.
	 *
	 * @param preSaveCalled {@code true} if pre-save executed
	 */
	public void setPreSaveCalled(boolean preSaveCalled) {
		this.preSaveCalled = preSaveCalled;
	}
	
	/**
	 * Returns whether post-save has been invoked.
	 *
	 * @return {@code true} when post-save was called
	 */
	public boolean isPostSaveCalled() {
		return postSaveCalled;
	}

	/**
	 * Records whether post-save has been invoked.
	 *
	 * @param postSaveCalled {@code true} if post-save executed
	 */
	public void setPostSaveCalled(boolean postSaveCalled) {
		this.postSaveCalled = postSaveCalled;
	}

	/**
	 * Returns whether pre-delete has been invoked.
	 *
	 * @return {@code true} when pre-delete was called
	 */
	public boolean isPreDeleteCalled() {
		return preDeleteCalled;
	}

	/**
	 * Records whether pre-delete has been invoked.
	 *
	 * @param preDeleteCalled {@code true} if pre-delete executed
	 */
	public void setPreDeleteCalled(boolean preDeleteCalled) {
		this.preDeleteCalled = preDeleteCalled;
	}
	
	/**
	 * Returns whether post-delete has been invoked.
	 *
	 * @return {@code true} when post-delete was called
	 */
	public boolean isPostDeleteCalled() {
		return postDeleteCalled;
	}

	/**
	 * Records whether post-delete has been invoked.
	 *
	 * @param postDeleteCalled {@code true} if post-delete executed
	 */
	public void setPostDeleteCalled(boolean postDeleteCalled) {
		this.postDeleteCalled = postDeleteCalled;
	}
}
