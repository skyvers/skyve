package util.sail.browser;

import org.opentest4j.TestAbortedException;

import jakarta.annotation.Nonnull;

/**
 * Says that a step needs a capability the current browser doesn't have, so the test is skipped, not failed: the test
 * can't show anything about the application in that browser. JUnit reports a {@link TestAbortedException} as skipped,
 * and the SAIL harness lets it through as one: the step after it doesn't run, and a suite counts the member as skipped.
 */
public class SailCapabilityUnavailableException extends TestAbortedException {
	private static final long serialVersionUID = 1L;

	/**
	 * Says which step needs which capability, and which browser lacks it.
	 *
	 * @param call	the step that needs it, such as the call's simple class name {@code Offline}
	 * @param capability	what it needs
	 * @param browser	the browser's name, such as {@code firefox}
	 */
	public SailCapabilityUnavailableException(@Nonnull String call, @Nonnull Capability capability, @Nonnull String browser) {
		super(call + " needs " + capability + ", which " + browser + " doesn't support");
	}
}
