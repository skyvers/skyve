package modules.test.OverrideProxied;

import modules.test.domain.OverrideProxied;

/**
 * Hand-written extension of {@link OverrideProxied}, as admin.User has UserExtension.
 */
public class OverrideProxiedExtension extends OverrideProxied {
	private static final long serialVersionUID = -7023489123476012384L;

	/**
	 * Describes this bean from its text.
	 *
	 * @return	"Proxied " followed by the text.
	 */
	public String describe() {
		return "Proxied " + getText();
	}
}
