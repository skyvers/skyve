package modules.test.OverrideExtended;

import modules.test.domain.OverrideExtended;

/**
 * Hand-written extension of {@link OverrideExtended}. The test customer's override adds a property, so
 * its generated <code>OverrideExtendedOverride</code> class extends this one, and the test customer's
 * own extension extends that. Every customer gets {@link #describe()}.
 */
public class OverrideExtendedExtension extends OverrideExtended {
	private static final long serialVersionUID = 5348236547651837652L;

	/**
	 * Describes this bean from its text.
	 *
	 * @return	"Extended " followed by the text.
	 */
	public String describe() {
		return "Extended " + getText();
	}
}
