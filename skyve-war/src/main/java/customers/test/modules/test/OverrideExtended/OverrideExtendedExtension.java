package customers.test.modules.test.OverrideExtended;

import customers.test.modules.test.domain.OverrideExtendedOverride;

/**
 * The test customer's own extension of OverrideExtended, which Skyve and the test customer's mapping
 * use for its beans. It extends the generated <code>OverrideExtendedOverride</code>, which extends the
 * vanilla <code>modules.test.OverrideExtended.OverrideExtendedExtension</code>.
 */
public class OverrideExtendedExtension extends OverrideExtendedOverride {
	private static final long serialVersionUID = -2614863705219473385L;

	/**
	 * Describes this bean from its text, as the vanilla extension does, and its customer text.
	 *
	 * @return	the vanilla description followed by " for " and the customer text.
	 */
	@Override
	public String describe() {
		return super.describe() + " for " + getCustomerText();
	}
}
