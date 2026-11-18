package modules.test.OverrideEnumExtended;

import modules.test.domain.OverrideEnumExtendedVanilla;

/**
 * Hand-written extension of {@link modules.test.domain.OverrideEnumExtended}. The test customer's
 * override leaves out <code>status</code>, so this extends the generated
 * {@link OverrideEnumExtendedVanilla}, which declares it.
 */
public class OverrideEnumExtendedExtension extends OverrideEnumExtendedVanilla {
	private static final long serialVersionUID = 4410982734650192837L;

	/**
	 * Describes this bean from its text.
	 *
	 * @return	"Enum extended " followed by the text.
	 */
	public String describe() {
		return "Enum extended " + getText();
	}
}
