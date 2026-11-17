package modules.test.OverrideNarrowedExtended;

import modules.test.domain.OverrideNarrowedExtendedVanilla;

/**
 * Hand-written extension of {@link modules.test.domain.OverrideNarrowedExtended}. The test customer's
 * override leaves out <code>dropped</code>, so this extends the generated
 * {@link OverrideNarrowedExtendedVanilla}, which declares it.
 */
public class OverrideNarrowedExtendedExtension extends OverrideNarrowedExtendedVanilla {
	private static final long serialVersionUID = -2384729384756129384L;

	/**
	 * Describes this bean from its text.
	 *
	 * @return	"Narrowed extended " followed by the text.
	 */
	public String describe() {
		return "Narrowed extended " + getText();
	}
}
