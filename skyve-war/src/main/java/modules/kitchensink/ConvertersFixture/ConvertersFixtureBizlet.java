package modules.kitchensink.ConvertersFixture;

import org.skyve.domain.types.Decimal2;
import org.skyve.metadata.model.document.Bizlet;

import modules.kitchensink.domain.ConvertersFixture;

import jakarta.annotation.Nonnull;

/**
 * Stores a negative for the absolute converter (Decimal2DollarsAndCentsAbsolute) of a fixture whose name ends with
 * {@value #NEGATIVE_SUFFIX}, so that its fields, cells and filters can be checked with a stored negative: the converter
 * is for entering positive dollars, or for showing a magnitude whose sign the label gives, so a user does not type one.
 */
public class ConvertersFixtureBizlet extends Bizlet<ConvertersFixture> {
	/** The end of the name of a fixture whose absolute converter's value is stored negative. */
	public static final String NEGATIVE_SUFFIX = " negative";

	/**
	 * Stores the absolute converter's value as a negative (its magnitude, as typed, negated) when the fixture's name
	 * ends with {@value #NEGATIVE_SUFFIX}. A negative is kept, so saving again does not change it.
	 *
	 * @param bean the fixture being saved
	 * @throws Exception if the bean cannot be updated
	 */
	@Override
	public void preSave(@Nonnull ConvertersFixture bean) throws Exception {
		String name = bean.getName();
		Decimal2 value = bean.getDecimal2DollarsAndCentsAbsolute();
		if ((name != null) && name.endsWith(NEGATIVE_SUFFIX) && (value != null) && (value.compareTo(Decimal2.ZERO) > 0)) {
			bean.setDecimal2DollarsAndCentsAbsolute(value.negate());
		}
	}
}
