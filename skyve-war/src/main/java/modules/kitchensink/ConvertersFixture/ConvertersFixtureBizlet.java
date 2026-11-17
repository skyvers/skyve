package modules.kitchensink.ConvertersFixture;

import org.skyve.domain.types.Decimal10;
import org.skyve.domain.types.Decimal2;
import org.skyve.domain.types.Decimal5;
import org.skyve.metadata.model.document.Bizlet;

import modules.kitchensink.domain.ConvertersFixture;

import jakarta.annotation.Nonnull;

/**
 * Stores values a user cannot type, so that the converters scripts can check how they are shown: a negative for the
 * absolute converter (Decimal2DollarsAndCentsAbsolute) of a fixture whose name ends with {@value #NEGATIVE_SUFFIX} (the
 * converter is for entering positive dollars, or for showing a magnitude whose sign the label gives); and, for a fixture
 * whose name ends with {@value #HALF_WAY_SUFFIX}, values half way between two shown values (1.005, -2.345, 0.12345 as a
 * percentage) and a negative that rounds to zero (-0.001), which a field does not keep when they are typed: it reads back
 * what it shows when the form is saved again, and a percentage is read to the places shown.
 */
public class ConvertersFixtureBizlet extends Bizlet<ConvertersFixture> {
	/** The end of the name of a fixture whose absolute converter's value is stored negative. */
	public static final String NEGATIVE_SUFFIX = " negative";

	/** The end of the name of a fixture whose values are stored half way between two shown values. */
	public static final String HALF_WAY_SUFFIX = " half way";

	/**
	 * Stores the absolute converter's value as a negative (its magnitude, as typed, negated) when the fixture's name
	 * ends with {@value #NEGATIVE_SUFFIX} (a negative is kept, so saving again does not change it), and the half way
	 * values when it ends with {@value #HALF_WAY_SUFFIX}: decimal5TwoDecimalPlaces 1.005 (shown 1.01),
	 * decimal10TwoDecimalPlaces -2.345 (-2.35), decimal5DollarsAndCents -0.001 (0.00) and
	 * decimal5TwoDecimalPlacesPercentage 0.12345 (12.35%).
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
		if ((name != null) && name.endsWith(HALF_WAY_SUFFIX)) {
			bean.setDecimal5TwoDecimalPlaces(new Decimal5("1.005"));
			bean.setDecimal10TwoDecimalPlaces(new Decimal10("-2.345"));
			bean.setDecimal5DollarsAndCents(new Decimal5("-0.001"));
			bean.setDecimal5TwoDecimalPlacesPercentage(new Decimal5("0.12345"));
		}
	}
}
