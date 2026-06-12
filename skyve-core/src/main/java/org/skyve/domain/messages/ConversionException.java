package org.skyve.domain.messages;

import java.util.Collections;
import java.util.List;

import org.skyve.domain.types.converters.date.DD_MMM_YYYY;
import org.skyve.domain.types.converters.date.DD_MM_YYYY;
import org.skyve.domain.types.converters.date.MMM_DD_YYYY;
import org.skyve.domain.types.converters.date.MM_DD_YYYY;
import org.skyve.domain.types.converters.date.YYYY_MM_DD;
import org.skyve.domain.types.converters.datetime.DD_MMM_YYYY_HH24_MI;
import org.skyve.domain.types.converters.datetime.DD_MMM_YYYY_HH_MI;
import org.skyve.domain.types.converters.datetime.DD_MM_YYYY_HH24_MI;
import org.skyve.domain.types.converters.datetime.DD_MM_YYYY_HH_MI;
import org.skyve.domain.types.converters.datetime.MMM_DD_YYYY_HH24_MI;
import org.skyve.domain.types.converters.datetime.MMM_DD_YYYY_HH_MI;
import org.skyve.domain.types.converters.datetime.MM_DD_YYYY_HH24_MI;
import org.skyve.domain.types.converters.datetime.MM_DD_YYYY_HH_MI;
import org.skyve.domain.types.converters.datetime.YYYY_MM_DD_HH24_MI;
import org.skyve.domain.types.converters.datetime.YYYY_MM_DD_HH_MI;
import org.skyve.domain.types.converters.decimal.Decimal10Converter;
import org.skyve.domain.types.converters.decimal.Decimal10TwoDecimalPlaces;
import org.skyve.domain.types.converters.decimal.Decimal2Converter;
import org.skyve.domain.types.converters.decimal.Decimal2Integer;
import org.skyve.domain.types.converters.decimal.Decimal2IntegerPercentage;
import org.skyve.domain.types.converters.decimal.Decimal2OneDecimalPlace;
import org.skyve.domain.types.converters.decimal.Decimal5Converter;
import org.skyve.domain.types.converters.decimal.Decimal5Integer;
import org.skyve.domain.types.converters.decimal.Decimal5IntegerPercentage;
import org.skyve.domain.types.converters.decimal.Decimal5OneDecimalPlace;
import org.skyve.domain.types.converters.decimal.Decimal5TimeDuration;
import org.skyve.domain.types.converters.decimal.Decimal5TwoDecimalPlaces;
import org.skyve.domain.types.converters.decimal.Decimal5TwoDecimalPlacesPercentage;
import org.skyve.domain.types.converters.decimal.currency.Decimal10DollarsAndCents;
import org.skyve.domain.types.converters.decimal.currency.Decimal2DollarsAndCents;
import org.skyve.domain.types.converters.decimal.currency.Decimal2DollarsAndCentsAbsolute;
import org.skyve.domain.types.converters.decimal.currency.Decimal5DollarsAndCents;
import org.skyve.domain.types.converters.enumeration.DynamicEnumerationConverter;
import org.skyve.domain.types.converters.geometry.GeometryConverter;
import org.skyve.domain.types.converters.integer.IntegerConverter;
import org.skyve.domain.types.converters.integer.IntegerSeparator;
import org.skyve.domain.types.converters.integer.LongIntegerConverter;
import org.skyve.domain.types.converters.integer.LongIntegerSeparator;
import org.skyve.domain.types.converters.integer.SimplePercentage;
import org.skyve.domain.types.converters.time.HH24_MI;
import org.skyve.domain.types.converters.time.HH24_MI_SS;
import org.skyve.domain.types.converters.time.HH_MI;
import org.skyve.domain.types.converters.time.HH_MI_SS;
import org.skyve.domain.types.converters.timestamp.DD_MMM_YYYY_HH24_MI_SS;
import org.skyve.domain.types.converters.timestamp.DD_MMM_YYYY_HH_MI_SS;
import org.skyve.domain.types.converters.timestamp.DD_MM_YYYY_HH24_MI_SS;
import org.skyve.domain.types.converters.timestamp.DD_MM_YYYY_HH_MI_SS;
import org.skyve.domain.types.converters.timestamp.MMM_DD_YYYY_HH24_MI_SS;
import org.skyve.domain.types.converters.timestamp.MMM_DD_YYYY_HH_MI_SS;
import org.skyve.domain.types.converters.timestamp.MM_DD_YYYY_HH24_MI_SS;
import org.skyve.domain.types.converters.timestamp.MM_DD_YYYY_HH_MI_SS;
import org.skyve.domain.types.converters.timestamp.YYYY_MM_DD_HH24_MI_SS;
import org.skyve.domain.types.converters.timestamp.YYYY_MM_DD_HH_MI_SS;
import org.skyve.util.Util;

import jakarta.annotation.Nonnull;

/**
 * Signals that a {@link org.skyve.domain.types.converters.Converter} could not parse a
 * display string into its target domain type.
 *
 * <p>Thrown by {@link org.skyve.domain.types.converters.Converter#fromDisplayValue} when
 * the input does not match the expected format. The exception wraps a single
 * {@link Message} whose text is the localised error string for the specific converter,
 * identified by one of the {@code *_KEY} constants on this class.
 *
 * <p>Each constant names the i18n resource bundle key used for a specific converter's
 * error message and is composed as {@code exception.conversion.<type>.<ConverterClassName>}.
 * Application code generally does not need to use these constants directly; they are
 * used by converter implementations and test assertions.
 *
 * @see org.skyve.domain.types.converters.Converter
 * @see MessageException
 */
@SuppressWarnings("java:S110") // This inheritance-depth warning is ridiculous for intentional framework hierarchies.
public class ConversionException extends DomainException implements MessageException {
	private static final long serialVersionUID = 6752293620540676434L;

	private static final String I18N_PREFIX = "exception.conversion.";
	
	private static final String I18N_DATE_PREFIX = I18N_PREFIX + "date.";
	/**
	 * Executes DD_MM_YYYY.class.getSimpleName.
	 * @return the result
	 */
	public static final String DD_MM_YYYY_KEY = I18N_DATE_PREFIX + DD_MM_YYYY.class.getSimpleName();
	/**
	 * Executes DD_MMM_YYYY.class.getSimpleName.
	 * @return the result
	 */
	public static final String DD_MMM_YYYY_KEY = I18N_DATE_PREFIX + DD_MMM_YYYY.class.getSimpleName();
	/**
	 * Executes MM_DD_YYYY.class.getSimpleName.
	 * @return the result
	 */
	public static final String MM_DD_YYYY_KEY = I18N_DATE_PREFIX + MM_DD_YYYY.class.getSimpleName();
	/**
	 * Executes MMM_DD_YYYY.class.getSimpleName.
	 * @return the result
	 */
	public static final String MMM_DD_YYYY_KEY = I18N_DATE_PREFIX + MMM_DD_YYYY.class.getSimpleName();
	/**
	 * Executes YYYY_MM_DD.class.getSimpleName.
	 * @return the result
	 */
	public static final String YYYY_MM_DD_KEY = I18N_DATE_PREFIX + YYYY_MM_DD.class.getSimpleName();
	
	private static final String I18N_DATETIME_PREFIX = I18N_PREFIX + "datetime.";
	private static final String I18N_DATETIME_SUFFIX = "_DateTime";
	/**
	 * Executes DD_MM_YYYY_HH_MI.class.getSimpleName.
	 * @return the result
	 */
	public static final String DD_MM_YYYY_HH_MI_KEY = I18N_DATETIME_PREFIX + DD_MM_YYYY_HH_MI.class.getSimpleName();
	/**
	 * Executes DD_MM_YYYY_HH24_MI.class.getSimpleName.
	 * @return the result
	 */
	public static final String DD_MM_YYYY_HH24_MI_KEY = I18N_DATETIME_PREFIX + DD_MM_YYYY_HH24_MI.class.getSimpleName();
	/**
	 * Executes DD_MM_YYYY.class.getSimpleName.
	 * @return the result
	 */
	public static final String DD_MM_YYYY_DATETIME_KEY = I18N_DATETIME_PREFIX + DD_MM_YYYY.class.getSimpleName() + I18N_DATETIME_SUFFIX;
	/**
	 * Executes DD_MMM_YYYY_HH_MI.class.getSimpleName.
	 * @return the result
	 */
	public static final String DD_MMM_YYYY_HH_MI_KEY = I18N_DATETIME_PREFIX + DD_MMM_YYYY_HH_MI.class.getSimpleName();
	/**
	 * Executes DD_MMM_YYYY_HH24_MI.class.getSimpleName.
	 * @return the result
	 */
	public static final String DD_MMM_YYYY_HH24_MI_KEY = I18N_DATETIME_PREFIX + DD_MMM_YYYY_HH24_MI.class.getSimpleName();
	/**
	 * Executes DD_MMM_YYYY.class.getSimpleName.
	 * @return the result
	 */
	public static final String DD_MMM_YYYY_DATETIME_KEY = I18N_DATETIME_PREFIX + DD_MMM_YYYY.class.getSimpleName() + I18N_DATETIME_SUFFIX;
	/**
	 * Executes MM_DD_YYYY_HH_MI.class.getSimpleName.
	 * @return the result
	 */
	public static final String MM_DD_YYYY_HH_MI_KEY = I18N_DATETIME_PREFIX + MM_DD_YYYY_HH_MI.class.getSimpleName();
	/**
	 * Executes MM_DD_YYYY_HH24_MI.class.getSimpleName.
	 * @return the result
	 */
	public static final String MM_DD_YYYY_HH24_MI_KEY = I18N_DATETIME_PREFIX + MM_DD_YYYY_HH24_MI.class.getSimpleName();
	/**
	 * Executes MM_DD_YYYY.class.getSimpleName.
	 * @return the result
	 */
	public static final String MM_DD_YYYY_DATETIME_KEY = I18N_DATETIME_PREFIX + MM_DD_YYYY.class.getSimpleName() + I18N_DATETIME_SUFFIX;
	/**
	 * Executes MMM_DD_YYYY_HH_MI.class.getSimpleName.
	 * @return the result
	 */
	public static final String MMM_DD_YYYY_HH_MI_KEY = I18N_DATETIME_PREFIX + MMM_DD_YYYY_HH_MI.class.getSimpleName();
	/**
	 * Executes MMM_DD_YYYY_HH24_MI.class.getSimpleName.
	 * @return the result
	 */
	public static final String MMM_DD_YYYY_HH24_MI_KEY = I18N_DATETIME_PREFIX + MMM_DD_YYYY_HH24_MI.class.getSimpleName();
	/**
	 * Executes MMM_DD_YYYY.class.getSimpleName.
	 * @return the result
	 */
	public static final String MMM_DD_YYYY_DATETIME_KEY = I18N_DATETIME_PREFIX + MMM_DD_YYYY.class.getSimpleName() + I18N_DATETIME_SUFFIX;
	/**
	 * Executes YYYY_MM_DD_HH_MI.class.getSimpleName.
	 * @return the result
	 */
	public static final String YYYY_MM_DD_HH_MI_KEY = I18N_DATETIME_PREFIX + YYYY_MM_DD_HH_MI.class.getSimpleName();
	/**
	 * Executes YYYY_MM_DD_HH24_MI.class.getSimpleName.
	 * @return the result
	 */
	public static final String YYYY_MM_DD_HH24_MI_KEY = I18N_DATETIME_PREFIX + YYYY_MM_DD_HH24_MI.class.getSimpleName();
	/**
	 * Executes YYYY_MM_DD.class.getSimpleName.
	 * @return the result
	 */
	public static final String YYYY_MM_DD_DATETIME_KEY = I18N_DATETIME_PREFIX + YYYY_MM_DD.class.getSimpleName() + I18N_DATETIME_SUFFIX;
	
	private static final String I18N_DECIMAL_PREFIX = I18N_PREFIX + "decimal.";
	/**
	 * Executes Decimal10Converter.class.getSimpleName.
	 * @return the result
	 */
	public static final String DECIMAL_10_CONVERTER_KEY = I18N_DECIMAL_PREFIX + Decimal10Converter.class.getSimpleName();
	/**
	 * Executes Decimal10TwoDecimalPlaces.class.getSimpleName.
	 * @return the result
	 */
	public static final String DECIMAL_10_TWO_DECIMAL_PLACES_KEY = I18N_DECIMAL_PREFIX + Decimal10TwoDecimalPlaces.class.getSimpleName();
	/**
	 * Executes Decimal2Converter.class.getSimpleName.
	 * @return the result
	 */
	public static final String DECIMAL_2_CONVERTER_KEY = I18N_DECIMAL_PREFIX + Decimal2Converter.class.getSimpleName();
	/**
	 * Executes Decimal2Integer.class.getSimpleName.
	 * @return the result
	 */
	public static final String DECIMAL_2_INTEGER_KEY = I18N_DECIMAL_PREFIX + Decimal2Integer.class.getSimpleName();
	/**
	 * Executes Decimal2IntegerPercentage.class.getSimpleName.
	 * @return the result
	 */
	public static final String DECIMAL_2_INTEGER_PERCENTAGE_KEY = I18N_DECIMAL_PREFIX + Decimal2IntegerPercentage.class.getSimpleName();
	/**
	 * Executes Decimal2OneDecimalPlace.class.getSimpleName.
	 * @return the result
	 */
	public static final String DECIMAL_2_ONE_DECIMAL_PLACE_KEY = I18N_DECIMAL_PREFIX + Decimal2OneDecimalPlace.class.getSimpleName();
	/**
	 * Executes Decimal5Converter.class.getSimpleName.
	 * @return the result
	 */
	public static final String DECIMAL_5_CONVERTER_KEY = I18N_DECIMAL_PREFIX + Decimal5Converter.class.getSimpleName();
	/**
	 * Executes Decimal5Integer.class.getSimpleName.
	 * @return the result
	 */
	public static final String DECIMAL_5_INTEGER_KEY = I18N_DECIMAL_PREFIX + Decimal5Integer.class.getSimpleName();
	/**
	 * Executes Decimal5IntegerPercentage.class.getSimpleName.
	 * @return the result
	 */
	public static final String DECIMAL_5_INTEGER_PERCENTAGE_KEY = I18N_DECIMAL_PREFIX + Decimal5IntegerPercentage.class.getSimpleName();
	/**
	 * Executes Decimal5OneDecimalPlace.class.getSimpleName.
	 * @return the result
	 */
	public static final String DECIMAL_5_ONE_DECIMAL_PLACE_KEY = I18N_DECIMAL_PREFIX + Decimal5OneDecimalPlace.class.getSimpleName();
	/**
	 * Executes Decimal5TimeDuration.class.getSimpleName.
	 * @return the result
	 */
	public static final String DECIMAL_5_TIME_DURATION_KEY = I18N_DECIMAL_PREFIX + Decimal5TimeDuration.class.getSimpleName();
	/**
	 * Executes Decimal5TwoDecimalPlaces.class.getSimpleName.
	 * @return the result
	 */
	public static final String DECIMAL_5_TWO_DECIMAL_PLACES_KEY = I18N_DECIMAL_PREFIX + Decimal5TwoDecimalPlaces.class.getSimpleName();
	/**
	 * Executes Decimal5TwoDecimalPlacesPercentage.class.getSimpleName.
	 * @return the result
	 */
	public static final String DECIMAL_5_TWO_DECIMAL_PLACES_PERCENTAGE_KEY = I18N_DECIMAL_PREFIX + Decimal5TwoDecimalPlacesPercentage.class.getSimpleName();
	private static final String I18N_CURRENCY_PREFIX = I18N_DECIMAL_PREFIX + "currency.";
	/**
	 * Executes Decimal10DollarsAndCents.class.getSimpleName.
	 * @return the result
	 */
	public static final String DECIMAL_10_DOLLARS_AND_CENTS_KEY = I18N_CURRENCY_PREFIX + Decimal10DollarsAndCents.class.getSimpleName();
	/**
	 * Executes Decimal2DollarsAndCents.class.getSimpleName.
	 * @return the result
	 */
	public static final String DECIMAL_2_DOLLARS_AND_CENTS_KEY = I18N_CURRENCY_PREFIX + Decimal2DollarsAndCents.class.getSimpleName();
	/**
	 * Executes Decimal2DollarsAndCentsAbsolute.class.getSimpleName.
	 * @return the result
	 */
	public static final String DECIMAL_2_DOLLARS_AND_CENTS_ABSOLUTE_KEY = I18N_CURRENCY_PREFIX + Decimal2DollarsAndCentsAbsolute.class.getSimpleName();
	/**
	 * Executes Decimal5DollarsAndCents.class.getSimpleName.
	 * @return the result
	 */
	public static final String DECIMAL_5_DOLLARS_AND_CENTS_KEY = I18N_CURRENCY_PREFIX + Decimal5DollarsAndCents.class.getSimpleName();
	
	/**
	 * Executes DynamicEnumerationConverter.class.getSimpleName.
	 * @return the result
	 */
	public static final String DYNAMIC_ENUMERATION_CONVERTER_KEY = I18N_PREFIX + "enumeration." + DynamicEnumerationConverter.class.getSimpleName();
	
	/**
	 * Executes GeometryConverter.class.getSimpleName.
	 * @return the result
	 */
	public static final String GEOMETRY_CONVERTER_KEY = I18N_PREFIX + "geometry." + GeometryConverter.class.getSimpleName();
	
	private static final String I18N_INTEGER_PREFIX = I18N_PREFIX + "integer.";
	/**
	 * Executes IntegerConverter.class.getSimpleName.
	 * @return the result
	 */
	public static final String INTEGER_CONVERTER_KEY = I18N_INTEGER_PREFIX + IntegerConverter.class.getSimpleName();
	/**
	 * Executes IntegerSeparator.class.getSimpleName.
	 * @return the result
	 */
	public static final String INTEGER_SEPARATOR_KEY = I18N_INTEGER_PREFIX + IntegerSeparator.class.getSimpleName();
	/**
	 * Executes LongIntegerConverter.class.getSimpleName.
	 * @return the result
	 */
	public static final String LONG_INTEGER_CONVERTER_KEY = I18N_INTEGER_PREFIX + LongIntegerConverter.class.getSimpleName();
	/**
	 * Executes LongIntegerSeparator.class.getSimpleName.
	 * @return the result
	 */
	public static final String LONG_INTEGER_SEPARATOR_KEY = I18N_INTEGER_PREFIX + LongIntegerSeparator.class.getSimpleName();
	/**
	 * Executes SimplePercentage.class.getSimpleName.
	 * @return the result
	 */
	public static final String SIMPLE_PERCENTAGE_KEY = I18N_INTEGER_PREFIX + SimplePercentage.class.getSimpleName();

	private static final String I18N_TIME_PREFIX = I18N_PREFIX + "time.";
	/**
	 * Executes HH_MI_SS.class.getSimpleName.
	 * @return the result
	 */
	public static final String HH_MI_SS_KEY = I18N_TIME_PREFIX + HH_MI_SS.class.getSimpleName();
	/**
	 * Executes HH_MI.class.getSimpleName.
	 * @return the result
	 */
	public static final String HH_MI_KEY = I18N_TIME_PREFIX + HH_MI.class.getSimpleName();
	/**
	 * Executes HH24_MI_SS.class.getSimpleName.
	 * @return the result
	 */
	public static final String HH24_MI_SS_KEY = I18N_TIME_PREFIX + HH24_MI_SS.class.getSimpleName();
	/**
	 * Executes HH24_MI.class.getSimpleName.
	 * @return the result
	 */
	public static final String HH24_MI_KEY = I18N_TIME_PREFIX + HH24_MI.class.getSimpleName();

	private static final String I18N_TIMESTAMP_PREFIX = I18N_PREFIX + "timestamp.";
	private static final String I18N_TIMESTAMP_SUFFIX = "_Timestamp";
	/**
	 * Executes DD_MM_YYYY_HH_MI_SS.class.getSimpleName.
	 * @return the result
	 */
	public static final String DD_MM_YYYY_HH_MI_SS_KEY = I18N_TIMESTAMP_PREFIX + DD_MM_YYYY_HH_MI_SS.class.getSimpleName();
	/**
	 * Executes DD_MM_YYYY_HH24_MI_SS.class.getSimpleName.
	 * @return the result
	 */
	public static final String DD_MM_YYYY_HH24_MI_SS_KEY = I18N_TIMESTAMP_PREFIX + DD_MM_YYYY_HH24_MI_SS.class.getSimpleName();
	/**
	 * Executes DD_MM_YYYY.class.getSimpleName.
	 * @return the result
	 */
	public static final String DD_MM_YYYY_TIMESTAMP_KEY = I18N_TIMESTAMP_PREFIX + DD_MM_YYYY.class.getSimpleName() + I18N_TIMESTAMP_SUFFIX;
	/**
	 * Executes DD_MMM_YYYY_HH_MI_SS.class.getSimpleName.
	 * @return the result
	 */
	public static final String DD_MMM_YYYY_HH_MI_SS_KEY = I18N_TIMESTAMP_PREFIX + DD_MMM_YYYY_HH_MI_SS.class.getSimpleName();
	/**
	 * Executes DD_MMM_YYYY_HH24_MI_SS.class.getSimpleName.
	 * @return the result
	 */
	public static final String DD_MMM_YYYY_HH24_MI_SS_KEY = I18N_TIMESTAMP_PREFIX + DD_MMM_YYYY_HH24_MI_SS.class.getSimpleName();
	/**
	 * Executes DD_MMM_YYYY.class.getSimpleName.
	 * @return the result
	 */
	public static final String DD_MMM_YYYY_TIMESTAMP_KEY = I18N_TIMESTAMP_PREFIX + DD_MMM_YYYY.class.getSimpleName() + I18N_TIMESTAMP_SUFFIX;
	/**
	 * Executes MM_DD_YYYY_HH_MI_SS.class.getSimpleName.
	 * @return the result
	 */
	public static final String MM_DD_YYYY_HH_MI_SS_KEY = I18N_TIMESTAMP_PREFIX + MM_DD_YYYY_HH_MI_SS.class.getSimpleName();
	/**
	 * Executes MM_DD_YYYY_HH24_MI_SS.class.getSimpleName.
	 * @return the result
	 */
	public static final String MM_DD_YYYY_HH24_MI_SS_KEY = I18N_TIMESTAMP_PREFIX + MM_DD_YYYY_HH24_MI_SS.class.getSimpleName();
	/**
	 * Executes MM_DD_YYYY.class.getSimpleName.
	 * @return the result
	 */
	public static final String MM_DD_YYYY_TIMESTAMP_KEY = I18N_TIMESTAMP_PREFIX + MM_DD_YYYY.class.getSimpleName() + I18N_TIMESTAMP_SUFFIX;
	/**
	 * Executes MMM_DD_YYYY_HH_MI_SS.class.getSimpleName.
	 * @return the result
	 */
	public static final String MMM_DD_YYYY_HH_MI_SS_KEY = I18N_TIMESTAMP_PREFIX + MMM_DD_YYYY_HH_MI_SS.class.getSimpleName();
	/**
	 * Executes MMM_DD_YYYY_HH24_MI_SS.class.getSimpleName.
	 * @return the result
	 */
	public static final String MMM_DD_YYYY_HH24_MI_SS_KEY = I18N_TIMESTAMP_PREFIX + MMM_DD_YYYY_HH24_MI_SS.class.getSimpleName();
	/**
	 * Executes MMM_DD_YYYY.class.getSimpleName.
	 * @return the result
	 */
	public static final String MMM_DD_YYYY_TIMESTAMP_KEY = I18N_TIMESTAMP_PREFIX + MMM_DD_YYYY.class.getSimpleName() + I18N_TIMESTAMP_SUFFIX;
	/**
	 * Executes YYYY_MM_DD_HH_MI_SS.class.getSimpleName.
	 * @return the result
	 */
	public static final String YYYY_MM_DD_HH_MI_SS_KEY = I18N_TIMESTAMP_PREFIX + YYYY_MM_DD_HH_MI_SS.class.getSimpleName();
	/**
	 * Executes YYYY_MM_DD_HH24_MI_SS.class.getSimpleName.
	 * @return the result
	 */
	public static final String YYYY_MM_DD_HH24_MI_SS_KEY = I18N_TIMESTAMP_PREFIX + YYYY_MM_DD_HH24_MI_SS.class.getSimpleName();
	/**
	 * Executes YYYY_MM_DD.class.getSimpleName.
	 * @return the result
	 */
	public static final String YYYY_MM_DD_TIMESTAMP_KEY = I18N_TIMESTAMP_PREFIX + YYYY_MM_DD.class.getSimpleName() + I18N_TIMESTAMP_SUFFIX;
	
	private List<Message> messages = null;
	
	/**
	 * Creates a new ConversionException instance.
	 * @param i18nKey the i18nKey
	 * @param i18nValues the i18nValues
	 */
	public ConversionException(@Nonnull String i18nKey, String... i18nValues) {
		super("Conversion failed for converter :- " + i18nKey, false);
		messages = Collections.singletonList(new Message(Util.nullSafeI18n(i18nKey, i18nValues)));
	}

	/**
	 * Creates a new ConversionException instance.
	 * @param i18nKey the i18nKey
	 * @param t the t
	 */
	public ConversionException(@Nonnull String i18nKey, @Nonnull Throwable t) {
		super("Conversion failed for converter :- " + i18nKey, t, false);
		messages = Collections.singletonList(new Message(Util.nullSafeI18n(i18nKey)));
	}

	/**
	 * Returns the messages.
	 * @return the result
	 */
	@Override
	public List<Message> getMessages() {
		return messages;
	}
}
