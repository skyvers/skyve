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
import org.skyve.domain.types.converters.decimal.Decimal2TwoDecimalPlacesPercentage;
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

public class ConversionException extends DomainException implements MessageException {
	private static final long serialVersionUID = 6752293620540676434L;

	private static final String I18N_PREFIX = "exception.conversion.";
	
	private static final String I18N_DATE_PREFIX = I18N_PREFIX + "date.";
	public static final String DD_MM_YYYY_KEY = I18N_DATE_PREFIX + DD_MM_YYYY.class.getSimpleName();
	public static final String DD_MMM_YYYY_KEY = I18N_DATE_PREFIX + DD_MMM_YYYY.class.getSimpleName();
	public static final String MM_DD_YYYY_KEY = I18N_DATE_PREFIX + MM_DD_YYYY.class.getSimpleName();
	public static final String MMM_DD_YYYY_KEY = I18N_DATE_PREFIX + MMM_DD_YYYY.class.getSimpleName();
	public static final String YYYY_MM_DD_KEY = I18N_DATE_PREFIX + YYYY_MM_DD.class.getSimpleName();
	
	private static final String I18N_DATETIME_PREFIX = I18N_PREFIX + "datetime.";
	private static final String I18N_DATETIME_SUFFIX = "_DateTime";
	public static final String DD_MM_YYYY_HH_MI_KEY = I18N_DATETIME_PREFIX + DD_MM_YYYY_HH_MI.class.getSimpleName();
	public static final String DD_MM_YYYY_HH24_MI_KEY = I18N_DATETIME_PREFIX + DD_MM_YYYY_HH24_MI.class.getSimpleName();
	public static final String DD_MM_YYYY_DATETIME_KEY = I18N_DATETIME_PREFIX + DD_MM_YYYY.class.getSimpleName() + I18N_DATETIME_SUFFIX;
	public static final String DD_MMM_YYYY_HH_MI_KEY = I18N_DATETIME_PREFIX + DD_MMM_YYYY_HH_MI.class.getSimpleName();
	public static final String DD_MMM_YYYY_HH24_MI_KEY = I18N_DATETIME_PREFIX + DD_MMM_YYYY_HH24_MI.class.getSimpleName();
	public static final String DD_MMM_YYYY_DATETIME_KEY = I18N_DATETIME_PREFIX + DD_MMM_YYYY.class.getSimpleName() + I18N_DATETIME_SUFFIX;
	public static final String MM_DD_YYYY_HH_MI_KEY = I18N_DATETIME_PREFIX + MM_DD_YYYY_HH_MI.class.getSimpleName();
	public static final String MM_DD_YYYY_HH24_MI_KEY = I18N_DATETIME_PREFIX + MM_DD_YYYY_HH24_MI.class.getSimpleName();
	public static final String MM_DD_YYYY_DATETIME_KEY = I18N_DATETIME_PREFIX + MM_DD_YYYY.class.getSimpleName() + I18N_DATETIME_SUFFIX;
	public static final String MMM_DD_YYYY_HH_MI_KEY = I18N_DATETIME_PREFIX + MMM_DD_YYYY_HH_MI.class.getSimpleName();
	public static final String MMM_DD_YYYY_HH24_MI_KEY = I18N_DATETIME_PREFIX + MMM_DD_YYYY_HH24_MI.class.getSimpleName();
	public static final String MMM_DD_YYYY_DATETIME_KEY = I18N_DATETIME_PREFIX + MMM_DD_YYYY.class.getSimpleName() + I18N_DATETIME_SUFFIX;
	public static final String YYYY_MM_DD_HH_MI_KEY = I18N_DATETIME_PREFIX + YYYY_MM_DD_HH_MI.class.getSimpleName();
	public static final String YYYY_MM_DD_HH24_MI_KEY = I18N_DATETIME_PREFIX + YYYY_MM_DD_HH24_MI.class.getSimpleName();
	public static final String YYYY_MM_DD_DATETIME_KEY = I18N_DATETIME_PREFIX + YYYY_MM_DD.class.getSimpleName() + I18N_DATETIME_SUFFIX;
	
	private static final String I18N_DECIMAL_PREFIX = I18N_PREFIX + "decimal.";
	public static final String DECIMAL_10_CONVERTER_KEY = I18N_DECIMAL_PREFIX + Decimal10Converter.class.getSimpleName();
	public static final String DECIMAL_10_TWO_DECIMAL_PLACES_KEY = I18N_DECIMAL_PREFIX + Decimal10TwoDecimalPlaces.class.getSimpleName();
	public static final String DECIMAL_2_CONVERTER_KEY = I18N_DECIMAL_PREFIX + Decimal2Converter.class.getSimpleName();
	public static final String DECIMAL_2_INTEGER_KEY = I18N_DECIMAL_PREFIX + Decimal2Integer.class.getSimpleName();
	public static final String DECIMAL_2_INTEGER_PERCENTAGE_KEY = I18N_DECIMAL_PREFIX + Decimal2IntegerPercentage.class.getSimpleName();
	public static final String DECIMAL_2_ONE_DECIMAL_PLACE_KEY = I18N_DECIMAL_PREFIX + Decimal2OneDecimalPlace.class.getSimpleName();
	public static final String DECIMAL_2_TWO_DECIMAL_PLACES_PERCENTAGE_KEY = I18N_DECIMAL_PREFIX + Decimal2TwoDecimalPlacesPercentage.class.getSimpleName();
	public static final String DECIMAL_5_CONVERTER_KEY = I18N_DECIMAL_PREFIX + Decimal5Converter.class.getSimpleName();
	public static final String DECIMAL_5_INTEGER_KEY = I18N_DECIMAL_PREFIX + Decimal5Integer.class.getSimpleName();
	public static final String DECIMAL_5_INTEGER_PERCENTAGE_KEY = I18N_DECIMAL_PREFIX + Decimal5IntegerPercentage.class.getSimpleName();
	public static final String DECIMAL_5_ONE_DECIMAL_PLACE_KEY = I18N_DECIMAL_PREFIX + Decimal5OneDecimalPlace.class.getSimpleName();
	public static final String DECIMAL_5_TIME_DURATION_KEY = I18N_DECIMAL_PREFIX + Decimal5TimeDuration.class.getSimpleName();
	public static final String DECIMAL_5_TWO_DECIMAL_PLACES_KEY = I18N_DECIMAL_PREFIX + Decimal5TwoDecimalPlaces.class.getSimpleName();
	public static final String DECIMAL_5_TWO_DECIMAL_PLACES_PERCENTAGE_KEY = I18N_DECIMAL_PREFIX + Decimal5TwoDecimalPlacesPercentage.class.getSimpleName();
	private static final String I18N_CURRENCY_PREFIX = I18N_DECIMAL_PREFIX + "currency.";
	public static final String DECIMAL_10_DOLLARS_AND_CENTS_KEY = I18N_CURRENCY_PREFIX + Decimal10DollarsAndCents.class.getSimpleName();
	public static final String DECIMAL_2_DOLLARS_AND_CENTS_KEY = I18N_CURRENCY_PREFIX + Decimal2DollarsAndCents.class.getSimpleName();
	public static final String DECIMAL_2_DOLLARS_AND_CENTS_ABSOLUTE_KEY = I18N_CURRENCY_PREFIX + Decimal2DollarsAndCentsAbsolute.class.getSimpleName();
	public static final String DECIMAL_5_DOLLARS_AND_CENTS_KEY = I18N_CURRENCY_PREFIX + Decimal5DollarsAndCents.class.getSimpleName();
	
	public static final String DYNAMIC_ENUMERATION_CONVERTER_KEY = I18N_PREFIX + "enumeration." + DynamicEnumerationConverter.class.getSimpleName();
	
	public static final String GEOMETRY_CONVERTER_KEY = I18N_PREFIX + "geometry." + GeometryConverter.class.getSimpleName();
	
	private static final String I18N_INTEGER_PREFIX = I18N_PREFIX + "integer.";
	public static final String INTEGER_CONVERTER_KEY = I18N_INTEGER_PREFIX + IntegerConverter.class.getSimpleName();
	public static final String INTEGER_SEPARATOR_KEY = I18N_INTEGER_PREFIX + IntegerSeparator.class.getSimpleName();
	public static final String LONG_INTEGER_CONVERTER_KEY = I18N_INTEGER_PREFIX + LongIntegerConverter.class.getSimpleName();
	public static final String LONG_INTEGER_SEPARATOR_KEY = I18N_INTEGER_PREFIX + LongIntegerSeparator.class.getSimpleName();
	public static final String SIMPLE_PERCENTAGE_KEY = I18N_INTEGER_PREFIX + SimplePercentage.class.getSimpleName();

	private static final String I18N_TIME_PREFIX = I18N_PREFIX + "time.";
	public static final String HH_MI_SS_KEY = I18N_TIME_PREFIX + HH_MI_SS.class.getSimpleName();
	public static final String HH_MI_KEY = I18N_TIME_PREFIX + HH_MI.class.getSimpleName();
	public static final String HH24_MI_SS_KEY = I18N_TIME_PREFIX + HH24_MI_SS.class.getSimpleName();
	public static final String HH24_MI_KEY = I18N_TIME_PREFIX + HH24_MI.class.getSimpleName();

	private static final String I18N_TIMESTAMP_PREFIX = I18N_PREFIX + "timestamp.";
	private static final String I18N_TIMESTAMP_SUFFIX = "_Timestamp";
	public static final String DD_MM_YYYY_HH_MI_SS_KEY = I18N_TIMESTAMP_PREFIX + DD_MM_YYYY_HH_MI_SS.class.getSimpleName();
	public static final String DD_MM_YYYY_HH24_MI_SS_KEY = I18N_TIMESTAMP_PREFIX + DD_MM_YYYY_HH24_MI_SS.class.getSimpleName();
	public static final String DD_MM_YYYY_TIMESTAMP_KEY = I18N_TIMESTAMP_PREFIX + DD_MM_YYYY.class.getSimpleName() + I18N_TIMESTAMP_SUFFIX;
	public static final String DD_MMM_YYYY_HH_MI_SS_KEY = I18N_TIMESTAMP_PREFIX + DD_MMM_YYYY_HH_MI_SS.class.getSimpleName();
	public static final String DD_MMM_YYYY_HH24_MI_SS_KEY = I18N_TIMESTAMP_PREFIX + DD_MMM_YYYY_HH24_MI_SS.class.getSimpleName();
	public static final String DD_MMM_YYYY_TIMESTAMP_KEY = I18N_TIMESTAMP_PREFIX + DD_MMM_YYYY.class.getSimpleName() + I18N_TIMESTAMP_SUFFIX;
	public static final String MM_DD_YYYY_HH_MI_SS_KEY = I18N_TIMESTAMP_PREFIX + MM_DD_YYYY_HH_MI_SS.class.getSimpleName();
	public static final String MM_DD_YYYY_HH24_MI_SS_KEY = I18N_TIMESTAMP_PREFIX + MM_DD_YYYY_HH24_MI_SS.class.getSimpleName();
	public static final String MM_DD_YYYY_TIMESTAMP_KEY = I18N_TIMESTAMP_PREFIX + MM_DD_YYYY.class.getSimpleName() + I18N_TIMESTAMP_SUFFIX;
	public static final String MMM_DD_YYYY_HH_MI_SS_KEY = I18N_TIMESTAMP_PREFIX + MMM_DD_YYYY_HH_MI_SS.class.getSimpleName();
	public static final String MMM_DD_YYYY_HH24_MI_SS_KEY = I18N_TIMESTAMP_PREFIX + MMM_DD_YYYY_HH24_MI_SS.class.getSimpleName();
	public static final String MMM_DD_YYYY_TIMESTAMP_KEY = I18N_TIMESTAMP_PREFIX + MMM_DD_YYYY.class.getSimpleName() + I18N_TIMESTAMP_SUFFIX;
	public static final String YYYY_MM_DD_HH_MI_SS_KEY = I18N_TIMESTAMP_PREFIX + YYYY_MM_DD_HH_MI_SS.class.getSimpleName();
	public static final String YYYY_MM_DD_HH24_MI_SS_KEY = I18N_TIMESTAMP_PREFIX + YYYY_MM_DD_HH24_MI_SS.class.getSimpleName();
	public static final String YYYY_MM_DD_TIMESTAMP_KEY = I18N_TIMESTAMP_PREFIX + YYYY_MM_DD.class.getSimpleName() + I18N_TIMESTAMP_SUFFIX;
	
	private List<Message> messages = null;
	
	public ConversionException(String i18nKey, String... i18nValues) {
		super("Conversion failed for converter :- " + i18nKey, false);
		messages = Collections.singletonList(new Message(Util.i18n(i18nKey, i18nValues)));
	}

	public ConversionException(String i18nKey, Throwable t) {
		super("Conversion failed for converter :- " + i18nKey, t, false);
		messages = Collections.singletonList(new Message(Util.i18n(i18nKey)));
	}

	@Override
	public List<Message> getMessages() {
		return messages;
	}
}
