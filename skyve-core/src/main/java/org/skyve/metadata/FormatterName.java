package org.skyve.metadata;

import org.skyve.domain.types.formatters.DecimalFormatter;
import org.skyve.domain.types.formatters.Formatter;
import org.skyve.domain.types.formatters.SimpleDateFormatter;
import org.skyve.domain.types.formatters.TimeDurationFormatter;
import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlType;

/**
 * Represents the Skyve built-in formatters that are supported in XML schema.
 */
@XmlType(namespace = XMLMetaData.COMMON_NAMESPACE)
public enum FormatterName {
	DD_MM_YYYY(new SimpleDateFormatter(org.skyve.domain.types.converters.date.DD_MM_YYYY.PATTERN)),
	DD_MMM_YYYY(new SimpleDateFormatter(org.skyve.domain.types.converters.date.DD_MMM_YYYY.PATTERN)),
	MM_DD_YYYY(new SimpleDateFormatter(org.skyve.domain.types.converters.date.MM_DD_YYYY.PATTERN)),
	MMM_DD_YYYY(new SimpleDateFormatter(org.skyve.domain.types.converters.date.MMM_DD_YYYY.PATTERN)),
	YYYY_MM_DD(new SimpleDateFormatter(org.skyve.domain.types.converters.date.YYYY_MM_DD.PATTERN)),
	DD_MM_YYYY_HH_MI(new SimpleDateFormatter(org.skyve.domain.types.converters.datetime.DD_MM_YYYY_HH_MI.PATTERN)),
	DD_MM_YYYY_HH24_MI(new SimpleDateFormatter(org.skyve.domain.types.converters.datetime.DD_MM_YYYY_HH24_MI.PATTERN)),
	DD_MMM_YYYY_HH_MI(new SimpleDateFormatter(org.skyve.domain.types.converters.datetime.DD_MMM_YYYY_HH_MI.PATTERN)),
	DD_MMM_YYYY_HH24_MI(new SimpleDateFormatter(org.skyve.domain.types.converters.datetime.DD_MMM_YYYY_HH24_MI.PATTERN)),
	MM_DD_YYYY_HH_MI(new SimpleDateFormatter(org.skyve.domain.types.converters.datetime.MM_DD_YYYY_HH_MI.PATTERN)), 
	MM_DD_YYYY_HH24_MI(new SimpleDateFormatter(org.skyve.domain.types.converters.datetime.MM_DD_YYYY_HH24_MI.PATTERN)),
	MMM_DD_YYYY_HH_MI(new SimpleDateFormatter(org.skyve.domain.types.converters.datetime.MMM_DD_YYYY_HH_MI.PATTERN)), 
	MMM_DD_YYYY_HH24_MI(new SimpleDateFormatter(org.skyve.domain.types.converters.datetime.MMM_DD_YYYY_HH24_MI.PATTERN)),
	YYYY_MM_DD_HH_MI(new SimpleDateFormatter(org.skyve.domain.types.converters.datetime.YYYY_MM_DD_HH_MI.PATTERN)), 
	YYYY_MM_DD_HH24_MI(new SimpleDateFormatter(org.skyve.domain.types.converters.datetime.YYYY_MM_DD_HH24_MI.PATTERN)),
	HH_MI(new SimpleDateFormatter(org.skyve.domain.types.converters.time.HH_MI.PATTERN)),
	HH24_MI(new SimpleDateFormatter(org.skyve.domain.types.converters.time.HH24_MI.PATTERN)),
	HH_MI_SS(new SimpleDateFormatter(org.skyve.domain.types.converters.time.HH_MI_SS.PATTERN)),
	HH24_MI_SS(new SimpleDateFormatter(org.skyve.domain.types.converters.time.HH24_MI_SS.PATTERN)),
	DD_MM_YYYY_HH_MI_SS(new SimpleDateFormatter(org.skyve.domain.types.converters.timestamp.DD_MM_YYYY_HH_MI_SS.PATTERN)),
	DD_MM_YYYY_HH24_MI_SS(new SimpleDateFormatter(org.skyve.domain.types.converters.timestamp.DD_MM_YYYY_HH24_MI_SS.PATTERN)),
	DD_MMM_YYYY_HH_MI_SS(new SimpleDateFormatter(org.skyve.domain.types.converters.timestamp.DD_MMM_YYYY_HH_MI_SS.PATTERN)),
	DD_MMM_YYYY_HH24_MI_SS(new SimpleDateFormatter(org.skyve.domain.types.converters.timestamp.DD_MMM_YYYY_HH24_MI_SS.PATTERN)),
	MM_DD_YYYY_HH_MI_SS(new SimpleDateFormatter(org.skyve.domain.types.converters.timestamp.MM_DD_YYYY_HH_MI_SS.PATTERN)),
	MM_DD_YYYY_HH24_MI_SS(new SimpleDateFormatter(org.skyve.domain.types.converters.timestamp.MM_DD_YYYY_HH24_MI_SS.PATTERN)),
	MMM_DD_YYYY_HH_MI_SS(new SimpleDateFormatter(org.skyve.domain.types.converters.timestamp.MMM_DD_YYYY_HH_MI_SS.PATTERN)),
	MMM_DD_YYYY_HH24_MI_SS(new SimpleDateFormatter(org.skyve.domain.types.converters.timestamp.MMM_DD_YYYY_HH24_MI_SS.PATTERN)),
	YYYY_MM_DD_HH_MI_SS(new SimpleDateFormatter(org.skyve.domain.types.converters.timestamp.YYYY_MM_DD_HH_MI_SS.PATTERN)),
	YYYY_MM_DD_HH24_MI_SS(new SimpleDateFormatter(org.skyve.domain.types.converters.timestamp.YYYY_MM_DD_HH24_MI_SS.PATTERN)),
	Integer(new DecimalFormatter(DecimalFormatter.ZERO_DECIMAL_PLACES_PATTERN, false, null)),
	OneDecimalPlace(new DecimalFormatter(DecimalFormatter.ONE_DECIMAL_PLACE_PATTERN, false, null)),
	TwoDecimalPlaces(new DecimalFormatter(DecimalFormatter.TWO_DECIMAL_PLACES_PATTERN, false, null)),
	ThreeDecimalPlaces(new DecimalFormatter(DecimalFormatter.THREE_DECIMAL_PLACES_PATTERN, false, null)),
	FourDecimalPlaces(new DecimalFormatter(DecimalFormatter.FOUR_DECIMAL_PLACES_PATTERN, false, null)),
	FiveDecimalPlaces(new DecimalFormatter(DecimalFormatter.FIVE_DECIMAL_PLACES_PATTERN, false, null)),
	SixDecimalPlaces(new DecimalFormatter(DecimalFormatter.SIX_DECIMAL_PLACES_PATTERN, false, null)),
	SevenDecimalPlaces(new DecimalFormatter(DecimalFormatter.SEVEN_DECIMAL_PLACES_PATTERN, false,  null)),
	EightDecimalPlaces(new DecimalFormatter(DecimalFormatter.EIGHT_DECIMAL_PLACES_PATTERN, false, null)),
	NineDecimalPlaces(new DecimalFormatter(DecimalFormatter.NINE_DECIMAL_PLACES_PATTERN, false, null)),
	TenDecimalPlaces(new DecimalFormatter(DecimalFormatter.TEN_DECIMAL_PLACES_PATTERN, false, null)),
	OneOptionalDecimalPlace(new DecimalFormatter(DecimalFormatter.ONE_OPTIONAL_DECIMAL_PLACE_PATTERN, false, null)),
	TwoOptionalDecimalPlaces(new DecimalFormatter(DecimalFormatter.TWO_OPTIONAL_DECIMAL_PLACES_PATTERN, false, null)),
	ThreeOptionalDecimalPlaces(new DecimalFormatter(DecimalFormatter.THREE_OPTIONAL_DECIMAL_PLACES_PATTERN, false, null)),
	FourOptionalDecimalPlaces(new DecimalFormatter(DecimalFormatter.FOUR_OPTIONAL_DECIMAL_PLACES_PATTERN, false, null)),
	FiveOptionalDecimalPlaces(new DecimalFormatter(DecimalFormatter.FIVE_OPTIONAL_DECIMAL_PLACES_PATTERN, false, null)),
	SixOptionalDecimalPlaces(new DecimalFormatter(DecimalFormatter.SIX_OPTIONAL_DECIMAL_PLACES_PATTERN, false, null)),
	SevenOptionalDecimalPlaces(new DecimalFormatter(DecimalFormatter.SEVEN_OPTIONAL_DECIMAL_PLACES_PATTERN, false,  null)),
	EightOptionalDecimalPlaces(new DecimalFormatter(DecimalFormatter.EIGHT_OPTIONAL_DECIMAL_PLACES_PATTERN, false, null)),
	NineOptionalDecimalPlaces(new DecimalFormatter(DecimalFormatter.NINE_OPTIONAL_DECIMAL_PLACES_PATTERN, false, null)),
	TenOptionalDecimalPlaces(new DecimalFormatter(DecimalFormatter.TEN_OPTIONAL_DECIMAL_PLACES_PATTERN, false, null)),
	IntegerPercentage(new DecimalFormatter(DecimalFormatter.ZERO_DECIMAL_PLACES_PATTERN, false, Boolean.TRUE)),
	OneDecimalPlacePercentage(new DecimalFormatter(DecimalFormatter.ONE_DECIMAL_PLACE_PATTERN, false, Boolean.TRUE)),
	TwoDecimalPlacesPercentage(new DecimalFormatter(DecimalFormatter.TWO_DECIMAL_PLACES_PATTERN, false, Boolean.TRUE)),
	ThreeDecimalPlacesPercentage(new DecimalFormatter(DecimalFormatter.THREE_DECIMAL_PLACES_PATTERN, false, Boolean.TRUE)),
	FourDecimalPlacesPercentage(new DecimalFormatter(DecimalFormatter.FOUR_DECIMAL_PLACES_PATTERN, false, Boolean.TRUE)),
	FiveDecimalPlacesPercentage(new DecimalFormatter(DecimalFormatter.FIVE_DECIMAL_PLACES_PATTERN, false, Boolean.TRUE)),
	SixDecimalPlacesPercentage(new DecimalFormatter(DecimalFormatter.SIX_DECIMAL_PLACES_PATTERN, false, Boolean.TRUE)),
	SevenDecimalPlacesPercentage(new DecimalFormatter(DecimalFormatter.SEVEN_DECIMAL_PLACES_PATTERN, false, Boolean.TRUE)),
	EightDecimalPlacesPercentage(new DecimalFormatter(DecimalFormatter.EIGHT_DECIMAL_PLACES_PATTERN, false, Boolean.TRUE)),
	NineDecimalPlacesPercentage(new DecimalFormatter(DecimalFormatter.NINE_DECIMAL_PLACES_PATTERN, false, Boolean.TRUE)),
	TenDecimalPlacesPercentage(new DecimalFormatter(DecimalFormatter.TEN_DECIMAL_PLACES_PATTERN, false, Boolean.TRUE)),
	IntegerSimplePercentage(new DecimalFormatter(DecimalFormatter.ZERO_DECIMAL_PLACES_PATTERN, false, Boolean.FALSE)),
	OneDecimalPlaceSimplePercentage(new DecimalFormatter(DecimalFormatter.ONE_DECIMAL_PLACE_PATTERN, false, Boolean.FALSE)),
	TwoDecimalPlacesSimplePercentage(new DecimalFormatter(DecimalFormatter.TWO_DECIMAL_PLACES_PATTERN, false, Boolean.FALSE)),
	ThreeDecimalPlacesSimplePercentage(new DecimalFormatter(DecimalFormatter.THREE_DECIMAL_PLACES_PATTERN, false, Boolean.FALSE)),
	FourDecimalPlacesSimplePercentage(new DecimalFormatter(DecimalFormatter.FOUR_DECIMAL_PLACES_PATTERN, false, Boolean.FALSE)),
	FiveDecimalPlacesSimplePercentage(new DecimalFormatter(DecimalFormatter.FIVE_DECIMAL_PLACES_PATTERN, false, Boolean.FALSE)),
	SixDecimalPlacesSimplePercentage(new DecimalFormatter(DecimalFormatter.SIX_DECIMAL_PLACES_PATTERN, false, Boolean.FALSE)),
	SevenDecimalPlacesSimplePercentage(new DecimalFormatter(DecimalFormatter.SEVEN_DECIMAL_PLACES_PATTERN, false, Boolean.FALSE)),
	EightDecimalPlacesSimplePercentage(new DecimalFormatter(DecimalFormatter.EIGHT_DECIMAL_PLACES_PATTERN, false, Boolean.FALSE)),
	NineDecimalPlacesSimplePercentage(new DecimalFormatter(DecimalFormatter.NINE_DECIMAL_PLACES_PATTERN, false, Boolean.FALSE)),
	TenDecimalPlacesSimplePercentage(new DecimalFormatter(DecimalFormatter.TEN_DECIMAL_PLACES_PATTERN, false, Boolean.FALSE)),
	AbsoluteInteger(new DecimalFormatter(DecimalFormatter.ZERO_DECIMAL_PLACES_PATTERN, true, null)),
	AbsoluteOneDecimalPlace(new DecimalFormatter(DecimalFormatter.ONE_DECIMAL_PLACE_PATTERN, true, null)),
	AbsoluteTwoDecimalPlaces(new DecimalFormatter(DecimalFormatter.TWO_DECIMAL_PLACES_PATTERN, true, null)),
	AbsoluteThreeDecimalPlaces(new DecimalFormatter(DecimalFormatter.THREE_DECIMAL_PLACES_PATTERN, true, null)),
	AbsoluteFourDecimalPlaces(new DecimalFormatter(DecimalFormatter.FOUR_DECIMAL_PLACES_PATTERN, true, null)),
	AbsoluteFiveDecimalPlaces(new DecimalFormatter(DecimalFormatter.FIVE_DECIMAL_PLACES_PATTERN, true, null)),
	AbsoluteSixDecimalPlaces(new DecimalFormatter(DecimalFormatter.SIX_DECIMAL_PLACES_PATTERN, true, null)),
	AbsoluteSevenDecimalPlaces(new DecimalFormatter(DecimalFormatter.SEVEN_DECIMAL_PLACES_PATTERN, true, null)),
	AbsoluteEightDecimalPlaces(new DecimalFormatter(DecimalFormatter.EIGHT_DECIMAL_PLACES_PATTERN, true, null)),
	AbsoluteNineDecimalPlaces(new DecimalFormatter(DecimalFormatter.NINE_DECIMAL_PLACES_PATTERN, true, null)),
	AbsoluteTenDecimalPlaces(new DecimalFormatter(DecimalFormatter.TEN_DECIMAL_PLACES_PATTERN, true, null)),
	AbsoluteOneOptionalDecimalPlace(new DecimalFormatter(DecimalFormatter.ONE_OPTIONAL_DECIMAL_PLACE_PATTERN, true, null)),
	AbsoluteTwoOptionalDecimalPlaces(new DecimalFormatter(DecimalFormatter.TWO_OPTIONAL_DECIMAL_PLACES_PATTERN, true, null)),
	AbsoluteThreeOptionalDecimalPlaces(new DecimalFormatter(DecimalFormatter.THREE_OPTIONAL_DECIMAL_PLACES_PATTERN, true, null)),
	AbsoluteFourOptionalDecimalPlaces(new DecimalFormatter(DecimalFormatter.FOUR_OPTIONAL_DECIMAL_PLACES_PATTERN, true, null)),
	AbsoluteFiveOptionalDecimalPlaces(new DecimalFormatter(DecimalFormatter.FIVE_OPTIONAL_DECIMAL_PLACES_PATTERN, true, null)),
	AbsoluteSixOptionalDecimalPlaces(new DecimalFormatter(DecimalFormatter.SIX_OPTIONAL_DECIMAL_PLACES_PATTERN, true, null)),
	AbsoluteSevenOptionalDecimalPlaces(new DecimalFormatter(DecimalFormatter.SEVEN_OPTIONAL_DECIMAL_PLACES_PATTERN, true, null)),
	AbsoluteEightOptionalDecimalPlaces(new DecimalFormatter(DecimalFormatter.EIGHT_OPTIONAL_DECIMAL_PLACES_PATTERN, true, null)),
	AbsoluteNineOptionalDecimalPlaces(new DecimalFormatter(DecimalFormatter.NINE_OPTIONAL_DECIMAL_PLACES_PATTERN, true, null)),
	AbsoluteTenOptionalDecimalPlaces(new DecimalFormatter(DecimalFormatter.TEN_OPTIONAL_DECIMAL_PLACES_PATTERN, true, null)),
	TimeDuration(new TimeDurationFormatter());

	private Formatter<?> formatter;
	private FormatterName(Formatter<?> formatter) {
		this.formatter = formatter;
	}
	
	@SuppressWarnings("unchecked")
	public <T> Formatter<T> getFormatter() {
		return (Formatter<T>) formatter;
	}
}
