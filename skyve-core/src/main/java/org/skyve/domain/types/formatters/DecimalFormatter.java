package org.skyve.domain.types.formatters;

import org.skyve.CORE;

/**
 * Format a java.lang.Number into a decimal representation.
 */
public class DecimalFormatter implements Formatter<Number> {
	public static final String ZERO_DECIMAL_PLACES_PATTERN = "###,###,###,##0";
	public static final String ONE_DECIMAL_PLACE_PATTERN = "###,###,###,##0.0";
	public static final String TWO_DECIMAL_PLACES_PATTERN = "###,###,###,##0.00";
	public static final String THREE_DECIMAL_PLACES_PATTERN = "###,###,###,##0.000";
	public static final String FOUR_DECIMAL_PLACES_PATTERN = "###,###,###,##0.0000";
	public static final String FIVE_DECIMAL_PLACES_PATTERN = "###,###,###,##0.00000";
	public static final String SIX_DECIMAL_PLACES_PATTERN = "###,###,###,##0.000000";
	public static final String SEVEN_DECIMAL_PLACES_PATTERN = "###,###,###,##0.0000000";
	public static final String EIGHT_DECIMAL_PLACES_PATTERN = "###,###,###,##0.00000000";
	public static final String NINE_DECIMAL_PLACES_PATTERN = "###,###,###,##0.000000000";
	public static final String TEN_DECIMAL_PLACES_PATTERN = "###,###,###,##0.0000000000";
	public static final String ONE_OPTIONAL_DECIMAL_PLACE_PATTERN = "###,###,###,##0.#";
	public static final String TWO_OPTIONAL_DECIMAL_PLACES_PATTERN = "###,###,###,##0.##";
	public static final String THREE_OPTIONAL_DECIMAL_PLACES_PATTERN = "###,###,###,##0.###";
	public static final String FOUR_OPTIONAL_DECIMAL_PLACES_PATTERN = "###,###,###,##0.####";
	public static final String FIVE_OPTIONAL_DECIMAL_PLACES_PATTERN = "###,###,###,##0.#####";
	public static final String SIX_OPTIONAL_DECIMAL_PLACES_PATTERN = "###,###,###,##0.######";
	public static final String SEVEN_OPTIONAL_DECIMAL_PLACES_PATTERN = "###,###,###,##0.#######";
	public static final String EIGHT_OPTIONAL_DECIMAL_PLACES_PATTERN = "###,###,###,##0.########";
	public static final String NINE_OPTIONAL_DECIMAL_PLACES_PATTERN = "###,###,###,##0.#########";
	public static final String TEN_OPTIONAL_DECIMAL_PLACES_PATTERN = "###,###,###,##0.##########";

	private String decimalFormatPattern;
	private boolean absolute;
	private Boolean calculatedPercentage;
	private boolean floatingPoint;
	
	public DecimalFormatter(String decimalFormatPattern, boolean absolute, Boolean calculatedPercentage) {
		this.decimalFormatPattern = decimalFormatPattern;
		this.absolute = absolute;
		this.calculatedPercentage = calculatedPercentage;
		this.floatingPoint = (calculatedPercentage != null) || (decimalFormatPattern.indexOf('.') > -1);
	}

	@Override
	public Class<Number> getValueType() {
		return Number.class;
	}
	
	@Override
	public String toDisplayValue(Number value) {
		java.text.DecimalFormat df = CORE.getDecimalFormat(decimalFormatPattern);
		if (absolute) {
			df.setNegativePrefix("");
			df.setNegativeSuffix("");
		}
		if (calculatedPercentage != null) {
			double num = value.doubleValue();
			if (Boolean.TRUE.equals(calculatedPercentage)) {
				num /= 100.0;
			}
			return df.format(num) + '%';
		}
		return df.format(floatingPoint ? value.doubleValue() : value.longValue());
	}
}
