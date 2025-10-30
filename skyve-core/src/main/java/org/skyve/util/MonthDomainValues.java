package org.skyve.util;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Locale;

import org.skyve.metadata.model.document.Bizlet.DomainValue;

/**
 * Provides DomainValue lists and helper methods for calendar month abbreviations.
 */
public final class MonthDomainValues {
	private MonthDomainValues() {
	}

	/** abbreviated forms of calendar months */
	public static enum CalendarMonth {
		JAN, FEB, MAR, APR, MAY, JUN, JUL, AUG, SEP, OCT, NOV, DEC
	}

	/** Immutable list of 3-letter upper-case month abbreviations (JAN..DEC) as DomainValues. */
	public static final List<DomainValue> CALENDAR_MONTHS;
	static {
		List<DomainValue> months = new ArrayList<>(12);
		for (CalendarMonth m : CalendarMonth.values()) {
			months.add(new DomainValue(m.name()));
		}
		CALENDAR_MONTHS = Collections.unmodifiableList(months);
	}

	/**
	 * Convert a month abbreviation (JAN..DEC case-insensitive) to a zero-based month index.
	 * Returns -1 if not recognised.
	 *
	 * @param monthAbbrev 3-letter or full month name (case-insensitive)
	 * @return 0 for January .. 11 for December or -1 if invalid
	 */
	public static int monthNameToZeroBasedIndex(String monthAbbrev) {
		if (monthAbbrev == null || monthAbbrev.isEmpty()) {
			return -1;
		}
		String upper = monthAbbrev.toUpperCase(Locale.ENGLISH);
		if (upper.length() > 3) { // normalise any longer form by first 3 letters
			upper = upper.substring(0, 3);
		}
		try {
			return CalendarMonth.valueOf(upper).ordinal();
		} catch (@SuppressWarnings("unused") IllegalArgumentException e) {
			return -1;
		}
	}
}
