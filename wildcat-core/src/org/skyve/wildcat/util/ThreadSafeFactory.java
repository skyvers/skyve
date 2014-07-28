package org.skyve.wildcat.util;

import java.text.DecimalFormat;
import java.text.Format;
import java.text.SimpleDateFormat;
import java.util.Map;
import java.util.TreeMap;

public class ThreadSafeFactory {
	private static final ThreadLocal<TreeMap<String, Format>> THREAD_LOCAL_FORMAT_MAP = new ThreadLocal<TreeMap<String, Format>>() {
		@Override
		protected TreeMap<String, Format> initialValue() {
			return new TreeMap<>();
		}
	};

	/**
	 * Prevent instantiation
	 */
	private ThreadSafeFactory() {
		// no-op
	}

	public static SimpleDateFormat getDateFormat(String formatString) {
		SimpleDateFormat result = null;

		Map<String, Format> map = THREAD_LOCAL_FORMAT_MAP.get();
		result = (SimpleDateFormat) map.get(formatString);
		if (result == null) {
			result = new SimpleDateFormat(formatString);
			map.put(formatString, result);
		}

		return result;
	}

	public static SimpleDateFormat getSerializableDateFormat() {
		return getDateFormat("yyyy-MM-dd");
	}

	public static SimpleDateFormat getSerializableTimeFormat() {
		return getDateFormat("HH:mm:ss");
	}

	public static DecimalFormat getDecimalFormat(String formatString) {
		DecimalFormat result = null;

		Map<String, Format> map = THREAD_LOCAL_FORMAT_MAP.get();
		result = (DecimalFormat) map.get(formatString);
		if (result == null) {
			result = new DecimalFormat(formatString);
			map.put(formatString, result);
		}

		return result;
	}
}
