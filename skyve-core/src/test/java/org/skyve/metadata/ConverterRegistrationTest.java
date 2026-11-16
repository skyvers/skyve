package org.skyve.metadata;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;
import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.TimeOnly;
import org.skyve.domain.types.Timestamp;
import org.skyve.domain.types.converters.Converter;
import org.skyve.metadata.model.Attribute.AttributeType;

/**
 * Checks that every {@link ConverterName} is registered everywhere a converter has to be
 * declared, that every conversion error message exists in each translated bundle, and that
 * the Vue list grid formats each date and time converter's values as the Java converter does,
 * in its cells and in its column filter calendars.
 */
@SuppressWarnings("static-method")
class ConverterRegistrationTest {
	private static final List<Path> SCHEMAS = List.of(Path.of("src/test/resources/schemas/common.xsd"),
														Path.of("../skyve-war/src/main/java/schemas/common.xsd"));
	private static final Path FACES_CONFIG = Path.of("../skyve-web/src/main/resources/META-INF/faces-config.xml");
	private static final Path VUE_CONVERTERS = Path.of("../skyve-vue/src/support/Converters.js");
	private static final Path VUE_MAIN = Path.of("../skyve-vue/src/main.js");
	private static final Path WAR_RESOURCES = Path.of("../skyve-war/src/main/java/resources");
	private static final Path WEB_TEST_BUNDLE = Path.of("../skyve-web/src/test/java/resources/i18n.properties");
	private static final String CONVERSION_KEY_PREFIX = "exception.conversion.";

	@Test
	void everySchemaDeclaresExactlyTheConverterNames() throws IOException {
		Set<String> names = converterNames();
		Pattern enumeration = Pattern.compile("<xs:enumeration value=\"([^\"]+)\"/>");
		List<Executable> checks = new ArrayList<>();
		for (Path schema : SCHEMAS) {
			String xsd = Files.readString(schema, StandardCharsets.UTF_8);
			int start = xsd.indexOf("<xs:simpleType name=\"converterName\">");
			assertTrue(start >= 0, schema + " declares converterName");
			String type = xsd.substring(start, xsd.indexOf("</xs:simpleType>", start));
			Set<String> declared = matches(enumeration, type);
			checks.add(() -> assertEquals(names, declared, schema + " converterName enumeration"));
		}
		assertAll(checks.stream());
	}

	@Test
	void everyConverterNameHasAFacesConverter() throws IOException {
		Set<String> ids = matches(Pattern.compile("<converter-id>([^<]+)</converter-id>"),
									Files.readString(FACES_CONFIG, StandardCharsets.UTF_8));
		assertAll(converterNames().stream()
							.map(name -> () -> assertTrue(ids.contains(name), name + " has a converter-id in " + FACES_CONFIG)));
	}

	@Test
	void vueDeclaresAFormatterForExactlyTheConverterNames() throws IOException {
		String source = Files.readString(VUE_CONVERTERS, StandardCharsets.UTF_8);
		int start = source.indexOf("const Converters = {");
		assertTrue(start >= 0, VUE_CONVERTERS + " declares Converters");
		String block = source.substring(start, source.indexOf("\n};", start));
		Set<String> formatters = matches(Pattern.compile("(?m)^\\s+(\\w+)\\s*[:,]"), block);
		Set<String> names = converterNames();
		Set<String> missing = new TreeSet<>(names);
		missing.removeAll(formatters);
		Set<String> unknown = new TreeSet<>(formatters);
		unknown.removeAll(names);
		assertAll(() -> assertEquals(Set.of(), missing, "converter names without a formatter in " + VUE_CONVERTERS),
					() -> assertEquals(Set.of(), unknown, "formatters in " + VUE_CONVERTERS + " that are not converter names"));
	}

	/**
	 * A Vue list grid's cells show a date or time with the Luxon pattern of the column's converter, which must be
	 * the Java converter's pattern (Luxon reads the letters Skyve's patterns use as SimpleDateFormat does).
	 */
	@Test
	void vueFormatsEachDateAndTimeCellWithItsJavaConverterPattern() throws IOException {
		String source = Files.readString(VUE_CONVERTERS, StandardCharsets.UTF_8);
		Map<String, String> patterns = new TreeMap<>();
		Matcher matcher = Pattern.compile("(?m)^\\s+(\\w+): _createDateFormatter\\('([^']*)'\\)").matcher(source);
		while (matcher.find()) {
			patterns.put(matcher.group(1), matcher.group(2));
		}
		Map<String, String> expected = new TreeMap<>();
		for (ConverterName name : dateAndTimeConverterNames()) {
			expected.put(name.name(), name.getConverter().getFormatPattern());
		}
		assertEquals(expected, patterns, "the date formatters in " + VUE_CONVERTERS + " and their Java converters' patterns");
	}

	/**
	 * A Vue list grid's column filter calendar (PrimeVue's Calendar, with the props CALENDARS in Converters.js gives
	 * the column's converter) must show a value as the Java converter writes it, and read that text back as the same
	 * value, so a user can type what a cell shows. PrimeVue's formats are checked through the equivalent
	 * SimpleDateFormat pattern: PrimeVue writes the date (dd day, mm month, M short month name, yy 4 digit year, with
	 * English month names), then a space and the time: HH:mm, or hh:mm then a space and the locale's AM or PM, with
	 * :ss after the minutes when it shows seconds.
	 */
	@Test
	void vueFilterCalendarTakesAndShowsEachDateAndTimeAsItsJavaConverterWritesIt() throws IOException {
		String main = Files.readString(VUE_MAIN, StandardCharsets.UTF_8);
		Map<String, String> calendars = vueCalendarPatterns();
		Set<String> names = new TreeSet<>();
		for (ConverterName name : dateAndTimeConverterNames()) {
			names.add(name.name());
		}
		List<Executable> checks = new ArrayList<>();
		checks.add(() -> assertTrue(main.contains("am: 'AM', pm: 'PM'"),
									VUE_MAIN + " gives PrimeVue's locale AM and PM, as the Java converters write a 12 hour time"));
		checks.add(() -> assertEquals(names, calendars.keySet(), "the converters with a filter calendar in " + VUE_CONVERTERS));
		// The first hour after midnight and noon (12 AM and 12 PM), and a day and month that read as another date if swapped
		Date[] samples = {sample(2026, 5, 6, 14, 47, 25), sample(2021, 9, 5, 0, 7, 9), sample(2024, 12, 31, 12, 30, 59)};
		for (ConverterName name : dateAndTimeConverterNames()) {
			String calendarPattern = calendars.get(name.name());
			if (calendarPattern != null) {
				for (Date sample : samples) {
					checks.add(() -> checkCalendarRoundTrip(name, calendarPattern, sample));
				}
			}
		}
		assertAll(checks.stream());
	}

	@Test
	void everyConversionMessageExistsInEveryBundleThatTranslatesThem() throws IOException, IllegalAccessException {
		Set<String> keys = conversionKeys();
		assertFalse(keys.isEmpty());
		List<Path> bundles = new ArrayList<>();
		try (var paths = Files.list(WAR_RESOURCES)) {
			paths.filter(p -> p.getFileName().toString().matches("i18n(_\\w+)?\\.properties")).sorted().forEach(bundles::add);
		}
		bundles.add(WEB_TEST_BUNDLE);
		List<Executable> checks = new ArrayList<>();
		for (Path bundle : bundles) {
			Set<String> bundleKeys = load(bundle).stringPropertyNames();
			boolean translatesConversions = bundleKeys.stream().anyMatch(k -> k.startsWith(CONVERSION_KEY_PREFIX));
			boolean base = bundle.getFileName().toString().equals("i18n.properties");
			if (base || translatesConversions) {
				for (String key : keys) {
					checks.add(() -> assertTrue(bundleKeys.contains(key), bundle + " defines " + key));
				}
				for (String key : bundleKeys) {
					if (key.startsWith(CONVERSION_KEY_PREFIX)) {
						checks.add(() -> assertTrue(keys.contains(key), bundle + " key " + key + " is used by ConversionException"));
					}
				}
			}
		}
		assertAll(checks.stream());
	}

	/**
	 * Checks that the calendar shows the value as the Java converter writes it, and reads that text as the value
	 * the Java converter reads from it.
	 */
	private static void checkCalendarRoundTrip(ConverterName name, String calendarPattern, Date sample) throws ConversionException, ParseException {
		@SuppressWarnings("unchecked")
		Converter<Date> converter = (Converter<Date>) name.getConverter();
		Date value = typed(converter.getAttributeType(), sample.getTime());
		String text = converter.toDisplayValue(value);
		SimpleDateFormat calendar = new SimpleDateFormat(calendarPattern, Locale.ENGLISH);
		calendar.setLenient(false);
		assertEquals(text, calendar.format(value), name + " calendar (" + calendarPattern + ") shows the converter's text");
		Date read = typed(converter.getAttributeType(), calendar.parse(text).getTime());
		assertEquals(converter.fromDisplayValue(text), read, name + " calendar (" + calendarPattern + ") reads " + text + " as the converter does");
	}

	/**
	 * The filter calendar of each converter in Converters.js's CALENDARS, as the SimpleDateFormat pattern of what the
	 * PrimeVue Calendar shows and reads with those props; empty if Converters.js has no CALENDARS.
	 */
	private static Map<String, String> vueCalendarPatterns() throws IOException {
		String source = Files.readString(VUE_CONVERTERS, StandardCharsets.UTF_8);
		Map<String, String> result = new TreeMap<>();
		int start = source.indexOf("const CALENDARS = {");
		if (start < 0) {
			return result;
		}
		String block = source.substring(start, source.indexOf("\n};", start));
		Pattern entry = Pattern.compile("(?m)^\\s+(\\w+): \\{ ([^}]*) \\},$");
		Pattern property = Pattern.compile("(\\w+): ('([^']*)'|true)");
		Matcher matcher = entry.matcher(block);
		while (matcher.find()) {
			Map<String, String> props = new TreeMap<>();
			for (String prop : matcher.group(2).split(", ")) {
				Matcher p = property.matcher(prop);
				assertTrue(p.matches(), matcher.group(1) + " calendar property " + prop);
				props.put(p.group(1), (p.group(3) == null) ? p.group(2) : p.group(3));
			}
			result.put(matcher.group(1), calendarPattern(matcher.group(1), props));
		}
		return result;
	}

	/**
	 * The SimpleDateFormat pattern of what a PrimeVue Calendar with these props shows and reads.
	 */
	private static String calendarPattern(String name, Map<String, String> props) {
		Set<String> known = Set.of("dateFormat", "hourFormat", "showSeconds");
		assertTrue(known.containsAll(props.keySet()), name + " calendar properties " + props.keySet());
		String dateFormat = props.get("dateFormat");
		String hourFormat = props.get("hourFormat");
		boolean seconds = "true".equals(props.get("showSeconds"));
		assertTrue((hourFormat == null) || hourFormat.equals("12") || hourFormat.equals("24"), name + " hourFormat " + hourFormat);
		assertTrue((dateFormat != null) || (hourFormat != null), name + " calendar has a date or a time");
		assertTrue((! seconds) || (hourFormat != null), name + " calendar shows seconds only with a time");
		StringBuilder result = new StringBuilder(32);
		if (dateFormat != null) {
			for (int i = 0, l = dateFormat.length(); i < l; i++) {
				char c = dateFormat.charAt(i);
				boolean doubled = (i + 1 < l) && (dateFormat.charAt(i + 1) == c);
				switch (c) {
					case 'd' -> result.append(doubled ? "dd" : "d");
					case 'm' -> result.append(doubled ? "MM" : "M");
					case 'M' -> result.append(doubled ? "MMMM" : "MMM");
					case 'y' -> result.append(doubled ? "yyyy" : "yy");
					default -> {
						assertFalse(Character.isLetter(c) || (c == '\'') || (c == '@') || (c == '!'), name + " dateFormat " + dateFormat + " has " + c);
						result.append(c);
					}
				}
				if (doubled && ("dmMy".indexOf(c) >= 0)) {
					i++;
				}
			}
		}
		if (hourFormat != null) {
			if (! result.isEmpty()) {
				result.append(' ');
			}
			result.append("12".equals(hourFormat) ? "hh" : "HH").append(":mm");
			if (seconds) {
				result.append(":ss");
			}
			if ("12".equals(hourFormat)) {
				result.append(" a");
			}
		}
		return result.toString();
	}

	/** A date and time in the default time zone; month is 1 to 12. */
	private static Date sample(int year, int month, int day, int hour, int minute, int second) {
		return new GregorianCalendar(year, month - 1, day, hour, minute, second).getTime();
	}

	/** The value of an attribute type, from milliseconds (a date drops its time, a time its date). */
	private static Date typed(AttributeType type, long millis) {
		return switch (type) {
			case date -> new DateOnly(millis);
			case dateTime -> new DateTime(millis);
			case timestamp -> new Timestamp(millis);
			case time -> new TimeOnly(millis);
			default -> throw new IllegalArgumentException(type + " is not a date or time type");
		};
	}

	/** The converter names of date, dateTime, timestamp and time converters. */
	private static List<ConverterName> dateAndTimeConverterNames() {
		List<ConverterName> result = new ArrayList<>();
		for (ConverterName name : ConverterName.values()) {
			AttributeType type = name.getConverter().getAttributeType();
			if ((type == AttributeType.date) || (type == AttributeType.dateTime) || (type == AttributeType.timestamp) || (type == AttributeType.time)) {
				result.add(name);
			}
		}
		return result;
	}

	private static Set<String> converterNames() {
		return Arrays.stream(ConverterName.values()).map(Enum::name).collect(Collectors.toCollection(TreeSet::new));
	}

	private static Set<String> conversionKeys() throws IllegalAccessException {
		Set<String> result = new TreeSet<>();
		for (Field field : ConversionException.class.getFields()) {
			int modifiers = field.getModifiers();
			if (Modifier.isStatic(modifiers) && (field.getType() == String.class) && field.getName().endsWith("_KEY")) {
				result.add((String) field.get(null));
			}
		}
		return result;
	}

	private static Set<String> matches(Pattern pattern, String text) {
		Set<String> result = new TreeSet<>();
		Matcher matcher = pattern.matcher(text);
		while (matcher.find()) {
			result.add(matcher.group(1));
		}
		return result;
	}

	private static Properties load(Path bundle) throws IOException {
		Properties result = new Properties();
		try (InputStream in = Files.newInputStream(bundle)) {
			result.load(in); // keys are ASCII, so the bundle encoding does not matter here
		}
		return result;
	}
}
