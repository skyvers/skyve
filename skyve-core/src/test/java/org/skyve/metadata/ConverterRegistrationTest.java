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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Properties;
import java.util.Set;
import java.util.TreeSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;
import org.skyve.domain.messages.ConversionException;

/**
 * Checks that every {@link ConverterName} is registered everywhere a converter has to be
 * declared, and that every conversion error message exists in each translated bundle.
 */
@SuppressWarnings("static-method")
class ConverterRegistrationTest {
	private static final List<Path> SCHEMAS = List.of(Path.of("src/test/resources/schemas/common.xsd"),
														Path.of("../skyve-war/src/main/java/schemas/common.xsd"));
	private static final Path FACES_CONFIG = Path.of("../skyve-web/src/main/resources/META-INF/faces-config.xml");
	private static final Path VUE_CONVERTERS = Path.of("../skyve-vue/src/support/Converters.js");
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
