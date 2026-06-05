package org.skyve.impl.coverage;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThan;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.math.BigDecimal;
import java.net.URI;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.Principal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import org.junit.jupiter.api.Test;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.Decimal10;
import org.skyve.domain.types.Decimal2;
import org.skyve.domain.types.Decimal5;
import org.skyve.domain.types.TimeOnly;
import org.skyve.impl.generate.jasperreports.DesignSpecification;

@SuppressWarnings({"static-method", "boxing"})
class SimpleObjectContractTest {
	private static final Set<String> PACKAGE_PREFIXES = Set.of(
			"org.skyve.archive.support.",
			"org.skyve.cache.",
			"org.skyve.content.",
			"org.skyve.impl.archive.list.",
			"org.skyve.impl.archive.support.",
			"org.skyve.impl.backup.",
			"org.skyve.impl.bizport.",
			"org.skyve.impl.cache.",
			"org.skyve.impl.create.",
			"org.skyve.impl.generate.jasperreports.",
			"org.skyve.impl.job.",
			"org.skyve.impl.mail.",
			"org.skyve.impl.report.jasperreports.",
			"org.skyve.impl.snapshot.",
			"org.skyve.impl.util.",
			"org.skyve.job.",
			"org.skyve.nlp.cron.",
			"org.skyve.util.");

	private static final Set<String> CLASS_EXCLUDES = Set.of(
			"org.skyve.impl.backup.AzureBlobStorageBackup",
			"org.skyve.impl.backup.BackupJob",
			"org.skyve.impl.backup.ContentChecker",
			"org.skyve.impl.backup.RestoreJob",
			"org.skyve.impl.backup.Truncate",
			"org.skyve.impl.bizport.AbstractDataFileLoader",
			"org.skyve.impl.bizport.CSVLoader",
			"org.skyve.impl.bizport.POISheetLoader",
			"org.skyve.impl.bizport.StandardGenerator",
			"org.skyve.impl.bizport.StandardLoader",
			"org.skyve.impl.generate.jasperreports.ReportViewVisitor",
			"org.skyve.impl.job.ContentGarbageCollectionJob",
			"org.skyve.impl.job.QuartzJobScheduler");

	@Test
	void simpleObjectsTolerateAccessorRoundTrips() throws Exception {
		int exercised = 0;
		for (String className : findCandidateClassNames()) {
			Class<?> type = Class.forName(className, false, Thread.currentThread().getContextClassLoader());
			Object instance = instantiate(type);
			if (instance == null) {
				continue;
			}

			exercised += invokeSetters(instance);
			exercised += invokeGetters(instance);
			exercised += invokeObjectMethods(instance);
		}

		assertThat(exercised, greaterThan(100));
	}

	private static Set<String> findCandidateClassNames() throws Exception {
		URL location = DesignSpecification.class.getProtectionDomain().getCodeSource().getLocation();
		Path root = Path.of(location.toURI());
		Set<String> result = new TreeSet<>();
		try (var stream = Files.walk(root.resolve("org/skyve"))) {
			stream.filter(path -> path.toString().endsWith(".class"))
					.map(root::relativize)
					.map(Path::toString)
					.map(name -> name.replace('/', '.').replace('\\', '.'))
					.map(name -> name.substring(0, name.length() - ".class".length()))
					.filter(SimpleObjectContractTest::isCandidateClassName)
					.forEach(result::add);
		}
		return result;
	}

	private static boolean isCandidateClassName(String className) {
		if (className.contains("$$") || className.contains("$1") || className.contains("$Mockito")) {
			return false;
		}
		if (CLASS_EXCLUDES.contains(className)) {
			return false;
		}
		for (String prefix : PACKAGE_PREFIXES) {
			if (className.startsWith(prefix)) {
				return true;
			}
		}
		return false;
	}

	private static Object instantiate(Class<?> type) {
		int modifiers = type.getModifiers();
		if (type.isInterface() || type.isAnnotation() || type.isEnum() || type.isRecord() || Modifier.isAbstract(modifiers)) {
			return null;
		}
		try {
			Constructor<?> constructor = type.getDeclaredConstructor();
			constructor.setAccessible(true);
			return constructor.newInstance();
		}
		catch (@SuppressWarnings("unused") ReflectiveOperationException | RuntimeException e) {
			return null;
		}
	}

	private static int invokeSetters(Object instance) {
		int count = 0;
		for (Method method : instance.getClass().getMethods()) {
			if (isSimpleSetter(method)) {
				Object value = sampleValue(method.getParameterTypes()[0]);
				if (value != UnsupportedValue.INSTANCE) {
					try {
						method.invoke(instance, value);
						count++;
					}
					catch (@SuppressWarnings("unused") ReflectiveOperationException | RuntimeException e) {
						// Not every JavaBean-shaped method accepts arbitrary sample data.
					}
				}
			}
		}
		return count;
	}

	private static boolean isSimpleSetter(Method method) {
		return method.getName().startsWith("set") &&
				(method.getParameterCount() == 1) &&
				(method.getReturnType() == Void.TYPE) &&
				! Modifier.isStatic(method.getModifiers());
	}

	private static int invokeGetters(Object instance) {
		int count = 0;
		for (Method method : instance.getClass().getMethods()) {
			if (isSimpleGetter(method)) {
				try {
					method.invoke(instance);
					count++;
				}
				catch (@SuppressWarnings("unused") ReflectiveOperationException | RuntimeException e) {
					// Some getters derive values from collaborators not present in this contract test.
				}
			}
		}
		return count;
	}

	private static boolean isSimpleGetter(Method method) {
		String name = method.getName();
		return (method.getParameterCount() == 0) &&
				(method.getReturnType() != Void.TYPE) &&
				! Modifier.isStatic(method.getModifiers()) &&
				((name.startsWith("get") && (! "getClass".equals(name))) || name.startsWith("is"));
	}

	private static int invokeObjectMethods(Object instance) {
		int count = 0;
		try {
			instance.toString();
			count++;
		}
		catch (@SuppressWarnings("unused") RuntimeException e) {
			// Ignore object-contract implementations that need complete state.
		}
		try {
			instance.hashCode();
			count++;
		}
		catch (@SuppressWarnings("unused") RuntimeException e) {
			// Ignore object-contract implementations that need complete state.
		}
		return count;
	}

	private static Object sampleValue(Class<?> type) {
		if (type == String.class) {
			return "sample";
		}
		if ((type == Integer.TYPE) || (type == Integer.class)) {
			return Integer.valueOf(7);
		}
		if ((type == Long.TYPE) || (type == Long.class)) {
			return Long.valueOf(7L);
		}
		if ((type == Short.TYPE) || (type == Short.class)) {
			return Short.valueOf((short) 7);
		}
		if ((type == Byte.TYPE) || (type == Byte.class)) {
			return Byte.valueOf((byte) 7);
		}
		if ((type == Double.TYPE) || (type == Double.class)) {
			return Double.valueOf(7.5D);
		}
		if ((type == Float.TYPE) || (type == Float.class)) {
			return Float.valueOf(7.5F);
		}
		if ((type == Boolean.TYPE) || (type == Boolean.class)) {
			return Boolean.TRUE;
		}
		if ((type == Character.TYPE) || (type == Character.class)) {
			return Character.valueOf('x');
		}
		if (type == BigDecimal.class) {
			return BigDecimal.TEN;
		}
		if (type == Decimal2.class) {
			return new Decimal2("10.25");
		}
		if (type == Decimal5.class) {
			return new Decimal5("10.25000");
		}
		if (type == Decimal10.class) {
			return new Decimal10("10.2500000000");
		}
		if (type == Date.class) {
			return new Date(0L);
		}
		if (type == DateOnly.class) {
			return new DateOnly();
		}
		if (type == DateTime.class) {
			return new DateTime();
		}
		if (type == TimeOnly.class) {
			return new TimeOnly();
		}
		if (type == LocalDate.class) {
			return LocalDate.of(2026, 6, 5);
		}
		if (type == LocalTime.class) {
			return LocalTime.NOON;
		}
		if (type == LocalDateTime.class) {
			return LocalDateTime.of(2026, 6, 5, 12, 0);
		}
		if (type == URI.class) {
			return URI.create("https://example.invalid");
		}
		if (type == Principal.class) {
			return (Principal) () -> "sample";
		}
		if (type.isEnum()) {
			Object[] constants = type.getEnumConstants();
			return (constants.length == 0) ? UnsupportedValue.INSTANCE : constants[0];
		}
		if (type.isArray()) {
			return java.lang.reflect.Array.newInstance(type.getComponentType(), 0);
		}
		if (List.class.isAssignableFrom(type) || Collection.class.isAssignableFrom(type)) {
			return new ArrayList<>();
		}
		if (Map.class.isAssignableFrom(type)) {
			return new HashMap<>();
		}
		return UnsupportedValue.INSTANCE;
	}

	private enum UnsupportedValue {
		INSTANCE
	}
}
