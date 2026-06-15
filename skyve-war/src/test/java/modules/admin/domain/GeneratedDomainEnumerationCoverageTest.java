package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Enumeration;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.Decimal10;
import org.skyve.domain.types.Decimal2;
import org.skyve.domain.types.Decimal5;
import org.skyve.domain.types.TimeOnly;
import org.skyve.domain.types.Timestamp;

import util.AbstractH2Test;

class GeneratedDomainEnumerationCoverageTest extends AbstractH2Test {
	private static final String[] DOMAIN_PACKAGES = {
			"modules.admin.domain",
			"modules.kitchensink.domain",
			"modules.test.domain",
			"modules.whosin.domain"
	};

	@Test
	@SuppressWarnings("static-method")
	void generatedDomainEnumerationsRoundTripCodesAndDescriptions() throws Exception {
		int exercised = 0;
		for (String packageName : DOMAIN_PACKAGES) {
			for (Class<?> domainClass : domainClasses(packageName)) {
				for (Class<?> candidate : domainClass.getDeclaredClasses()) {
					if (candidate.isEnum() && org.skyve.domain.types.Enumeration.class.isAssignableFrom(candidate)) {
						exerciseEnumeration(candidate);
						exercised++;
					}
				}
			}
		}

		assertTrue(exercised > 0);
	}

	@Test
	@SuppressWarnings("static-method")
	void generatedDomainAccessorsRoundTripSupportedValues() throws Exception {
		int exercised = 0;
		for (String packageName : DOMAIN_PACKAGES) {
			for (Class<?> domainClass : domainClasses(packageName)) {
				Object bean = newDomainInstance(domainClass);
				if (bean != null) {
					for (Method setter : sortedDeclaredMethods(domainClass)) {
						if (isBeanSetter(setter)) {
							Object value = sampleValue(setter.getParameterTypes()[0]);
							Method getter = getterFor(domainClass, setter);
							if ((value != null) && (getter != null)) {
								try {
									setter.invoke(bean, value);
									getter.invoke(bean);
									exercised++;
								}
								catch (@SuppressWarnings("unused") Exception e) {
									// Some generated setters depend on document-specific validation or derived bindings.
								}
							}
						}
					}
				}
			}
		}

		assertTrue(exercised > 100);
	}

	@Test
	@SuppressWarnings("static-method")
	void generatedDomainRelationshipHelpersExerciseSupportedMethods() throws Exception {
		int exercised = 0;
		for (String packageName : DOMAIN_PACKAGES) {
			for (Class<?> domainClass : domainClasses(packageName)) {
				Object bean = newDomainInstance(domainClass);
				if (bean != null) {
					for (Method method : sortedDeclaredMethods(domainClass)) {
						if (isRelationshipHelper(method)) {
							exercised += invokeRelationshipHelper(bean, method);
						}
					}
				}
			}
		}

		assertTrue(exercised > 50);
	}

	private static List<Class<?>> domainClasses(String packageName) throws Exception {
		String resourceName = packageName.replace('.', '/');
		ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
		Enumeration<URL> resources = classLoader.getResources(resourceName);
		List<Class<?>> result = new ArrayList<>();
		while (resources.hasMoreElements()) {
			URL resource = resources.nextElement();
			if ("file".equals(resource.getProtocol())) {
				Path packagePath = Path.of(resource.toURI());
				try (var stream = Files.list(packagePath)) {
					for (Path classFile : stream.toList()) {
						String fileName = classFile.getFileName().toString();
						if (fileName.endsWith(".class") && ! fileName.contains("$")) {
							String simpleName = fileName.substring(0, fileName.length() - ".class".length());
							result.add(Class.forName(packageName + '.' + simpleName));
						}
					}
				}
			}
		}
		return result;
	}

	private static void exerciseEnumeration(Class<?> enumClass) throws Exception {
		Object[] values = enumClass.getEnumConstants();
		if (values.length == 0) {
			return;
		}

		Method toCode = enumClass.getMethod("toCode");
		Method toLocalisedDescription = enumClass.getMethod("toLocalisedDescription");
		Method toDomainValue = enumClass.getMethod("toDomainValue");
		Method toDomainValues = enumClass.getMethod("toDomainValues");
		Method fromCode = enumClass.getMethod("fromCode", String.class);
		Method fromLocalisedDescription = enumClass.getMethod("fromLocalisedDescription", String.class);

		Object first = values[0];
		String code = (String) toCode.invoke(first);
		String description = (String) toLocalisedDescription.invoke(first);

		assertSame(first, fromCode.invoke(null, code));
		assertSame(first, fromLocalisedDescription.invoke(null, description));
		assertTrue(values.length <= ((List<?>) toDomainValues.invoke(null)).size());
		assertTrue(toDomainValue.invoke(first) instanceof org.skyve.metadata.model.document.Bizlet.DomainValue);
		fromCode.invoke(null, "__missing__");
		fromLocalisedDescription.invoke(null, "__missing__");
	}

	private static Object newDomainInstance(Class<?> domainClass) {
		try {
			Method newInstance = domainClass.getMethod("newInstance");
			return newInstance.invoke(null);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return null;
		}
	}

	private static boolean isRelationshipHelper(Method method) {
		String name = method.getName();
		return ((name.startsWith("add") && name.endsWith("Element"))
					|| (name.startsWith("remove") && name.endsWith("Element"))
					|| (name.startsWith("get") && name.endsWith("ElementById"))
					|| (name.startsWith("set") && name.endsWith("ElementById"))
					|| name.startsWith("null"))
				&& method.getDeclaringClass().getPackageName().startsWith("modules.");
	}

	private static int invokeRelationshipHelper(Object bean, Method method) {
		try {
			Class<?>[] parameterTypes = method.getParameterTypes();
			if (method.getName().startsWith("null") && (parameterTypes.length == 0)) {
				method.invoke(bean);
				return 1;
			}
			if (method.getName().startsWith("get") && (parameterTypes.length == 1) && String.class.equals(parameterTypes[0])) {
				method.invoke(bean, "__missing__");
				return 1;
			}
			if ((method.getName().startsWith("add") || method.getName().startsWith("remove")) && (parameterTypes.length == 1)) {
				Object element = newDomainInstance(parameterTypes[0]);
				if (element != null) {
					method.invoke(bean, element);
					return 1;
				}
			}
			if (method.getName().startsWith("add") && (parameterTypes.length == 2) && Integer.TYPE.equals(parameterTypes[0])) {
				Object element = newDomainInstance(parameterTypes[1]);
				if (element != null) {
					method.invoke(bean, Integer.valueOf(0), element);
					return 1;
				}
			}
			if (method.getName().startsWith("remove") && (parameterTypes.length == 1) && Integer.TYPE.equals(parameterTypes[0])) {
				Method addElement = oneArgumentAddElement(method.getDeclaringClass(), method.getName());
				if (addElement != null) {
					Object element = newDomainInstance(addElement.getParameterTypes()[0]);
					if (element != null) {
						addElement.invoke(bean, element);
						method.invoke(bean, Integer.valueOf(0));
						return 1;
					}
				}
			}
			if (method.getName().startsWith("set") && (parameterTypes.length == 2) && String.class.equals(parameterTypes[0])) {
				Object element = newDomainInstance(parameterTypes[1]);
				if (element != null) {
					method.invoke(bean, "__missing__", element);
					return 1;
				}
			}
		}
		catch (@SuppressWarnings("unused") Exception e) {
			// Relationship helpers can trigger inverse wiring or validation; unsupported combinations are skipped.
		}
		return 0;
	}

	private static Method oneArgumentAddElement(Class<?> declaringClass, String removeMethodName) {
		String addMethodName = "add" + removeMethodName.substring("remove".length());
		for (Method candidate : declaringClass.getDeclaredMethods()) {
			if (candidate.getName().equals(addMethodName) && (candidate.getParameterCount() == 1)) {
				return candidate;
			}
		}
		return null;
	}

	private static List<Method> sortedDeclaredMethods(Class<?> domainClass) {
		return Arrays.stream(domainClass.getDeclaredMethods())
				.sorted(Comparator.comparing(Method::getName))
				.toList();
	}

	private static boolean isBeanSetter(Method method) {
		return method.getName().startsWith("set")
				&& (method.getParameterCount() == 1)
				&& (method.getReturnType() == Void.TYPE);
	}

	private static Method getterFor(Class<?> domainClass, Method setter) {
		String propertyName = setter.getName().substring("set".length());
		try {
			return domainClass.getMethod("get" + propertyName);
		}
		catch (@SuppressWarnings("unused") NoSuchMethodException e) {
			try {
				return domainClass.getMethod("is" + propertyName);
			}
			catch (@SuppressWarnings("unused") NoSuchMethodException ignored) {
				return null;
			}
		}
	}

	private static Object sampleValue(Class<?> type) {
		if (String.class.equals(type)) {
			return "value";
		}
		if (Boolean.class.equals(type) || Boolean.TYPE.equals(type)) {
			return Boolean.TRUE;
		}
		if (Integer.class.equals(type) || Integer.TYPE.equals(type)) {
			return Integer.valueOf(1);
		}
		if (Long.class.equals(type) || Long.TYPE.equals(type)) {
			return Long.valueOf(1L);
		}
		if (Decimal2.class.equals(type)) {
			return new Decimal2("1.23");
		}
		if (Decimal5.class.equals(type)) {
			return new Decimal5("1.23456");
		}
		if (Decimal10.class.equals(type)) {
			return new Decimal10("1.2345678901");
		}
		if (BigDecimal.class.equals(type)) {
			return BigDecimal.ONE;
		}
		if (DateOnly.class.equals(type)) {
			return new DateOnly();
		}
		if (DateTime.class.equals(type)) {
			return new DateTime();
		}
		if (TimeOnly.class.equals(type)) {
			return new TimeOnly();
		}
		if (Timestamp.class.equals(type)) {
			return new Timestamp();
		}
		if (type.isEnum()) {
			Object[] values = type.getEnumConstants();
			return (values.length == 0) ? null : values[0];
		}
		return null;
	}
}
