package org.skyve.impl.bind;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.DynamicTest.dynamicTest;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.management.ManagementFactory;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.stream.Stream;

import com.sun.management.ThreadMXBean;

import org.junit.jupiter.api.DynamicTest;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestFactory;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.DynamicBean;
import org.skyve.domain.types.Decimal10;
import org.skyve.domain.types.Decimal2;
import org.skyve.domain.types.Decimal5;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.model.document.field.Enumeration;
import org.skyve.impl.metadata.model.document.field.Enumeration.EnumeratedValue;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.user.User;

import jakarta.el.ELClass;
import jakarta.el.ELProcessor;

	@SuppressWarnings({ "static-method", "null" })
	class ELExpressionEvaluatorTest {

	// ---- validateWithoutPrefixOrSuffix ----

	@Test
	void validateReturnsNullWhenNotTypesafe() {
		ELExpressionEvaluator evaluator = new ELExpressionEvaluator(false);
		String result = evaluator.validateWithoutPrefixOrSuffix("bean.name", null, null, null, null);
		assertNull(result);
	}

	@Test
	void validateReturnsNullForAnyExpressionWhenNotTypesafe() {
		ELExpressionEvaluator evaluator = new ELExpressionEvaluator(false);
		assertNull(evaluator.validateWithoutPrefixOrSuffix(null, String.class, null, null, null));
		assertNull(evaluator.validateWithoutPrefixOrSuffix("", null, null, null, null));
		assertNull(evaluator.validateWithoutPrefixOrSuffix("malformed!!!", null, null, null, null));
	}

	// ---- completeWithoutPrefixOrSuffix — no-delimiter paths (no Customer needed) ----

	@Test
	void completeReturnsAllCompletesForNullFragment() {
		ELExpressionEvaluator evaluator = new ELExpressionEvaluator(false);
		List<String> result = evaluator.completeWithoutPrefixOrSuffix(null, null, null, null);
		// COMMENCING_COMPLETES has many entries; all should be returned for empty match
		assertTrue(result.size() > 10, "Should return all commencing completes for null fragment");
		assertTrue(result.contains("bean"), "Should contain 'bean'");
		assertTrue(result.contains("user"), "Should contain 'user'");
		assertTrue(result.contains("empty"), "Should contain 'empty'");
	}

	@Test
	void completeReturnsAllCompletesForEmptyFragment() {
		ELExpressionEvaluator evaluator = new ELExpressionEvaluator(false);
		List<String> result = evaluator.completeWithoutPrefixOrSuffix("", null, null, null);
		assertTrue(result.size() > 10, "Should return all commencing completes for empty fragment");
		assertTrue(result.contains("bean"), "Should contain 'bean'");
	}

	@Test
	void completeReturnsMatchingCompletesForFragmentWithNoDelimiter() {
		ELExpressionEvaluator evaluator = new ELExpressionEvaluator(false);
		List<String> result = evaluator.completeWithoutPrefixOrSuffix("bean", null, null, null);
		assertTrue(result.contains("bean"), "'bean' should be among completions for 'bean'");
		// 'user' should NOT appear since it doesn't start with 'bean'
		assertTrue(result.stream().allMatch(s -> s.startsWith("bean")),
				"All completions should start with 'bean'");
	}

	@Test
	void completeReturnsEmptyForClosedOrUnrecognisedFragments() {
		ELExpressionEvaluator evaluator = new ELExpressionEvaluator(false);
		for (String fragment : List.of("]", ")", "x.y")) {
			List<String> result = evaluator.completeWithoutPrefixOrSuffix(fragment, null, null, null);
			assertTrue(result.isEmpty(), "Fragment should return no completions: " + fragment);
		}
	}

	@Test
	void completeReturnsCompletesWithPrefixForUnrecognisedCompoundFragmentWithEmptyTail() {
		ELExpressionEvaluator evaluator = new ELExpressionEvaluator(false);
		// "x." → delimiter = dot, no commencing token → newExpression("x.") → lastChar='.' (not ] or )) → match="" → adds all with prefix "x."
		List<String> result = evaluator.completeWithoutPrefixOrSuffix("x.", null, null, null);
		assertTrue(result.size() > 10, "Should return all completes prefixed with 'x.'");
		assertTrue(result.contains("x.bean"), "Should contain 'x.bean'");
	}

	// ---- prefixBindingWithoutPrefixOrSuffix ----

	@TestFactory
	Stream<DynamicTest> prefixBindingInsertsBindingAfterBeanReferences() {
		ELExpressionEvaluator evaluator = new ELExpressionEvaluator(false);
		// expression, binding, expected
		String[][] cases = {
			{"bean.x + bean.y", "field", "bean.field.x + bean.field.y"},
			{"bean", "myField", "bean.myField"},
			{"x + y", "field", "x + y"},
			{"bean.name", "contact", "bean.contact.name"},
			{"user.name", "field", "user.name"}
		};
		return Stream.of(cases).map(c -> dynamicTest(c[0] + " with " + c[1], () -> {
			StringBuilder sb = new StringBuilder(c[0]);
			evaluator.prefixBindingWithoutPrefixOrSuffix(sb, c[1]);
			assertEquals(c[2], sb.toString());
		}));
	}

	// ---- constructor variants ----

	private static ELProcessor processorWithFreshFunctions() throws NoSuchMethodException {
		ELProcessor processor = new ELProcessor();
		processor.defineBean("user", CORE.getUser());
		processor.defineBean("stash", CORE.getStash());
		for (Method method : ELFunctions.class.getDeclaredMethods()) {
			if (Modifier.isPublic(method.getModifiers()) && Modifier.isStatic(method.getModifiers())) {
				processor.defineFunction("", "", ELFunctions.class.getMethod(method.getName(), method.getParameterTypes()));
			}
		}
		processor.getELManager().importClass(Decimal2.class.getCanonicalName());
		processor.getELManager().importClass(Decimal5.class.getCanonicalName());
		processor.getELManager().importClass(Decimal10.class.getCanonicalName());
		processor.getELManager().addELResolver(new BindingELResolver());
		return processor;
	}

	@Test
	void repeatedEvaluationAvoidsProcessorSetupAllocations() throws Exception {
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		when(persistence.getStash()).thenReturn(new TreeMap<>());
		ThreadLocalPersistenceTestUtil.setThreadLocalPersistence(persistence);
		try {
			ELExpressionEvaluator evaluator = new ELExpressionEvaluator(false);
			ThreadMXBean threads = (ThreadMXBean) ManagementFactory.getThreadMXBean();
			long threadId = Thread.currentThread().getId();
			for (int i = 0; i < 100; i++) {
				assertEquals(3L, ((Number) evaluator.evaluateWithoutPrefixOrSuffix("1 + 2", null)).longValue());
				processorWithFreshFunctions().eval("1 + 2");
			}
			long before = threads.getThreadAllocatedBytes(threadId);
			for (int i = 0; i < 100; i++) {
				evaluator.evaluateWithoutPrefixOrSuffix("1 + 2", null);
			}
			long evaluationBytes = threads.getThreadAllocatedBytes(threadId) - before;
			before = threads.getThreadAllocatedBytes(threadId);
			for (int i = 0; i < 100; i++) {
				processorWithFreshFunctions().eval("1 + 2");
			}
			long processorBytes = threads.getThreadAllocatedBytes(threadId) - before;
			assertTrue(evaluationBytes < processorBytes * 4 / 5,
					"Evaluation allocated " + evaluationBytes + " bytes versus " + processorBytes + " bytes with processor setup");
		}
		finally {
			ThreadLocalPersistenceTestUtil.clearThreadLocalPersistence();
		}
	}

	@Test
	void validationProcessorAvoidsRepeatedFunctionSetupAllocations() throws Exception {
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		when(persistence.getStash()).thenReturn(new TreeMap<>());
		ThreadLocalPersistenceTestUtil.setThreadLocalPersistence(persistence);
		try {
			ThreadMXBean threads = (ThreadMXBean) ManagementFactory.getThreadMXBean();
			long threadId = Thread.currentThread().getId();
			for (int i = 0; i < 100; i++) {
				ELExpressionEvaluator.newSkyveValidationProcessor(null, null).eval("newDecimal2(1.5)");
				processorWithFreshFunctions().eval("newDecimal2(1.5)");
			}
			long before = threads.getThreadAllocatedBytes(threadId);
			for (int i = 0; i < 100; i++) {
				ELExpressionEvaluator.newSkyveValidationProcessor(null, null).eval("newDecimal2(1.5)");
			}
			long validationBytes = threads.getThreadAllocatedBytes(threadId) - before;
			before = threads.getThreadAllocatedBytes(threadId);
			for (int i = 0; i < 100; i++) {
				processorWithFreshFunctions().eval("newDecimal2(1.5)");
			}
			long processorBytes = threads.getThreadAllocatedBytes(threadId) - before;
			assertTrue(validationBytes < processorBytes * 2 / 5,
					"Validation allocated " + validationBytes + " bytes versus " + processorBytes + " bytes with processor setup");
		}
		finally {
			ThreadLocalPersistenceTestUtil.clearThreadLocalPersistence();
		}
	}

	@Test
	void evaluationProcessorAvoidsRepeatedFunctionSetupAllocations() throws Exception {
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		when(persistence.getStash()).thenReturn(new TreeMap<>());
		ThreadLocalPersistenceTestUtil.setThreadLocalPersistence(persistence);
		try {
			ThreadMXBean threads = (ThreadMXBean) ManagementFactory.getThreadMXBean();
			long threadId = Thread.currentThread().getId();
			for (int i = 0; i < 100; i++) {
				ELExpressionEvaluator.newSkyveEvaluationProcessor(null).eval("newDecimal2(1.5)");
				processorWithFreshFunctions().eval("newDecimal2(1.5)");
			}
			long before = threads.getThreadAllocatedBytes(threadId);
			for (int i = 0; i < 100; i++) {
				ELExpressionEvaluator.newSkyveEvaluationProcessor(null).eval("newDecimal2(1.5)");
			}
			long evaluationBytes = threads.getThreadAllocatedBytes(threadId) - before;
			before = threads.getThreadAllocatedBytes(threadId);
			for (int i = 0; i < 100; i++) {
				processorWithFreshFunctions().eval("newDecimal2(1.5)");
			}
			long processorBytes = threads.getThreadAllocatedBytes(threadId) - before;
			assertTrue(evaluationBytes < processorBytes * 2 / 3,
					"Evaluation processor allocated " + evaluationBytes + " bytes versus " + processorBytes + " bytes with function setup");
		}
		finally {
			ThreadLocalPersistenceTestUtil.clearThreadLocalPersistence();
		}
	}

	@Test
	void evaluationUsesOnlyCurrentBeanUserAndStash() throws Exception {
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		Bean firstBean = mock(Bean.class);
		Bean secondBean = mock(Bean.class);
		User firstUser = mock(User.class);
		User secondUser = mock(User.class);
		SortedMap<String, Object> firstStash = new TreeMap<>();
		SortedMap<String, Object> secondStash = new TreeMap<>();
		firstStash.put("value", "first");
		secondStash.put("value", "second");
		when(firstBean.getBizId()).thenReturn("firstBean");
		when(secondBean.getBizId()).thenReturn("secondBean");
		when(firstUser.getId()).thenReturn("firstUser");
		when(secondUser.getId()).thenReturn("secondUser");
		when(persistence.getUser()).thenReturn(firstUser, secondUser);
		when(persistence.getStash()).thenReturn(firstStash).thenReturn(secondStash);
		ThreadLocalPersistenceTestUtil.setThreadLocalPersistence(persistence);
		try {
			ELExpressionEvaluator evaluator = new ELExpressionEvaluator(false);
			assertEquals("firstBean:firstUser:first", evaluator.evaluateWithoutPrefixOrSuffix(
					"bean.bizId.concat(':').concat(user.id).concat(':').concat(stash['value'])", firstBean));
			assertEquals("secondBean:secondUser:second", evaluator.evaluateWithoutPrefixOrSuffix(
					"bean.bizId.concat(':').concat(user.id).concat(':').concat(stash['value'])", secondBean));
		}
		finally {
			ThreadLocalPersistenceTestUtil.clearThreadLocalPersistence();
		}
	}

	@Test
	void concurrentEvaluationsKeepRequestValuesSeparate() throws Exception {
		ELExpressionEvaluator evaluator = new ELExpressionEvaluator(false);
		ExecutorService executor = Executors.newFixedThreadPool(8);
		CountDownLatch start = new CountDownLatch(1);
		List<Future<?>> results = new ArrayList<>();
		try {
			for (int i = 0; i < 8; i++) {
				String id = Integer.toString(i);
				results.add(executor.submit(() -> {
					AbstractPersistence persistence = mock(AbstractPersistence.class);
					Bean bean = mock(Bean.class);
					User user = mock(User.class);
					SortedMap<String, Object> stash = new TreeMap<>();
					stash.put("value", id);
					when(bean.getBizId()).thenReturn(id);
					when(user.getId()).thenReturn(id);
					when(persistence.getUser()).thenReturn(user);
					when(persistence.getStash()).thenReturn(stash);
					ThreadLocalPersistenceTestUtil.setThreadLocalPersistence(persistence);
					try {
						start.await();
						for (int call = 0; call < 100; call++) {
							assertEquals(id + ':' + id + ':' + id, evaluator.evaluateWithoutPrefixOrSuffix(
									"bean.bizId.concat(':').concat(user.id).concat(':').concat(stash['value'])", bean));
						}
					}
					finally {
						ThreadLocalPersistenceTestUtil.clearThreadLocalPersistence();
					}
					return null;
				}));
			}
			start.countDown();
			for (Future<?> result : results) {
				result.get();
			}
		}
		finally {
			executor.shutdownNow();
		}
	}

	@Test
	void evaluationRetainsFunctionsAndClassImports() throws Exception {
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		when(persistence.getStash()).thenReturn(new TreeMap<>());
		ThreadLocalPersistenceTestUtil.setThreadLocalPersistence(persistence);
		try {
			ELExpressionEvaluator evaluator = new ELExpressionEvaluator(false);
			assertEquals(new Decimal2(1.5), evaluator.evaluateWithoutPrefixOrSuffix("newDecimal2(1.5)", null));
			assertEquals(Decimal2.ONE, evaluator.evaluateWithoutPrefixOrSuffix("Decimal2.ONE", null));
			assertEquals(Decimal2.ONE, ELExpressionEvaluator.newSkyveEvaluationProcessor(null).eval("Decimal2.ONE"));
			assertEquals(Decimal2.ONE, ELExpressionEvaluator.newSkyveValidationProcessor(null, null).eval("Decimal2.ONE"));
		}
		finally {
			ThreadLocalPersistenceTestUtil.clearThreadLocalPersistence();
		}
	}

	public static class EnumBean extends DynamicBean {
		private static final long serialVersionUID = 1L;

		public enum Choice { RED, BLUE }

		public EnumBean() {
			super("test", "EnumBean", new HashMap<>());
		}
	}

	public static final class ExtendedEnumBean extends EnumBean {
		private static final long serialVersionUID = 1L;
	}

	public static final class HidingEnumBean extends EnumBean {
		private static final long serialVersionUID = 1L;

		public enum Choice { GREEN }

		@SuppressWarnings("unused")
		private ImportedChoice.Choice importedChoice;
	}

	public static final class ImportedChoice {
		public enum Choice { ORANGE }
	}

	public static final class ImportedEnumBean extends EnumBean {
		private static final long serialVersionUID = 1L;

		@SuppressWarnings("unused")
		private ImportedChoice.Choice importedChoice;
	}

	@Test
	void evaluatesNestedEnumBySimpleTypeName() throws Exception {
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		when(persistence.getStash()).thenReturn(new TreeMap<>());
		ThreadLocalPersistenceTestUtil.setThreadLocalPersistence(persistence);
		try {
			EnumBean bean = new EnumBean();
			assertEquals(EnumBean.Choice.RED,
					new ELExpressionEvaluator(false).evaluateWithoutPrefixOrSuffix("Choice.RED", bean));
			assertEquals(EnumBean.Choice.BLUE,
					ELExpressionEvaluator.newSkyveEvaluationProcessor(bean).eval("Choice.BLUE"));
			assertEquals(EnumBean.Choice.RED,
					new ELExpressionEvaluator(false).evaluateWithoutPrefixOrSuffix("Choice.RED", new ExtendedEnumBean()));
			assertEquals(HidingEnumBean.Choice.GREEN,
					new ELExpressionEvaluator(false).evaluateWithoutPrefixOrSuffix("Choice.GREEN", new HidingEnumBean()));
			assertEquals(EnumBean.Choice.RED,
					new ELExpressionEvaluator(false).evaluateWithoutPrefixOrSuffix("Choice.RED", new ImportedEnumBean()));
		}
		finally {
			ThreadLocalPersistenceTestUtil.clearThreadLocalPersistence();
		}
	}

	@Test
	void validatesNestedEnumFromMetadataWithoutGeneratedClass() {
		Customer customer = mock(Customer.class);
		DocumentImpl document = mock(DocumentImpl.class);
		Enumeration enumeration = mock(Enumeration.class);
		EnumeratedValue red = new EnumeratedValue();
		red.setName("RED");
		red.setCode("red");
		when(enumeration.toJavaIdentifier()).thenReturn("Choice");
		when(enumeration.getValues()).thenReturn(List.of(red));
		when(enumeration.getImplementingType()).thenThrow(new MetaDataException("Not generated", new ClassNotFoundException("Choice")));
		when(document.getAttribute("choice")).thenReturn(enumeration);
		doReturn(List.of(enumeration)).when(document).getAllAttributes(customer);
		ELExpressionEvaluator evaluator = new ELExpressionEvaluator(true);
		assertEquals("red", ELExpressionEvaluator.newSkyveValidationProcessor(customer, document).eval("Choice.RED"));
		assertNull(evaluator.validateWithoutPrefixOrSuffix("Choice.RED", null, customer, null, document));
		assertNull(evaluator.validateWithoutPrefixOrSuffix("bean.choice == Choice.RED", Boolean.class, customer, null, document));
		assertNotNull(evaluator.validateWithoutPrefixOrSuffix("Choice.BLUE", null, customer, null, document));
	}

	@Test
	void validatesNestedEnumUsingClassWhenAvailable() {
		Customer customer = mock(Customer.class);
		DocumentImpl document = mock(DocumentImpl.class);
		Enumeration enumeration = mock(Enumeration.class);
		when(enumeration.toJavaIdentifier()).thenReturn("Choice");
		doReturn(EnumBean.Choice.class).when(enumeration).getImplementingType();
		doReturn(List.of(enumeration)).when(document).getAllAttributes(customer);
		ELExpressionEvaluator evaluator = new ELExpressionEvaluator(true);
		assertNull(evaluator.validateWithoutPrefixOrSuffix("Choice.RED", EnumBean.Choice.class, customer, null, document));
		assertNotNull(evaluator.validateWithoutPrefixOrSuffix("Choice.PURPLE", EnumBean.Choice.class, customer, null, document));
	}

	@Test
	void validatesEnumAttributeComparisonUsingClassWhenAvailable() {
		Customer customer = mock(Customer.class);
		DocumentImpl document = mock(DocumentImpl.class);
		Enumeration enumeration = mock(Enumeration.class);
		when(enumeration.toJavaIdentifier()).thenReturn("Choice");
		doReturn(EnumBean.Choice.class).when(enumeration).getImplementingType();
		when(document.getAttribute("choice")).thenReturn(enumeration);
		doReturn(List.of(enumeration)).when(document).getAllAttributes(customer);
		ELExpressionEvaluator evaluator = new ELExpressionEvaluator(true);
		assertNull(evaluator.validateWithoutPrefixOrSuffix("bean.choice == Choice.RED", Boolean.class, customer, null, document));
		assertNull(evaluator.validateWithoutPrefixOrSuffix("bean.choice", EnumBean.Choice.class, customer, null, document));
		assertNotNull(evaluator.validateWithoutPrefixOrSuffix("bean.choice == Choice.PURPLE", Boolean.class, customer, null, document));
	}

	@Test
	void validationProcessorRequiresCustomerWithDocument() {
		DocumentImpl document = mock(DocumentImpl.class);
		assertThrows(IllegalArgumentException.class, () -> ELExpressionEvaluator.newSkyveValidationProcessor(null, document));
		assertNotNull(new ELExpressionEvaluator(true).validateWithoutPrefixOrSuffix("bean", null, null, null, document));
		assertDoesNotThrow(() -> ELExpressionEvaluator.newSkyveValidationProcessor(null, null));
	}

	public static final class StaticEnumHolder {
		public enum Level { HIGH }
	}

	public static final class StaticEnumFieldBean extends EnumBean {
		private static final long serialVersionUID = 1L;

		@SuppressWarnings("unused")
		private static StaticEnumHolder.Level defaultLevel = StaticEnumHolder.Level.HIGH;
	}

	@Test
	void enumBeansAreCachedImmutablePerClass() {
		Map<String, ELClass> first = ELExpressionEvaluator.ENUM_BEANS.get(ImportedEnumBean.class);
		assertSame(first, ELExpressionEvaluator.ENUM_BEANS.get(ImportedEnumBean.class));
		assertEquals(EnumBean.Choice.class, first.get("Choice").getKlass());
		ELClass other = new ELClass(EnumBean.Choice.class);
		assertThrows(UnsupportedOperationException.class, () -> first.put("Other", other));
	}

	@Test
	void enumBeansIgnoreStaticFieldTypes() throws Exception {
		assertFalse(ELExpressionEvaluator.enumBeans(StaticEnumFieldBean.class).containsKey("Level"));
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		when(persistence.getStash()).thenReturn(new TreeMap<>());
		ThreadLocalPersistenceTestUtil.setThreadLocalPersistence(persistence);
		try {
			ELExpressionEvaluator evaluator = new ELExpressionEvaluator(false);
			StaticEnumFieldBean bean = new StaticEnumFieldBean();
			assertEquals(EnumBean.Choice.RED, evaluator.evaluateWithoutPrefixOrSuffix("Choice.RED", bean));
			assertThrows(Exception.class, () -> evaluator.evaluateWithoutPrefixOrSuffix("Level.HIGH", bean));
		}
		finally {
			ThreadLocalPersistenceTestUtil.clearThreadLocalPersistence();
		}
	}

	@Test
	void localFunctionOverrideDoesNotAffectSharedFunctions() throws Exception {
		ELProcessor first = ELExpressionEvaluator.newSkyveValidationProcessor(null, null);
		first.defineFunction("", "newDecimal2", ELFunctions.class.getMethod("newDecimal5", Double.TYPE));
		assertEquals(new Decimal5(1.5), first.eval("newDecimal2(1.5)"));
		assertEquals(new Decimal2(1.5), ELExpressionEvaluator.newSkyveValidationProcessor(null, null).eval("newDecimal2(1.5)"));
	}

	@Test
	void validationBindsOnlyStaticEnumerationsWithLocalPrecedence() {
		Customer customer = mock(Customer.class);
		DocumentImpl document = mock(DocumentImpl.class);
		org.skyve.metadata.model.Attribute text = mock(org.skyve.metadata.model.Attribute.class);
		Enumeration dynamic = mock(Enumeration.class);
		when(Boolean.valueOf(dynamic.isDynamic())).thenReturn(Boolean.TRUE);
		when(dynamic.toJavaIdentifier()).thenReturn("Dynamic");
		Enumeration importedByClass = mock(Enumeration.class);
		when(importedByClass.getImplementingEnumClassName()).thenReturn(ImportedChoice.Choice.class.getName());
		when(importedByClass.toJavaIdentifier()).thenReturn("Choice");
		doReturn(ImportedChoice.Choice.class).when(importedByClass).getImplementingType();
		Enumeration local = mock(Enumeration.class);
		when(local.toJavaIdentifier()).thenReturn("Choice");
		doReturn(EnumBean.Choice.class).when(local).getImplementingType();
		doReturn(List.of(text, dynamic, importedByClass, local)).when(document).getAllAttributes(customer);
		ELProcessor processor = ELExpressionEvaluator.newSkyveValidationProcessor(customer, document);
		assertEquals(EnumBean.Choice.RED, processor.eval("Choice.RED"));
		assertThrows(Exception.class, () -> processor.eval("Dynamic.VALUE"));
	}

	@Test
	void validationRethrowsNonClassLoadingEnumFailures() {
		Customer customer = mock(Customer.class);
		DocumentImpl document = mock(DocumentImpl.class);
		Enumeration enumeration = mock(Enumeration.class);
		when(enumeration.toJavaIdentifier()).thenReturn("Choice");
		when(enumeration.getImplementingType()).thenThrow(new MetaDataException("Broken metadata"));
		doReturn(List.of(enumeration)).when(document).getAllAttributes(customer);
		assertThrows(MetaDataException.class, () -> ELExpressionEvaluator.newSkyveValidationProcessor(customer, document));
	}

	public static final class NonBindableEnumBean extends EnumBean {
		private static final long serialVersionUID = 1L;

		private enum Hidden { SECRET }

		public interface NotAnEnum {
			// a nested type that is not an enum
		}

		@SuppressWarnings("unused")
		private String text;
		@SuppressWarnings("unused")
		private Hidden hidden;
	}

	@Test
	void enumBeansSkipNonPublicAndNonEnumTypes() {
		Map<String, ELClass> beans = ELExpressionEvaluator.enumBeans(NonBindableEnumBean.class);
		assertFalse(beans.containsKey("Hidden"));
		assertFalse(beans.containsKey("NotAnEnum"));
		assertFalse(beans.containsKey("String"));
		assertTrue(beans.containsKey("Choice"));
	}

	@Test
	void processorCanRegisterSeveralLocalFunctions() throws Exception {
		ELProcessor processor = ELExpressionEvaluator.newSkyveValidationProcessor(null, null);
		processor.defineFunction("", "first", ELFunctions.class.getMethod("newDecimal2", Double.TYPE));
		processor.defineFunction("", "second", ELFunctions.class.getMethod("newDecimal5", Double.TYPE));
		assertEquals(new Decimal2(1.5), processor.eval("first(1.5)"));
		assertEquals(new Decimal5(1.5), processor.eval("second(1.5)"));
	}

	@Test
	void validationPrefersLocalEnumOverImportedNameCollision() {
		Customer customer = mock(Customer.class);
		DocumentImpl document = mock(DocumentImpl.class);
		Enumeration imported = mock(Enumeration.class);
		when(imported.getAttributeRef()).thenReturn("choice");
		when(imported.toJavaIdentifier()).thenReturn("Choice");
		doReturn(ImportedChoice.Choice.class).when(imported).getImplementingType();
		Enumeration local = mock(Enumeration.class);
		when(local.toJavaIdentifier()).thenReturn("Choice");
		doReturn(HidingEnumBean.Choice.class).when(local).getImplementingType();
		doReturn(List.of(imported, local)).when(document).getAllAttributes(customer);
		assertEquals(HidingEnumBean.Choice.GREEN,
				ELExpressionEvaluator.newSkyveValidationProcessor(customer, document).eval("Choice.GREEN"));
	}

	@Test
	void concurrentValidationProcessorsKeepDocumentEnumsSeparate() throws Exception {
		Customer firstCustomer = mock(Customer.class);
		DocumentImpl firstDocument = mock(DocumentImpl.class);
		Enumeration firstEnum = mock(Enumeration.class);
		when(firstEnum.toJavaIdentifier()).thenReturn("Choice");
		doReturn(EnumBean.Choice.class).when(firstEnum).getImplementingType();
		doReturn(List.of(firstEnum)).when(firstDocument).getAllAttributes(firstCustomer);
		Customer secondCustomer = mock(Customer.class);
		DocumentImpl secondDocument = mock(DocumentImpl.class);
		Enumeration secondEnum = mock(Enumeration.class);
		when(secondEnum.toJavaIdentifier()).thenReturn("Choice");
		doReturn(HidingEnumBean.Choice.class).when(secondEnum).getImplementingType();
		doReturn(List.of(secondEnum)).when(secondDocument).getAllAttributes(secondCustomer);
		ExecutorService executor = Executors.newFixedThreadPool(2);
		CountDownLatch start = new CountDownLatch(1);
		try {
			Future<?> first = executor.submit(() -> {
				start.await();
				for (int i = 0; i < 100; i++) {
					assertEquals(firstDocument, ELExpressionEvaluator.newSkyveValidationProcessor(firstCustomer, firstDocument).eval("bean"));
					assertEquals(EnumBean.Choice.RED,
							ELExpressionEvaluator.newSkyveValidationProcessor(firstCustomer, firstDocument).eval("Choice.RED"));
				}
				return null;
			});
			Future<?> second = executor.submit(() -> {
				start.await();
				for (int i = 0; i < 100; i++) {
					assertEquals(secondDocument, ELExpressionEvaluator.newSkyveValidationProcessor(secondCustomer, secondDocument).eval("bean"));
					assertEquals(HidingEnumBean.Choice.GREEN,
							ELExpressionEvaluator.newSkyveValidationProcessor(secondCustomer, secondDocument).eval("Choice.GREEN"));
				}
				return null;
			});
			start.countDown();
			first.get();
			second.get();
		}
		finally {
			executor.shutdownNow();
		}
	}

	@Test
	void validationContextsDoNotShareContextObjects() {
		ELProcessor first = ELExpressionEvaluator.newSkyveValidationProcessor(null, null);
		first.getELManager().getELContext().putContext(String.class, "first");
		ELProcessor second = ELExpressionEvaluator.newSkyveValidationProcessor(null, null);
		assertNull(second.getELManager().getELContext().getContext(String.class));
	}

	@Test
	void evaluationContextsDoNotShareContextObjects() throws Exception {
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		when(persistence.getStash()).thenReturn(new TreeMap<>());
		ThreadLocalPersistenceTestUtil.setThreadLocalPersistence(persistence);
		try {
			ELProcessor first = ELExpressionEvaluator.newSkyveEvaluationProcessor(null);
			first.getELManager().getELContext().putContext(String.class, "first");
			ELProcessor second = ELExpressionEvaluator.newSkyveEvaluationProcessor(null);
			assertNull(second.getELManager().getELContext().getContext(String.class));
		}
		finally {
			ThreadLocalPersistenceTestUtil.clearThreadLocalPersistence();
		}
	}

	@Test
	void processorFunctionRegistrationRemainsLocal() throws Exception {
		ELProcessor first = ELExpressionEvaluator.newSkyveValidationProcessor(null, null);
		first.defineFunction("", "localDate", ELFunctions.class.getMethod("newDateOnly"));
		assertNotNull(first.eval("localDate()"));
		ELProcessor second = ELExpressionEvaluator.newSkyveValidationProcessor(null, null);
		assertThrows(Exception.class, () -> second.eval("localDate()"));
	}

	@Test
	void constructorWithTypesafeTrue() {
		assertDoesNotThrow(() -> new ELExpressionEvaluator(true));
	}

	@Test
	void constructorWithTypesafeFalse() {
		assertDoesNotThrow(() -> new ELExpressionEvaluator(false));
	}

	// ---- validateWithoutPrefixOrSuffix — typesafe=true paths ----

	@Test
	void validateWithTypesafeAndReturnTypeIncompatibilityReturnsErrorMessage() {
		ELExpressionEvaluator evaluator = new ELExpressionEvaluator(true);
		Customer customer = mock(Customer.class);
		// "user" evaluates to UserImpl.class (Class<?>) — not assignable to String
		String result = evaluator.validateWithoutPrefixOrSuffix("user", String.class, customer, null, null);
		assertNotNull(result, "Should return error message for incompatible return type");
		assertTrue(result.contains("incompatible"), "Error should mention incompatibility");
	}

	@TestFactory
	Stream<DynamicTest> validateWithTypesafeChecksReturnTypeAndSyntax() {
		ELExpressionEvaluator evaluator = new ELExpressionEvaluator(true);
		Customer customer = mock(Customer.class);
		// "user" evaluates to UserImpl.class; a null return type is unconstrained; malformed EL is reported
		Object[][] cases = {
			{"user", org.skyve.impl.metadata.user.UserImpl.class, Boolean.FALSE},
			{"user", null, Boolean.FALSE},
			{"bad#expression!!", String.class, Boolean.TRUE}
		};
		return Stream.of(cases).map(c -> dynamicTest(c[0] + " returning " + c[1], () -> {
			String result = evaluator.validateWithoutPrefixOrSuffix((String) c[0], (Class<?>) c[1], customer, null, null);
			assertEquals(c[2], Boolean.valueOf(result != null), String.valueOf(result));
		}));
	}

	// ---- completeWithoutPrefixOrSuffix — continuation paths (EL evaluation) ----

	@Test
	void completeWithUserDotFragmentReturnsUserImplProperties() {
		ELExpressionEvaluator evaluator = new ELExpressionEvaluator(false);
		// "user." → evaluates "user" to UserImpl.class → Class<?> path → property descriptors
		List<String> result = evaluator.completeWithoutPrefixOrSuffix("user.", null, null, null);
		// Should contain at least some UserImpl properties starting with "user."
		assertFalse(result.isEmpty(), "Should return completions for 'user.' fragment");
		assertTrue(result.stream().allMatch(s -> s.startsWith("user.")),
				"All completions should start with 'user.'");
	}

	@Test
	void completeWithNonMapOpenBracketOffersNoMapNotation() {
		ELExpressionEvaluator evaluator = new ELExpressionEvaluator(false);
		// user.locale evaluates to Locale.class, which is not a Map
		assertFalse(evaluator.completeWithoutPrefixOrSuffix("user.locale[", null, null, null).contains("user.locale['"));
		// user.name evaluates to a String mock rather than a Class
		assertFalse(evaluator.completeWithoutPrefixOrSuffix("user.name[", null, null, null).contains("user.name['"));
	}

	@Test
	void completeAfterClosingSquareBraceIncludesBrace() {
		ELExpressionEvaluator evaluator = new ELExpressionEvaluator(false);
		assertNotNull(evaluator.completeWithoutPrefixOrSuffix("stash['a'].", null, null, null));
		assertNotNull(evaluator.completeWithoutPrefixOrSuffix("stash['a']", null, null, null));
	}

	@Test
	void completeWithStashOpenBracketReturnsMapNotation() {
		ELExpressionEvaluator evaluator = new ELExpressionEvaluator(false);
		// "stash[" → evaluates "stash" to Map.class → open square brace + Map → adds "stash['"
		List<String> result = evaluator.completeWithoutPrefixOrSuffix("stash[", null, null, null);
		assertTrue(result.contains("stash['"), "Should contain map key notation 'stash[\\''");
	}

	@Test
	void completeWithUserDotPrefixFiltersToMatchingProperties() {
		ELExpressionEvaluator evaluator = new ELExpressionEvaluator(false);
		// "user.name" → evaluates "user" to UserImpl.class, filter by "name" prefix
		List<String> result = evaluator.completeWithoutPrefixOrSuffix("user.name", null, null, null);
		// All completions should start with "user.name"
		for (String completion : result) {
			assertTrue(completion.startsWith("user.name"),
					"Completion '" + completion + "' should start with 'user.name'");
		}
	}

	@Test
	void completeWithUnknownBeanDotFragmentHandlesGracefully() {
		ELExpressionEvaluator evaluator = new ELExpressionEvaluator(false);
		// "bean." → "bean" is not defined (no document) → eval throws → caught, returns empty or newExpression
		List<String> result = evaluator.completeWithoutPrefixOrSuffix("bean.", null, null, null);
		// no NPE — graceful handling whether empty or with new-expression suggestions
		assertNotNull(result);
	}

}
