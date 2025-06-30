package org.skyve.impl.bind;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.mockStatic;

import java.util.Collections;
import java.util.List;
import java.util.SortedMap;
import java.util.TreeMap;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;

@Disabled("Until byte buddy can be uplifted to allow mockito-inline lib to work")
@ExtendWith(MockitoExtension.class)
public class StashExpressionEvaluatorTest {

	private StashExpressionEvaluator evaluator;
	@Mock
	private Bean bean;
	private SortedMap<String, Object> stash;
	private MockedStatic<CORE> mockedCORE;

	@BeforeEach
	public void before() {
		evaluator = new StashExpressionEvaluator();
		// bean = new TestBean();
		stash = new TreeMap<>();
		mockedCORE = mockStatic(CORE.class);
		mockedCORE.when(CORE::getStash).thenReturn(stash);
	}

	@AfterEach
	public void tearDown() {
		// Closing the mockStatic after each test
		mockedCORE.close();
	}

	@Test
	public void testEvaluateWithoutPrefixOrSuffix() {
		// setup the test data
		String key = "testKey";
		String value = "testValue";
		stash.put(key, value);

		// call the method under test
		Object result = evaluator.evaluateWithoutPrefixOrSuffix(key, bean);

		// verify the result
		assertThat(result, is(value));
	}

	@Test
	public void testEvaluateWithoutPrefixOrSuffixNullValue() {
		// setup the test data
		String key = "testKey";
		stash.put(key, null);

		// call the method under test
		Object result = evaluator.evaluateWithoutPrefixOrSuffix(key, bean);

		// verify the result
		assertThat(result, is((Object) null));
	}

	@Test
	public void testFormatWithoutPrefixOrSuffix() {
		// setup the test data
		String key = "testKey";
		String value = "testValue";
		stash.put(key, value);

		// call the method under test
		String result = evaluator.formatWithoutPrefixOrSuffix(key, bean);

		// verify the result
		assertThat(result, is(value));
	}

	@Test
	public void testFormatWithoutPrefixOrSuffixNullValue() {
		// setup the test data
		String key = "testKey";
		stash.put(key, null);

		// call the method under test
		String result = evaluator.formatWithoutPrefixOrSuffix(key, bean);

		// verify the result
		assertThat(result, is(""));
	}

	@Test
	public void testValidateWithoutPrefixOrSuffix() {
		// setup the test data
		String expression = "testKey";
		Class<?> returnType = String.class;
		Customer customer = null;
		Module module = null;
		Document document = null;

		// call the method under test
		String result = evaluator.validateWithoutPrefixOrSuffix(expression, returnType, customer, module, document);

		// verify the result
		assertThat(result, is((String) null)); // any key is valid
	}

	@Test
	public void testCompleteWithoutPrefixOrSuffix() {
		// setup the test data
		String fragment = "test";
		Customer customer = null;
		Module module = null;
		Document document = null;

		// call the method under test
		List<String> result = evaluator.completeWithoutPrefixOrSuffix(fragment, customer, module, document);

		// verify the result
		assertThat(result, is(Collections.emptyList())); // any key is valid
	}

	@Test
	public void testPrefixBindingWithoutPrefixOrSuffix() {
		// setup the test data
		StringBuilder expression = new StringBuilder();
		String binding = "testBinding";

		// call the method under test
		evaluator.prefixBindingWithoutPrefixOrSuffix(expression, binding);

		// verify the result
		assertThat(expression.toString(), is("")); // nothing should be added
	}
} 