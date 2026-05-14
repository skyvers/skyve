package org.skyve.domain.types.converters;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;
import org.skyve.domain.types.converters.Format.TextCase;

@SuppressWarnings("static-method")
class FormatTest {

	@Test
	void getMaskReturnsConstructorValue() {
		Format<String> f = new Format<>("###-####", null);
		assertEquals("###-####", f.getMask());
	}

	@Test
	void getTextCaseReturnsConstructorValue() {
		Format<String> f = new Format<>(null, TextCase.upper);
		assertEquals(TextCase.upper, f.getTextCase());
	}

	@Test
	void textCaseNullByDefault() {
		Format<String> f = new Format<>(null, null);
		assertNull(f.getTextCase());
	}

	@Test
	void toDisplayValueNullValueReturnsEmpty() throws Exception {
		Format<String> f = new Format<>(null, null);
		assertEquals("", f.toDisplayValue(null));
	}

	@Test
	void toDisplayValueUpperCaseApplied() throws Exception {
		Format<String> f = new Format<>(null, TextCase.upper);
		assertEquals("HELLO", f.toDisplayValue("hello"));
	}

	@Test
	void toDisplayValueLowerCaseApplied() throws Exception {
		Format<String> f = new Format<>(null, TextCase.lower);
		assertEquals("hello", f.toDisplayValue("HELLO"));
	}

	@Test
	void toDisplayValueCapitalCaseApplied() throws Exception {
		Format<String> f = new Format<>(null, TextCase.capital);
		assertEquals("Hello World", f.toDisplayValue("hello world"));
	}

	@Test
	void toDisplayValueNoCasePreservesOriginal() throws Exception {
		Format<String> f = new Format<>(null, null);
		assertEquals("hElLo", f.toDisplayValue("hElLo"));
	}

	@Test
	void fromDisplayValueNoMaskReturnsValue() throws Exception {
		Format<String> f = new Format<>(null, null);
		assertEquals("hello", f.fromDisplayValue("hello"));
	}

	@Test
	void fromDisplayValueUpperCaseApplied() throws Exception {
		Format<String> f = new Format<>(null, TextCase.upper);
		assertEquals("HELLO", f.fromDisplayValue("hello"));
	}

	@Test
	void toDisplayValueWithMaskAndNullTextCase() throws Exception {
		// mask with null textCase: 'L' is replaced with '?' (letter)
		Format<String> f = new Format<>("LAA", null);
		// 'LAA' becomes '?AA' in MaskFormatter
		String result = f.toDisplayValue("bAb");
		assertEquals("bAb", result);
	}

	@Test
	void toDisplayValueWithMaskAndUpperTextCase() throws Exception {
		// mask with upper textCase: 'L' is replaced with 'U' (upper letter)
		Format<String> f = new Format<>("LAA", TextCase.upper);
		String result = f.toDisplayValue("bab");
		assertEquals("BAB", result);
	}

	@Test
	void toDisplayValueWithMaskAndCapitalTextCase() throws Exception {
		// mask with capital textCase: first 'L' replaced with 'U'
		Format<String> f = new Format<>("LAA", TextCase.capital);
		String result = f.toDisplayValue("bab");
		assertEquals("Bab", result);
	}

	@Test
	void toDisplayValueWithMaskAndLowerTextCase() throws Exception {
		// mask with lower textCase: 'L' stays as 'L' (lower letter in MaskFormatter)
		Format<String> f = new Format<>("LAA", TextCase.lower);
		String result = f.toDisplayValue("BAB");
		assertEquals("bab", result);
	}

	@Test
	void fromDisplayValueWithMaskAndNullTextCase() throws Exception {
		Format<String> f = new Format<>("LAA", null);
		String result = f.fromDisplayValue("bAb");
		assertEquals("bAb", result);
	}
}
