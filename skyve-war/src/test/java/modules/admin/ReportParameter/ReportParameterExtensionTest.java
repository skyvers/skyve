package modules.admin.ReportParameter;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.ValidationException;

import org.skyve.domain.app.admin.ReportParameter.Type;

class ReportParameterExtensionTest {

	private ReportParameterExtension bean;

	@BeforeEach
	void setUp() {
		bean = new ReportParameterExtension();
		bean.setName("testParam");
	}

	// --- validate() tests ---

	@Test
	void testValidateRequiredWithNoInputAndNoDefaultAddsError() {
		bean.setRequired(Boolean.TRUE);
		// reportInputValue is null by default, defaultValueString is null

		ValidationException e = new ValidationException();
		bean.validate(e, 0);

		assertFalse(e.getMessages().isEmpty());
	}

	@Test
	void testValidateOptionalWithNoInputAndNoDefaultAddsNoError() {
		bean.setRequired(Boolean.FALSE);

		ValidationException e = new ValidationException();
		bean.validate(e, 0);

		assertTrue(e.getMessages().isEmpty());
	}

	@Test
	void testValidateRequiredWithInputValueAddsNoError() {
		bean.setRequired(Boolean.TRUE);
		bean.setReportInputValue("someValue");
		bean.setType(Type.text);

		ValidationException e = new ValidationException();
		bean.validate(e, 0);

		assertTrue(e.getMessages().isEmpty());
	}

	@Test
	void testValidateIntegerTypeWithValidValueAddsNoError() {
		bean.setType(Type.integer);
		bean.setReportInputValue("42");

		ValidationException e = new ValidationException();
		bean.validate(e, 0);

		assertTrue(e.getMessages().isEmpty());
	}

	@Test
	void testValidateIntegerTypeWithInvalidValueAddsError() {
		bean.setType(Type.integer);
		bean.setReportInputValue("not-a-number");

		ValidationException e = new ValidationException();
		bean.validate(e, 0);

		assertFalse(e.getMessages().isEmpty());
		assertEquals(1, e.getMessages().size());
	}

	@Test
	void testValidateLongIntegerTypeWithValidValueAddsNoError() {
		bean.setType(Type.longInteger);
		bean.setReportInputValue("9999999999");

		ValidationException e = new ValidationException();
		bean.validate(e, 0);

		assertTrue(e.getMessages().isEmpty());
	}

	@Test
	void testValidateLongIntegerTypeWithInvalidValueAddsError() {
		bean.setType(Type.longInteger);
		bean.setReportInputValue("not-a-long");

		ValidationException e = new ValidationException();
		bean.validate(e, 0);

		assertFalse(e.getMessages().isEmpty());
		assertEquals(1, e.getMessages().size());
	}

	// --- validateTest() tests ---

	@Test
	void testValidateTestRequiredWithNullTestValueAddsError() {
		bean.setRequired(Boolean.TRUE);
		// all test values are null

		ValidationException e = new ValidationException();
		bean.validateTest(e);

		assertFalse(e.getMessages().isEmpty());
	}

	@Test
	void testValidateTestRequiredWithTextTestValueAddsNoError() {
		bean.setRequired(Boolean.TRUE);
		bean.setTextTestValue("test-value");

		ValidationException e = new ValidationException();
		bean.validateTest(e);

		assertTrue(e.getMessages().isEmpty());
	}

	@Test
	void testValidateTestOptionalWithNullTestValueAddsNoError() {
		bean.setRequired(Boolean.FALSE);

		ValidationException e = new ValidationException();
		bean.validateTest(e);

		assertTrue(e.getMessages().isEmpty());
	}

	// --- getDefaultValueString() tests ---

	@Test
	void testGetDefaultValueStringReturnsTextDefaultValue() {
		bean.setTextDefaultValue("my-default");

		String result = bean.getDefaultValueString();

		assertEquals("my-default", result);
	}

	@Test
	void testGetDefaultValueStringReturnsNullWhenAllValuesNull() {
		// textDefaultValue, dateDefaultValue, numericalDefaultValue all null
		String result = bean.getDefaultValueString();

		assertNull(result);
	}

	// --- getTestValueString() tests ---

	@Test
	void testGetTestValueStringReturnsTextTestValue() {
		bean.setTextTestValue("test-text");

		String result = bean.getTestValueString();

		assertEquals("test-text", result);
	}

	@Test
	void testGetTestValueStringReturnsNullWhenAllValuesNull() {
		String result = bean.getTestValueString();

		assertNull(result);
	}

        // --- getFormattedInputValue() tests ---

        @Test
        void testGetFormattedInputValueReturnsDefaultWhenInputNullAndDefaultSet() {
                bean.setTextDefaultValue("default-val");
                String result = bean.getFormattedInputValue();
                assertEquals("default-val", result);
        }

        @Test
        void testGetFormattedInputValueReturnsTestValueWhenInputAndDefaultNull() {
                bean.setTextTestValue("test-val");
                String result = bean.getFormattedInputValue();
                assertEquals("test-val", result);
        }

        @Test
        void testGetFormattedInputValueReturnsNullWhenAllNull() {
                String result = bean.getFormattedInputValue();
                assertNull(result);
        }
}
