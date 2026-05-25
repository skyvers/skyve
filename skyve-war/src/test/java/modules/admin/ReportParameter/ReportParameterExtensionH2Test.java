package modules.admin.ReportParameter;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.app.admin.ReportParameter.Type;
import org.skyve.domain.messages.ValidationException;
import org.skyve.domain.types.DateOnly;

import util.AbstractH2Test;

public class ReportParameterExtensionH2Test extends AbstractH2Test {

	private ReportParameterExtension bean;

	@BeforeEach
	void setUpBean() {
		bean = new ReportParameterExtension();
		bean.setName("testParam");
	}

	@Test
	void getDefaultValueStringWithNumericalDefaultValue() {
		bean.setNumericalDefaultValue(42L);
		assertNotNull(bean.getDefaultValueString());
	}

	@Test
	void getDefaultValueStringWithDateDefaultValue() {
		bean.setDateDefaultValue(new DateOnly());
		assertNotNull(bean.getDefaultValueString());
	}

	@Test
	void getTestValueStringWithNumericalTestValue() {
		bean.setNumericalTestValue(99L);
		assertNotNull(bean.getTestValueString());
	}

	@Test
	void getTestValueStringWithDateTestValue() {
		bean.setDateTestValue(new DateOnly());
		assertNotNull(bean.getTestValueString());
	}

	@Test
	void getFormattedInputValueWithTextInput() {
		bean.setType(Type.text);
		bean.setReportInputValue("hello");
		assertNotNull(bean.getFormattedInputValue());
	}

	@Test
	void validateDateTypeWithInvalidValue() {
		bean.setType(Type.date);
		bean.setReportInputValue("not-a-date");
		ValidationException e = new ValidationException();
		bean.validate(e, 0);
		assertFalse(e.getMessages().isEmpty());
	}

	@Test
	void validateDateTypeWithNullValueNoError() {
		bean.setType(Type.date);
		bean.setReportInputValue(null);
		ValidationException e = new ValidationException();
		bean.validate(e, 0);
		assertTrue(e.getMessages().isEmpty());
	}

	@Test
	void validateTestWithNumericalTestValueNoError() {
		bean.setRequired(Boolean.TRUE);
		bean.setNumericalTestValue(5L);
		ValidationException e = new ValidationException();
		bean.validateTest(e);
		assertTrue(e.getMessages().isEmpty());
	}
}
