package modules.admin.ReportParameter;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.domain.app.admin.ReportParameter.Type;
import org.skyve.domain.messages.ValidationException;

@SuppressWarnings("static-method")
public class ReportParameterBizletTest {

	private static final ReportParameterBizlet bizlet = new ReportParameterBizlet();

	@Test
	void validateWithValidNameAddsNoErrors() throws Exception {
		ReportParameterExtension bean = new ReportParameterExtension();
		bean.setName("validParam123");
		ValidationException e = new ValidationException();
		bizlet.validate(bean, e);
		assertTrue(e.getMessages().isEmpty());
	}

	@Test
	void validateWithWhitespaceInNameAddsError() throws Exception {
		ReportParameterExtension bean = new ReportParameterExtension();
		bean.setName("invalid param");
		ValidationException e = new ValidationException();
		bizlet.validate(bean, e);
		assertFalse(e.getMessages().isEmpty());
	}

	@Test
	void validateWithSpecialCharsInNameAddsError() throws Exception {
		ReportParameterExtension bean = new ReportParameterExtension();
		bean.setName("invalid-param!");
		ValidationException e = new ValidationException();
		bizlet.validate(bean, e);
		assertFalse(e.getMessages().isEmpty());
	}

	@Test
	void validateRequiredWithNoTestValueAddsError() throws Exception {
		ReportParameterExtension bean = new ReportParameterExtension();
		bean.setName("myParam");
		bean.setRequired(Boolean.TRUE);
		bean.setType(Type.text);
		// No test value set - should fail
		ValidationException e = new ValidationException();
		bizlet.validate(bean, e);
		assertFalse(e.getMessages().isEmpty());
	}

	@Test
	void validateRequiredDateTypeWithNoTestValueAddsError() throws Exception {
		ReportParameterExtension bean = new ReportParameterExtension();
		bean.setName("dateParam");
		bean.setRequired(Boolean.TRUE);
		bean.setType(Type.date);
		ValidationException e = new ValidationException();
		bizlet.validate(bean, e);
		assertFalse(e.getMessages().isEmpty());
	}

	@Test
	void validateRequiredIntegerTypeWithNoTestValueAddsError() throws Exception {
		ReportParameterExtension bean = new ReportParameterExtension();
		bean.setName("intParam");
		bean.setRequired(Boolean.TRUE);
		bean.setType(Type.integer);
		ValidationException e = new ValidationException();
		bizlet.validate(bean, e);
		assertFalse(e.getMessages().isEmpty());
	}
}
