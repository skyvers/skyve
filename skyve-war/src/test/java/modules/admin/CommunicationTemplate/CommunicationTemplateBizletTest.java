package modules.admin.CommunicationTemplate;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.ValidationException;

import modules.admin.domain.CommunicationTemplate;

@SuppressWarnings("static-method")
class CommunicationTemplateBizletTest {

	private static final CommunicationTemplateBizlet bizlet = new CommunicationTemplateBizlet();

	@Test
	void validateWithDefaultTemplateAddsNoErrors() throws Exception {
		CommunicationTemplate bean = new CommunicationTemplate();
		// Default template contains {body}
		ValidationException e = new ValidationException();
		bizlet.validate(bean, e);
		assertTrue(e.getMessages().isEmpty());
	}

	@Test
	void validateWithTemplateWithoutBodyAddsError() throws Exception {
		CommunicationTemplate bean = new CommunicationTemplate();
		bean.setTemplate("<html><body>No substitution here</body></html>");
		ValidationException e = new ValidationException();
		bizlet.validate(bean, e);
		assertFalse(e.getMessages().isEmpty());
	}
}
