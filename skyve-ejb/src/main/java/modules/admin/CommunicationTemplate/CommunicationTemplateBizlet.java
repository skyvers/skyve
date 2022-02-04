package modules.admin.CommunicationTemplate;

import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.model.document.Bizlet;

import modules.admin.domain.CommunicationTemplate;

public class CommunicationTemplateBizlet extends Bizlet<CommunicationTemplate> {

	public static final String TEMPLATE_BODY = "{body}";

	@Override
	public void validate(CommunicationTemplate bean, ValidationException e) throws Exception {
		if(bean.getTemplate() == null) {
			e.getMessages().add(new Message(CommunicationTemplate.templatePropertyName, "Template is required."));
		}
		
		// make sure that the template contains the substitution binding
		if(bean.getTemplate().indexOf(TEMPLATE_BODY) < 0) {
			e.getMessages().add(new Message(CommunicationTemplate.templatePropertyName, String.format(
					"%s must be present in the template at the point where the email body is to be inserted.", TEMPLATE_BODY)));
		}
		
		super.validate(bean, e);
	}

}
