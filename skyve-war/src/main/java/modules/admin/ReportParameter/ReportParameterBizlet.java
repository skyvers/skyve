package modules.admin.ReportParameter;

import org.apache.commons.lang3.StringUtils;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.model.document.Bizlet;

import modules.admin.domain.ReportParameter;

public class ReportParameterBizlet extends Bizlet<ReportParameterExtension> {

	@Override
	public void validate(ReportParameterExtension bean, ValidationException e) throws Exception {
		// validate the report name cannot contain spaces or special characters
		if (StringUtils.containsWhitespace(bean.getName())) {
			e.getMessages().add(new Message(ReportParameter.namePropertyName,
					String.format("Parameter name (%s) cannot contain spaces.", bean.getName())));
			return;
		}

		if (!bean.getName().matches("^[A-Za-z0-9_]+$")) {
			e.getMessages().add(new Message(ReportParameter.namePropertyName,
					String.format("Parameter name (%s) can only contain letters or digits.", bean.getName())));
		}

		if (Boolean.TRUE.equals(bean.getRequired())) {
			if (bean.getTestValueString() == null) {
				switch (bean.getType()) {
					case date:
						e.getMessages()
								.add(new Message(ReportParameter.dateTestValuePropertyName, "Test value is required."));
						break;
					case integer:
					case longInteger:
						e.getMessages()
								.add(new Message(ReportParameter.numericalTestValuePropertyName, "Test value is required."));
						break;
					case text:
						e.getMessages()
								.add(new Message(ReportParameter.textTestValuePropertyName, "Test value is required."));
						break;
					default:
						e.getMessages().add(new Message("Test value is required."));
				}
			}
		}
		super.validate(bean, e);
	}

}
