package modules.admin.ReportParameter;

import org.apache.commons.lang3.StringUtils;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.model.document.Bizlet;

import modules.admin.domain.ReportParameter;

/**
 * Validates report parameter naming and required test values during report design.
 */
public class ReportParameterBizlet extends Bizlet<ReportParameterExtension> {
	private static final String TEST_VALUE_REQUIRED = "Test value is required.";

	/**
	 * Validates parameter naming conventions and required test-value inputs.
	 * @param bean the bean value
	 * @param e the e value
	 * @throws Exception if the operation fails
	 */
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
								.add(new Message(ReportParameter.dateTestValuePropertyName, TEST_VALUE_REQUIRED));
						break;
					case integer:
					case longInteger:
						e.getMessages()
								.add(new Message(ReportParameter.numericalTestValuePropertyName, TEST_VALUE_REQUIRED));
						break;
					case text:
						e.getMessages()
								.add(new Message(ReportParameter.textTestValuePropertyName, TEST_VALUE_REQUIRED));
						break;
					default:
						e.getMessages().add(new Message(TEST_VALUE_REQUIRED));
				}
			}
		}
		super.validate(bean, e);
	}
}
