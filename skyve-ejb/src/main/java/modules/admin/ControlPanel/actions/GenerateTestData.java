package modules.admin.ControlPanel.actions;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.JobMetaData;
import org.skyve.metadata.module.Module;
import org.skyve.util.BeanValidator;
import org.skyve.util.PushMessage;
import org.skyve.util.Util;
import org.skyve.web.WebContext;

import modules.admin.ControlPanel.ControlPanelExtension;
import modules.admin.domain.ControlPanel;

public class GenerateTestData implements ServerSideAction<ControlPanelExtension> {

	@Override
	public ServerSideActionResult<ControlPanelExtension> execute(ControlPanelExtension bean, WebContext webContext)
			throws Exception {
		// validate required fields
		validateFields(bean);

		Customer customer = CORE.getCustomer();
		Module m = customer.getModule(ControlPanel.MODULE_NAME);
		JobMetaData job = m.getJob("jGenerateTestData");

		EXT.getJobScheduler().runOneShotJob(job, bean, CORE.getUser());
		EXT.push(new PushMessage().user(CORE.getUser()).rerender());

		return new ServerSideActionResult<>(bean);
	}

	private static void validateFields(ControlPanelExtension bean) {
		ValidationException ve = new ValidationException();

		if (bean.getTestModuleName() == null) {
			ve.getMessages().add(new Message(ControlPanel.testModuleNamePropertyName,
					Util.i18n(BeanValidator.VALIDATION_REQUIRED_KEY, Util.i18n("admin.controlPanel.testModuleName.displayName"))));
		}

		if (bean.getTestDocumentNames().size() == 0) {
			ve.getMessages().add(new Message(ControlPanel.testDocumentNamesPropertyName, "At least one Document is required."));
		}

		if (bean.getTestNumberToGenerate() == null) {
			ve.getMessages().add(new Message(ControlPanel.testNumberToGeneratePropertyName,
					Util.i18n(BeanValidator.VALIDATION_REQUIRED_KEY,
							Util.i18n("admin.controlPanel.testNumberToGenerate.displayName"))));
		}

		if (bean.getTestNumberToGenerate() == null || bean.getTestNumberToGenerate().intValue() < 1) {
			ve.getMessages().add(new Message(ControlPanel.testNumberToGeneratePropertyName,
					Util.i18n("admin.controlPanel.testNumberToGenerate.displayName") + " must be greater than 0."));

		} else if (bean.getTestNumberToGenerate().intValue() > 10000) {
			ve.getMessages().add(new Message(ControlPanel.testNumberToGeneratePropertyName,
					Util.i18n("admin.controlPanel.testNumberToGenerate.displayName") + " must be less than 10,000."));
		}
		
		if(Boolean.TRUE.equals(bean.getTestTagGeneratedData()) && bean.getTestTagName()==null) {
			ve.getMessages().add(new Message(ControlPanel.testTagNamePropertyName, "Enter the name of a tag for data tagging"));
		}

		if (ve.getMessages().size() > 0) {
			throw ve;
		}
	}
}