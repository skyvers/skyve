package modules.admin.ReportTemplate.actions;

import java.util.Objects;

import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.admin.domain.ReportTemplate;

public class AddUserToEmail implements ServerSideAction<ReportTemplate> {

	@Override
	public ServerSideActionResult<ReportTemplate> execute(ReportTemplate bean, WebContext webContext) throws Exception {
		if (bean.getNewUserToEmail() == null) {
			throw new ValidationException(ReportTemplate.newUserToEmailPropertyName, "Please select an existing user to email.");
		}

		if (bean.getUsersToEmail().stream().anyMatch(u -> Objects.equals(u, bean.getNewUserToEmail()))) {
			throw new ValidationException(ReportTemplate.newUserToEmailPropertyName, "User has already been added.");
		}

		bean.getUsersToEmail().add(bean.getNewUserToEmail());
		bean.setNewUserToEmail(null);

		return new ServerSideActionResult<>(bean);
	}
}
