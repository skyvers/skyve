package modules.whosin.Staff;

import modules.whosin.domain.Position;
import modules.whosin.domain.Staff;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.persistence.Persistence;
import org.skyve.web.WebContext;

public class StaffBizlet extends Bizlet<Staff> {

	/**
	 * 
	 */
	private static final long serialVersionUID = 5976944590775263367L;

	@Override
	public Staff preExecute(ImplicitActionName actionName, Staff bean, Bean parentBean, WebContext webContext) throws Exception {

		if (ImplicitActionName.Edit.equals(actionName)) {
			Position position = bean.getPosition();
			if (position != null) {
				bean.setReportsTo(position.getParent());
			} else {
				bean.setReportsTo(null);
			}
		}

		if (ImplicitActionName.Save.equals(actionName) || ImplicitActionName.OK.equals(actionName)) {
			Persistence pers = CORE.getPersistence();
			Position myPosition = bean.getPosition();

			if (myPosition != null) {

				// assign reports to of the associated position
				myPosition.setReportsTo(bean.getReportsTo());

				// need to update the position that contains this staff
				myPosition.setPositionTitle(bean.getRoleTitle());
				myPosition = pers.save(myPosition);

			} else if (bean.originalValues().containsKey(Staff.reportsToPropertyName)) {

				// create a new position and set the reports to

				Position newPosition = Position.newInstance();
				newPosition.setStaff(bean);
				newPosition.setPositionTitle(bean.getRoleTitle());
				newPosition.setReportsTo(bean.getReportsTo());

				newPosition = pers.save(newPosition);
			}

		}

		return super.preExecute(actionName, bean, parentBean, webContext);
	}
}
