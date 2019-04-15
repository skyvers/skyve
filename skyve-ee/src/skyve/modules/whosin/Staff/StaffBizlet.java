package modules.whosin.Staff;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.persistence.Persistence;
import org.skyve.web.WebContext;

import modules.whosin.domain.Position;
import modules.whosin.domain.Staff;
import modules.whosin.domain.Staff.Status;

public class StaffBizlet extends Bizlet<Staff> {

	/**
	 * 
	 */
	private static final long serialVersionUID = 5976944590775263367L;

	@Override
	public Staff preExecute(ImplicitActionName actionName, Staff bean, Bean parentBean, WebContext webContext) throws Exception {

		// Load the transient reportsTo attribute
		if (ImplicitActionName.Edit.equals(actionName)) {
			Position position = bean.getPosition();
			if (position != null) {
				bean.setReportsTo(position.getParent());
			}
		}

		// Update the hierarchy according to what is set in the reportsTo transient attribute
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

			//if location is within the office, set the status
			if(bean.getLocation().within(bean.getBaseOffice().getBoundary())) {
				bean.setStatus(Status.inTheOffice);
				
				// otherwise, if status was "in the office", set new status to "out of the office"
			} else if(Status.inTheOffice.equals(bean.getStatus())) {
				bean.setStatus(Status.outOfTheOffice);
			}
			
		}

		return super.preExecute(actionName, bean, parentBean, webContext);
	}

	@Override
	public void preRerender(String source, Staff bean, WebContext webContext) throws Exception {

		if(Staff.statusPropertyName.equals(source)) {
			//if status is "in the office" then set the location to be the centroid of the office boundary
			if(Status.inTheOffice.equals(bean.getStatus()) && bean.getBaseOffice()!=null && bean.getBaseOffice().getBoundary()!=null) {
				bean.setLocation(bean.getBaseOffice().getBoundary().getCentroid());
			} 
		}
		
		super.preRerender(source, bean, webContext);
	}

}
