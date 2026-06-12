package modules.whosin.Staff;

import org.skyve.domain.Bean;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.web.WebContext;

import modules.whosin.domain.Staff;
import modules.whosin.domain.Staff.Status;

/**
 * Applies hierarchy and location-driven status rules for {@link Staff} records.
 */
public class StaffBizlet extends Bizlet<Staff> {
	/**
	 * Synchronises transient reporting relationships and office-presence status before execution.
	 *
	 * @param actionName the implicit action being executed
	 * @param bean the staff bean under edit
	 * @param parentBean the parent bean in the interaction context
	 * @param webContext the active web context
	 * @return the bean to continue processing
	 * @throws Exception if lifecycle processing fails
	 */
	@Override
	@SuppressWarnings("java:S3776") // Complexity OK
	public Staff preExecute(ImplicitActionName actionName, Staff bean, Bean parentBean, WebContext webContext) throws Exception {

		if(ImplicitActionName.Edit.equals(actionName)) {
			//load to the staff this person reports to
			bean.setReportsTo(bean.getParent());
		}

		// Update the hierarchy according to what is set in the reportsTo transient attribute
		if (ImplicitActionName.Save.equals(actionName) || ImplicitActionName.OK.equals(actionName)) {
			
			if(bean.originalValues().containsKey(Staff.reportsToPropertyName)) {
				if(bean.getReportsTo()==null) {
					bean.setBizParentId(null);
				} else {
					bean.setBizParentId(bean.getReportsTo().getBizId());
				}
			}

			// if location is within the office, set the status
			if (bean.getBaseOffice() != null && bean.getBaseOffice().getBoundary() != null) {
				if (bean.getLocation().within(bean.getBaseOffice().getBoundary())) {
					bean.setStatus(Status.inTheOffice);
					// otherwise, if status was "in the office", set new status to "out of the office"
				} else if (Status.inTheOffice.equals(bean.getStatus())) {
					bean.setStatus(Status.outOfTheOffice);
				}
			}
		}

		return super.preExecute(actionName, bean, parentBean, webContext);
	}

	/**
	 * Repositions staff to the office centroid when status is set to in-office during rerender.
	 *
	 * @param source the source binding that triggered rerender
	 * @param bean the staff bean being rerendered
	 * @param webContext the active web context
	 * @throws Exception if rerender preprocessing fails
	 */
	@Override
	public void preRerender(String source, Staff bean, WebContext webContext) throws Exception {

		if (Staff.statusPropertyName.equals(source)) {
			// if status is "in the office" then set the location to be the centroid of the office boundary
			if (Status.inTheOffice.equals(bean.getStatus()) && bean.getBaseOffice() != null && bean.getBaseOffice().getBoundary() != null) {
				bean.setLocation(bean.getBaseOffice().getBoundary().getCentroid());
			}
		}

		super.preRerender(source, bean, webContext);
	}
}
