package modules.whosin.Staff;

import modules.whosin.domain.Position;
import modules.whosin.domain.Staff;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.web.WebContext;

public class StaffBizlet extends Bizlet<Staff> {

	/**
	 * 
	 */
	private static final long serialVersionUID = 5976944590775263367L;

//	@Override
//	public Staff preExecute(ImplicitActionName actionName, Staff bean, Bean parentBean, WebContext webContext) throws Exception {
//
//		if (ImplicitActionName.Edit.equals(actionName)) {
//			Position position = getPositionOf(bean);
//			if (position != null) {
//				bean.setReportsTo(position.getParent());
//			} else {
//				bean.setReportsTo(null);
//			}
//		}
//
//		if (ImplicitActionName.Save.equals(actionName) || ImplicitActionName.OK.equals(actionName)) {
//			Position pos = getPositionOf(bean);
//			if (pos != null) {
//				pos.setReportsTo(bean.getReportsTo());
//			} else if(bean.originalValues().containsKey(Staff.reportsToPropertyName)){
//				// create a new position and set the reports to
//				Persistence pers = CORE.getPersistence();
//				User user = pers.getUser();
//				Customer customer = user.getCustomer();
//				Module module = customer.getModule(Position.MODULE_NAME);
//				Document document = module.getDocument(customer, Position.DOCUMENT_NAME);
//
//				Position newPosition = document.newInstance(user);
//				newPosition.setStaff(bean);
//				newPosition.setPositionTitle(bean.getRoleTitle());
//				newPosition.setReportsTo(bean.getReportsTo());
//				
//				newPosition = pers.save(newPosition);
//			}
//		}
//
//		return super.preExecute(actionName, bean, parentBean, webContext);
//	}
//
//	public static Position getPositionOf(Staff bean) throws Exception {
//
//		Persistence pers = CORE.getPersistence();
//		DocumentQuery q = pers.newDocumentQuery(Position.MODULE_NAME, Position.DOCUMENT_NAME);
//		q.getFilter().addEquals(Position.staffPropertyName, bean);
//
//		Position position = q.beanResult();
//		return position;
//	}
}
