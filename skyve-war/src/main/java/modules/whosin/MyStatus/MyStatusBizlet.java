package modules.whosin.MyStatus;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.util.Binder;

import modules.whosin.domain.MyStatus;
import modules.whosin.domain.Staff;

/**
 * Initialises {@link MyStatus} records for the currently logged-in staff member.
 */
public class MyStatusBizlet extends Bizlet<MyStatus> {
	/**
	 * Resolves the staff record associated with the active user and assigns it to the status bean.
	 *
	 * @param bean the transient status bean
	 * @return the initialised status bean
	 * @throws Exception if staff lookup fails
	 */
	@Override
	public MyStatus newInstance(MyStatus bean) throws Exception {
		
		//get the contact corresponding to the currently logged in user
		Persistence pers = CORE.getPersistence();
		DocumentQuery q = pers.newDocumentQuery(Staff.MODULE_NAME, Staff.DOCUMENT_NAME);
		q.getFilter().addEquals(Binder.createCompoundBinding(Staff.contactPropertyName,Bean.DOCUMENT_ID), pers.getUser().getContactId());
		
		Staff staff = q.beanResult();
		bean.setMyStaff(staff);
		
		return bean;
	}
}
