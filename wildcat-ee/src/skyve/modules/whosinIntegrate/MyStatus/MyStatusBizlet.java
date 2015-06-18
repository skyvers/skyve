package modules.whosinIntegrate.MyStatus;

import modules.whosinIntegrate.domain.MyStatus;
import modules.whosinIntegrate.domain.Staff;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.util.Binder;

public class MyStatusBizlet extends Bizlet<MyStatus> {
	private static final long serialVersionUID = 5831916461072143515L;

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
