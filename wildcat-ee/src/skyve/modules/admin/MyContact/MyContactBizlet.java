package modules.admin.MyContact;

import java.util.List;

import modules.admin.domain.Contact;
import modules.admin.domain.MyContact;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;


public class MyContactBizlet extends Bizlet<MyContact> {

	/**
	 * 
	 */
	private static final long serialVersionUID = 5831916461072143515L;

	@Override
	public MyContact newInstance(MyContact bean) throws Exception {

		//get the contact corresponding to the currently logged in user
		Persistence pers = CORE.getPersistence();
		DocumentQuery q = pers.newDocumentQuery(Contact.MODULE_NAME, Contact.DOCUMENT_NAME);
		q.getFilter().addEquals(Bean.DOCUMENT_ID, pers.getUser().getContactId());
		
		List<Contact> contacts= pers.retrieve(q);
		if(!contacts.isEmpty()){
			bean.setMyContact(contacts.get(0));
		}
		
		return super.newInstance(bean);
	}

	
}
