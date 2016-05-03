package modules.whosinIntegrate.Staff;

import modules.admin.domain.Contact;
import modules.admin.domain.Contact.ContactType;
import modules.whosinIntegrate.domain.Staff;

import org.skyve.CORE;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.persistence.Persistence;

public class StaffBizlet extends Bizlet<Staff> {
	private static final long serialVersionUID = -5073318444540975484L;

	@Override
	public Staff newInstance(Staff bean) throws Exception {

		Contact contact = Contact.newInstance();
		contact.setContactType(ContactType.person);
		bean.setContact(contact);

		return bean;
	}

	public static boolean staffIsMe(Staff bean) {

		Persistence pers = CORE.getPersistence();
		boolean result = pers.getUser().getContactId().equals(bean.getContact().getBizId());
		return result;

	}

}
