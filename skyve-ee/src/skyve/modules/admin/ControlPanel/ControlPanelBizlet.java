package modules.admin.ControlPanel;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.skyve.CORE;
import org.skyve.impl.metadata.repository.AbstractRepository;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.module.Module;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;

import modules.ModulesUtil;
import modules.ModulesUtil.DomainValueSortByCode;
import modules.admin.domain.Contact;
import modules.admin.domain.ControlPanel;

public class ControlPanelBizlet extends Bizlet<ControlPanel> {
	private static final long serialVersionUID = -6033906392152210002L;

	@Override
	public ControlPanel newInstance(ControlPanel bean) throws Exception {
		Persistence p = CORE.getPersistence();
		Contact contact = p.retrieve(Contact.MODULE_NAME, Contact.DOCUMENT_NAME, CORE.getUser().getContactId(), false);
		bean.setEmailFrom(contact.getEmail1());

		return bean;
	}

	@Override
	public List<DomainValue> getVariantDomainValues(String attributeName) throws Exception {
		List<DomainValue> result = new ArrayList<>();

		if (ControlPanel.emailToPropertyName.equals(attributeName)) {
			Persistence pers = CORE.getPersistence();
			DocumentQuery query = pers.newDocumentQuery(Contact.MODULE_NAME, Contact.DOCUMENT_NAME);
			query.setMaxResults(100);

			List<Contact> contacts = query.beanResults();
			for (Contact contact : contacts) {
				if (contact.getEmail1() != null) {
					result.add(new DomainValue(contact.getEmail1()));
				}
			}
		}
		else if (ControlPanel.designModuleDocumentNamePropertyName.equals(attributeName)) {
			Customer c = CORE.getUser().getCustomer();
			for (Module m : c.getModules()) {
				for (String d : m.getDocumentRefs().keySet()) {
					result.add(new DomainValue(m.getName() + '.' + d));
				}
			}
			Collections.sort(result, new DomainValueSortByCode());
			return result;
		}
		else if (ControlPanel.customerNameToSwapToPropertyName.equals(attributeName)) {
			AbstractRepository rep = AbstractRepository.get();
			for (String cus : rep.getAllCustomerNames()) {
				result.add(new DomainValue(cus));
			}
			Collections.sort(result, new ModulesUtil.DomainValueSortByDescription());
		}

		return result;
	}
}
