package modules.admin.ControlPanel;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.skyve.CORE;
import org.skyve.impl.metadata.repository.AbstractRepository;
import org.skyve.impl.metadata.repository.router.Router;
import org.skyve.impl.metadata.repository.router.UxUiMetadata;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.module.Module;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.util.Util;

import modules.admin.ModulesUtil;
import modules.admin.ModulesUtil.DomainValueSortByCode;
import modules.admin.domain.Contact;
import modules.admin.domain.ControlPanel;
import modules.admin.domain.ControlPanel.SailTestStrategy;
import modules.admin.domain.UserProxy;

public class ControlPanelBizlet extends Bizlet<ControlPanelExtension> {
	private static final long serialVersionUID = -6033906392152210002L;

	@Override
	public ControlPanelExtension newInstance(ControlPanelExtension bean) throws Exception {
		// Set the user name and email to the logged in user
		UserProxy user = ModulesUtil.currentAdminUserProxy();
		bean.setSailUser(user);
		Contact contact = user.getContact();
		bean.setEmailFrom(contact.getEmail1());

		bean.setSailBaseUrl(Util.getSkyveContextUrl() + '/');
		bean.setSailTestStrategy(SailTestStrategy.None);

		// Set module name to the first non-admin module found
		AbstractRepository r = AbstractRepository.get();
		for (String moduleName : r.getAllVanillaModuleNames()) {
			if (! ControlPanel.MODULE_NAME.equals(moduleName)) {
				bean.setSailModuleName(moduleName);
			}
		}
		
		bean.loadStartupConfiguration();

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
		else if (ControlPanel.sailModuleNamePropertyName.equals(attributeName)) {
			AbstractRepository r = AbstractRepository.get();
			for (String moduleName : r.getAllVanillaModuleNames()) {
					result.add(new DomainValue(moduleName));
			}
		}
		else if (ControlPanel.sailUxUiPropertyName.equals(attributeName)) {
			Router r = CORE.getRepository().getRouter();
			for (UxUiMetadata uxui : r.getUxUis()) {
				result.add(new DomainValue(uxui.getName()));
			}
		}

		return result;
	}
}
