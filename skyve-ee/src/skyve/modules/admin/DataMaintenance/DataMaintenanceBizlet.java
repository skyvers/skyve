package modules.admin.DataMaintenance;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.skyve.CORE;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;

import modules.ModulesUtil.DomainValueSortByDescription;
import modules.admin.domain.DataMaintenance;

public class DataMaintenanceBizlet extends Bizlet<DataMaintenance> {
	private static final long serialVersionUID = 1L;

	@Override
	public DataMaintenance newInstance(DataMaintenance bean) throws Exception {
		Persistence persistence = CORE.getPersistence();
		DocumentQuery q = persistence.newDocumentQuery(DataMaintenance.MODULE_NAME, DataMaintenance.DOCUMENT_NAME);
		DataMaintenance result = q.beanResult();
		if (result == null) {
			result = bean;
		}

		return result;
	}

	@Override
	public List<DomainValue> getVariantDomainValues(String attributeName) throws Exception {
		if (DataMaintenance.modDocNamePropertyName.equals(attributeName)) {
			List<DomainValue> result = new ArrayList<>();

			Customer c = CORE.getUser().getCustomer();
			for (Module m : c.getModules()) {
				for (String k : m.getDocumentRefs().keySet()) {
					Document d = m.getDocument(c, k);
					if (d.getPersistent() != null) {
						result.add(new DomainValue(String.format("%s.%s", m.getName(), k), 
													String.format("%s.%s", m.getTitle(), d.getSingularAlias())));
					}
				}
			}
			Collections.sort(result, new DomainValueSortByDescription());

			return result;
		}

		return super.getVariantDomainValues(attributeName);
	}
}
