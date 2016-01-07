package modules.admin.DataMaintenance;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import modules.ModulesUtil.DomainValueSortByCode;
import modules.admin.domain.DataMaintenance;

import org.skyve.CORE;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.module.Module;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;

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

		return bean;
	}

	@Override
	public List<DomainValue> getVariantDomainValues(String attributeName) throws Exception {
		if (DataMaintenance.modDocNamePropertyName.equals(attributeName)) {
			List<DomainValue> result = new ArrayList<>();

			// If database has just been truncated, the user and customer will not exist
			Customer c = CORE.getUser().getCustomer();
			for (Module m : c.getModules()) {
				for (String d : m.getDocumentRefs().keySet()) {
					result.add(new DomainValue(m.getName() + '.' + d, m.getTitle() + '.' + d));
				}
			}
			Collections.sort(result, new DomainValueSortByCode());

			return result;
		}

		return super.getVariantDomainValues(attributeName);
	}
}
