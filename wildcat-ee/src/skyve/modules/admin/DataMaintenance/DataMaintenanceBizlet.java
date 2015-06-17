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

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	@Override
	public DataMaintenance newInstance(DataMaintenance bean) throws Exception {

		DataMaintenance result = bean;
		Persistence persistence = CORE.getPersistence();
		DocumentQuery q = persistence.newDocumentQuery(DataMaintenance.MODULE_NAME, DataMaintenance.DOCUMENT_NAME);
		List<DataMaintenance> d = q.beanResults();
		if (d.size() > 0) {
			result = d.get(0);
		}

		return super.newInstance(result);
	}

	@Override
	public List<DomainValue> getVariantDomainValues(String attributeName) throws Exception {

		List<DomainValue> result = new ArrayList<>();

		if (DataMaintenance.modDocNamePropertyName.equals(attributeName)) {
			Customer customer = CORE.getUser().getCustomer();
			for (Module mod : customer.getModules()) {
				for (String d : mod.getDocumentRefs().keySet()) {
					result.add(new DomainValue(mod.getName() + '.' + d, mod.getTitle() + '.' + d));
				}
			}
			Collections.sort(result, new DomainValueSortByCode());
			return result;
		}

		return super.getVariantDomainValues(attributeName);
	}


}
