package modules.admin.Snapshot;

import java.util.ArrayList;
import java.util.List;

import org.skyve.CORE;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.QueryDefinition;
import org.skyve.persistence.Persistence;

import modules.admin.domain.Snapshot;

public class SnapshotBizlet extends Bizlet<Snapshot> {

	@Override
	public List<DomainValue> getDynamicDomainValues(String attributeName, Snapshot bean) throws Exception {

		Persistence pers = CORE.getPersistence();
		List<DomainValue> result = new ArrayList<>();

		Customer customer = pers.getUser().getCustomer();

		if (Snapshot.queryNamePropertyName.equals(attributeName) && bean.getModuleName()!=null) {
			Module module = customer.getModule(bean.getModuleName());
			
			for (QueryDefinition query : module.getMetadataQueries()) {
				result.add(new DomainValue(query.getName(), query.getLocalisedDescription()));
			}
		}

		return result;
	}

	@Override
	public List<DomainValue> getVariantDomainValues(String attributeName) throws Exception {

		List<DomainValue> result = new ArrayList<>();

		Customer customer = CORE.getUser().getCustomer();
		if (Snapshot.moduleNamePropertyName.equals(attributeName)) {
			for (Module module : customer.getModules()) {
				result.add(new DomainValue(module.getName(), module.getLocalisedTitle()));
			}
		}

		return result;
	}

}
