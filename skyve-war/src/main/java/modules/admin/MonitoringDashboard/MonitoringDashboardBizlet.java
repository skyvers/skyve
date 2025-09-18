package modules.admin.MonitoringDashboard;

import java.util.ArrayList;
import java.util.List;

import org.skyve.CORE;
import org.skyve.metadata.model.document.SingletonCachedBizlet;

import modules.admin.domain.MonitoringDashboard;

public class MonitoringDashboardBizlet extends SingletonCachedBizlet<MonitoringDashboard> {

	@Override
	public List<DomainValue> getDynamicDomainValues(String attributeName, MonitoringDashboard bean) throws Exception {
		List<DomainValue> results = new ArrayList<>();
		if (MonitoringDashboard.documentNamePropertyName.equals(attributeName)) {
			CORE.getCustomer()
					.getModules()
					.stream()
					.forEach(m -> m.getDocumentRefs()
							.forEach((dName, dRef) -> results.add(new DomainValue(m.getName() + "." + dName))));
			return results;
		}
		return super.getDynamicDomainValues(attributeName, bean);
	}
}
