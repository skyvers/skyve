package modules.admin.Snapshot;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.skyve.CORE;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.Module.DocumentRef;
import org.skyve.metadata.module.query.QueryDefinition;

import jakarta.enterprise.inject.Default;
import modules.admin.ModulesUtil.DomainValueSortByDescription;

/**
 * This class acts as a service layer to encapsulate domain logic.
 *
 * Add this line to classes that wish to use it: @Inject private transient SnapshotService snapshotService;
 */
@Default
public class SnapshotService {

	@SuppressWarnings("static-method")
	public List<DomainValue> getModuleDomainValues() {
		Customer customer = CORE.getUser().getCustomer();
		List<Module> modules = customer.getModules();
		List<DomainValue> result = new ArrayList<>(modules.size());
		for (Module module : modules) {
			result.add(new DomainValue(module.getName(), module.getLocalisedTitle()));
		}

		return result;
	}

	@SuppressWarnings("static-method")
	public List<DomainValue> getQueryDomainValues(String moduleName) {
		if (moduleName != null) {
			Customer customer = CORE.getCustomer();
			Module module = customer.getModule(moduleName);

			Map<String, DocumentRef> refs = module.getDocumentRefs();
			List<QueryDefinition> queries = module.getMetadataQueries();
			List<DomainValue> result = new ArrayList<>(refs.size() + queries.size());

			for (Entry<String, DocumentRef> ref : refs.entrySet()) {
				DocumentRef dr = ref.getValue();
				if (dr.getDefaultQueryName() == null) {
					String documentName = ref.getKey();
					Document document = module.getDocument(customer, documentName);
					Persistent persistent = document.getPersistent();
					if ((persistent != null) && (persistent.getName() != null)) { // persistent document
						result.add(new DomainValue(documentName,
								module.getDocument(customer, documentName).getLocalisedSingularAlias()));
					}
				}
			}
			for (QueryDefinition query : queries) {
				result.add(new DomainValue(query.getName(), query.getLocalisedDescription()));
			}
			result.sort(new DomainValueSortByDescription());

			return result;
		}

		return Collections.emptyList();
	}
}
