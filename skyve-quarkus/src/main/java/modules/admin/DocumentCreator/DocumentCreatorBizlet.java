package modules.admin.DocumentCreator;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.skyve.CORE;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.module.Module;
import org.skyve.util.Util;

import modules.admin.ModulesUtil.DomainValueSortByDescription;
import modules.admin.domain.DocumentCreator;
import modules.admin.domain.ModuleDocument;

public class DocumentCreatorBizlet extends Bizlet<DocumentCreator> {

	@Override
	public List<DomainValue> getVariantDomainValues(String attributeName) throws Exception {
		if (DocumentCreator.defaultModulePropertyName.equals(attributeName)) {
			List<DomainValue> values = new ArrayList<>();
			Customer c = CORE.getUser().getCustomer();
			for (Module m : c.getModules()) {
				ModuleDocument module = ModuleDocument.newInstance();
				module.setModuleName(m.getName());
				module.setDocumentName(null);
				DomainValue v = new DomainValue(m.getName());
				values.add(v);
			}

			Collections.sort(values, new DomainValueSortByDescription());
			return values;
		}

		return super.getVariantDomainValues(attributeName);
	}

	@Override
	public DocumentCreator newInstance(DocumentCreator bean) throws Exception {
		// populate the output directory from the JSON if provided
		if (Util.getModuleDirectory() != null) {
			bean.setOutputLocation(Util.getModuleDirectory());
		}

		return bean;
	}

}
