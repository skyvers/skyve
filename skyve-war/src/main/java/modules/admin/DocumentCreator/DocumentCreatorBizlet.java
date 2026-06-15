package modules.admin.DocumentCreator;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.skyve.CORE;
import org.skyve.domain.types.Enumeration.DomainValueSortByDescription;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.module.Module;
import org.skyve.util.Util;

import modules.admin.domain.DocumentCreator;
import modules.admin.domain.ModuleDocument;

/**
 * Provides Bizlet behaviour for the document-creator admin workflow.
 * <p>
 * The bizlet supplies module domain values for script defaults and initialises
 * a default output location when the runtime module directory is configured.
 */
public class DocumentCreatorBizlet extends Bizlet<DocumentCreator> {
	/**
	 * Returns module names that can be used as the script default module.
	 *
	 * @param attributeName
	 *        the attribute requesting domain values
	 * @return the sorted module domain values for {@link DocumentCreator#defaultModulePropertyName},
	 *         otherwise the superclass result
	 * @throws Exception
	 *         if domain value resolution fails
	 */
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

	/**
	 * Initialises the new bean with a default output directory when available.
	 *
	 * @param bean
	 *        the new document-creator bean
	 * @return the same bean instance after default initialisation
	 * @throws Exception
	 *         if initialisation fails
	 */
	@Override
	public DocumentCreator newInstance(DocumentCreator bean) throws Exception {
		// populate the output directory from the JSON if provided
		if (Util.getModuleDirectory() != null) {
			bean.setOutputLocation(Util.getModuleDirectory());
		}

		return bean;
	}
}
