package org.skyve.metadata.view.model.list;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.SortedMap;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.util.Binder;

/**
 * A class to extend to make a quick model based on a collection or an association within the edited bean.
 * The referenceBinding can be compound.
 * Update() and remove() are not implemented so make sure your listGrid is read-only or implement them yourself.
 * 
 * @author mike
 *
 * @param <T>	The type of the bean under edit.
 */
public abstract class ReferenceListModel<T extends Bean> extends InMemoryListModel<T> {
	private static final long serialVersionUID = 6928933794461259542L;

	private String referenceBinding;
	
	/**
	 * A simple or compound binding expression to the collection to list with respect to the bean under edit.
	 * @param module	The reference's document's module.
	 * @param drivingDocument	The reference's document
	 * @param referenceBinding	The binding to the reference with respect to the edited bean - can be compound.
	 * @throws Exception
	 */
	public ReferenceListModel(Module module, Document drivingDocument, String referenceBinding)
	throws Exception {
		this.referenceBinding = referenceBinding;
		setDrivingDocument(module, drivingDocument);
	}
	
	/**
	 * Convenience constructor
	 * @param drivingDocument	The reference's document
	 * @param referenceBinding	The binding to the reference with respect to the edited bean - can be compound.
	 * @throws Exception
	 */
	public ReferenceListModel(Document drivingDocument, String referenceBinding)
	throws Exception {
		this(CORE.getCustomer().getModule(drivingDocument.getOwningModuleName()), drivingDocument, referenceBinding);
	}
	
	/**
	 * Convenience constructor
	 * @param moduleName	The reference's document's module name
	 * @param drivingDocumentName	The reference's document name
	 * @param referenceBinding	The binding to the reference with respect to the edited bean - can be compound.
	 * @throws Exception
	 */
	public ReferenceListModel(String moduleName, String drivingDocumentName, String referenceBinding)
	throws Exception {
		Customer c = CORE.getCustomer();
		Module m = c.getModule(moduleName);
		Document d = m.getDocument(c, drivingDocumentName);
		this.referenceBinding = referenceBinding;
		setDrivingDocument(m, d);
	}
	
	/**
	 * Return 1 association row or all collection rows.
	 */
	@Override
	public List<Bean> getRows() throws Exception {
		T bean = getBean();
		if (bean != null) {
			Object value = Binder.get(bean, referenceBinding);
			if (value instanceof List) {
				// Make a defensive copy of the actual list here as it will be mutated by the model
				@SuppressWarnings("unchecked")
				List<Bean> beans = new ArrayList<>((List<Bean>) value);
				return beans;
			}

			if (value instanceof Bean) {
				return Collections.singletonList((Bean) value);
			}
		}

		return Collections.emptyList();
	}
	
	/**
	 * Not implemented.
	 */
	@Override
	public Bean update(String bizId, SortedMap<String, Object> properties) throws Exception {
		throw new IllegalStateException("NOT IMPLEMENTED");
	}

	/**
	 * Not implemented.
	 */
	@Override
	public void remove(String bizId) throws Exception {
		throw new IllegalStateException("NOT IMPLEMENTED");
	}
}
