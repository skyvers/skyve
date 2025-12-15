package org.skyve.metadata.view.model.list;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.SortedMap;

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
	private String referenceBinding;
	
	/**
	 * A simple or compound binding expression to the collection to list with respect to the bean under edit.
	 * @param module	The reference's document's module.
	 * @param drivingDocument	The reference's document
	 * @param referenceBinding	The binding to the reference with respect to the edited bean - can be compound.
	 */
	protected ReferenceListModel(Module module, Document drivingDocument, String referenceBinding) {
		super(module, drivingDocument);
		this.referenceBinding = referenceBinding;
	}
	
	private String moduleName;
	private String drivingDocumentName;
	
	/**
	 * Convenience constructor
	 * @param drivingDocument	The reference's document
	 * @param referenceBinding	The binding to the reference with respect to the edited bean - can be compound.
	 */
	protected ReferenceListModel(Document drivingDocument, String referenceBinding) {
		this.moduleName = drivingDocument.getOwningModuleName();
		this.drivingDocumentName = drivingDocument.getName();
		this.referenceBinding = referenceBinding;
	}
	
	/**
	 * Convenience constructor
	 * @param moduleName	The reference's document's module name
	 * @param drivingDocumentName	The reference's document name
	 * @param referenceBinding	The binding to the reference with respect to the edited bean - can be compound.
	 */
	protected ReferenceListModel(String moduleName, String drivingDocumentName, String referenceBinding) {
		this.moduleName = moduleName;
		this.drivingDocumentName = drivingDocumentName;
		this.referenceBinding = referenceBinding;
	}
	
	@Override
	public void postConstruct(Customer customer, boolean runtime) {
		// resolve driving document if required (for convenience constructor usage)
		if ((moduleName != null) && (drivingDocumentName != null)) {
			Module m = customer.getModule(moduleName);
			Document d = m.getDocument(customer, drivingDocumentName);
			setDrivingDocument(m, d);
		}
		
		super.postConstruct(customer, runtime);
	}
	
	/**
	 * Return 1 association row or all collection rows.
	 */
	@Override
	public List<Bean> getRows() throws Exception {
		T bean = getBean();
		if (bean != null) {
			Object value = Binder.get(bean, referenceBinding);
			if (value instanceof List list) {
				@SuppressWarnings("unchecked")
				List<Bean> values = list;
				// Make a defensive copy of the actual list here as it will be mutated by the model
				return new ArrayList<>(values);
			}

			if (value instanceof Bean beanValue) {
				// Note we can't use Collections.singletonList here as that is immutable and grid implementations may add a summary row to this list.
				List<Bean> result = new ArrayList<>(1);
				result.add(beanValue);
				return result;
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
