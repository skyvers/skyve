package org.skyve.metadata.view.model.list;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.SortedMap;
import java.util.TreeMap;

import org.skyve.domain.Bean;
import org.skyve.domain.DynamicBean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.TransientBean;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.persistence.DocumentQuery;
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
	public ReferenceListModel(Module module, Document drivingDocument, String referenceBinding) {
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
	public ReferenceListModel(Document drivingDocument, String referenceBinding) {
		this.moduleName = drivingDocument.getOwningModuleName();
		this.drivingDocumentName = drivingDocument.getName();
	}
	
	/**
	 * Convenience constructor
	 * @param moduleName	The reference's document's module name
	 * @param drivingDocumentName	The reference's document name
	 * @param referenceBinding	The binding to the reference with respect to the edited bean - can be compound.
	 */
	public ReferenceListModel(String moduleName, String drivingDocumentName, String referenceBinding) {
		this.moduleName = moduleName;
		this.drivingDocumentName = drivingDocumentName;
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
			if (value instanceof List) {
				@SuppressWarnings("unchecked")
				List<Bean> values = (List<Bean>) value;
				// Make a defensive copy of the actual list here as it will be mutated by the model
				List<Bean> result = new ArrayList<>(values.size());
				for (Bean element : values) {
					result.add(defendTransientBean(element));
				}
				return result;
			}

			if (value instanceof Bean) {
				// Note we can't use Collections.singletonList here as that is immutable and grid implementations may add a summary row to this list.
				List<Bean> result = new ArrayList<>(1);
				result.add((Bean) value);
				return result;
			}
		}

		return Collections.emptyList();
	}
	
	private static Bean defendTransientBean(Bean bean) {
		if (bean instanceof TransientBean) {
			Map<String, Object> properties = new TreeMap<>();
			properties.put(DocumentQuery.THIS_ALIAS, bean);
			properties.put(PersistentBean.LOCK_NAME, null);
			properties.put(PersistentBean.TAGGED_NAME, Boolean.FALSE);
			properties.put(PersistentBean.FLAG_COMMENT_NAME, null);
			return new DynamicBean(bean.getBizModule(), bean.getBizDocument(), properties);
		}
		return bean;
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
