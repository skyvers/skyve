package org.skyve.impl.web.faces.models;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.TreeMap;

import javax.faces.FacesException;

import org.primefaces.model.DualListModel;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.web.WebUtil;
import org.skyve.impl.web.faces.beans.FacesView;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.DomainType;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.module.Module;
import org.skyve.persistence.Persistence;
import org.skyve.util.Binder;
import org.skyve.util.Binder.TargetMetaData;

/**
 * Deals with DomainValues in this model and uses scatter and gather methods to
 * effect changes in the domain model.
 * 
 * @author mike
 */
public class SkyveDualListModelMap extends TreeMap<String, DualListModel<DomainValue>> {
	private static final long serialVersionUID = -9080244378508878127L;

	private FacesView<? extends Bean> view;

	public SkyveDualListModelMap(FacesView<? extends Bean> view) {
		this.view = view;
	}

	/**
	 * Override get() to lazily create and scatter a DualListModel.
	 * 
	 * @param key	The lookup description binding.
	 */
	@Override
	public DualListModel<DomainValue> get(Object key) {
		if (! containsKey(key)) {
			scatter((String) key);
		}

		return super.get(key);
	}

	/**
	 * Populate the source and target lists of the model with domain values and domain model values.
	 * 
	 * @param binding
	 */
	private void scatter(String binding) {
		try {
			Bean bean = view.getCurrentBean().getBean();
			String bizModule = bean.getBizModule();
			String bizDocument = bean.getBizDocument();

			// Find the target metadata
			Customer customer = CORE.getUser().getCustomer();
			Module module = customer.getModule(bizModule);
			Document document = module.getDocument(customer, bizDocument);
			TargetMetaData targetMetaData = Binder.getMetaDataForBinding(customer, module, document, binding);
			Attribute targetAttribute = targetMetaData.getAttribute();
			Document targetDocument = targetMetaData.getDocument();

			if ((targetDocument != null) && (targetAttribute != null)) {
				// Find the owning bean (if we have a compound binding)
				Bean owningBean = bean;
				int lastDotIndex = binding.lastIndexOf('.');
				if (lastDotIndex > 0) {
					owningBean = (Bean) Binder.get(bean, binding.substring(0, lastDotIndex));
				}

				// Find the domain values for the list membership binding
				DomainType domainType = targetAttribute.getDomainType();
				List<DomainValue> source = ((DocumentImpl) targetDocument).getDomainValues((CustomerImpl) customer,
																							domainType, 
																							targetAttribute,
																							owningBean);

				// If the domain value exists in the target collection, 
				// move it from the source to the target in the DualListModel
				List<DomainValue> target = new ArrayList<>(10);
				Iterator<DomainValue> i = source.iterator();
				while (i.hasNext()) {
					DomainValue domainValue = i.next();
					Bean element = Binder.getElementInCollection(bean, binding, domainValue.getCode());
					if (element != null) {
						i.remove();
						target.add(domainValue);
					}
				}

				// put in the new list model
				put(binding, new DualListModel<>(source, target));
			}
			else { // can't find metadata, put in an empty list
				put(binding, new DualListModel<>(Collections.emptyList(), Collections.emptyList()));
			}
		}
		catch (Exception e) {
			throw new FacesException("Could not scatter list membership " + binding, e);
		}
	}

	/**
	 * Effect the changes required to keep the model lists and the domain model collection in sync.
	 */
	public void gather() {
		Bean bean = view.getCurrentBean().getBean();
		Persistence p = CORE.getPersistence();
		Customer c = p.getUser().getCustomer();
		Module m = c.getModule(bean.getBizModule());
		Document d = m.getDocument(c, bean.getBizDocument());
		
		for (String binding : keySet()) {
			try {
				DualListModel<DomainValue> model = get(binding);
				@SuppressWarnings("unchecked")
				List<Bean> collection = (List<Bean>) Binder.get(bean, binding);
				// collection could be null if we have a list membership that is bound to a compound binding...
				// eg <listMembership binding="foo.bars" />
				// Now, if foo is made null by another control - maybe a combo empty value is chosen then
				// 1) foo is null
				// 2) foo.bars yields null and we don't need to apply any processing
				if (collection != null) {
					TargetMetaData tmd = Binder.getMetaDataForBinding(c, m, d, binding);
					Relation r = (Relation) tmd.getAttribute();
					Document rd = m.getDocument(c, r.getDocumentName());

					// process the existing beans applying the changes and ordering from the model target
					int newIndex = 0;
					for (DomainValue domainValue : model.getTarget()) {
						String thisBizId = domainValue.getCode();
						Bean thisBean = BindUtil.getElementInCollection(collection, thisBizId);
						if (thisBean == null) { // DNE in collection
							thisBean = WebUtil.findReferencedBean(rd, thisBizId, p);
							collection.add(newIndex, thisBean);
						}
						else { // found in collection
							// Only move the bean in the collection if required
							// NB We do this conditionally so we don't upset hibernate collection dirtiness
							if (collection.indexOf(thisBean) != newIndex) {
								collection.remove(thisBean);
								collection.add(newIndex, thisBean);
							}
						}
						newIndex++;
					}
					// delete any left over beans in the list as these were not present in the model target
					while (collection.size() > newIndex) {
						collection.remove(newIndex);
					}

					// Sort the collection if required
					BindUtil.sortCollectionByMetaData(bean, c, m, d, binding);
					// Ensure that a new model is recreated with the correct sorting & domain values etc
					remove(binding);
				}
			}
			catch (Exception e) {
				throw new FacesException("Could not gather list membership " + binding, e);
			}
		}
	}
}
