package org.skyve.metadata.view.model.comparison;

import java.util.LinkedHashMap;
import java.util.Map;

import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.view.model.comparison.ComparisonComposite.Mutation;
import org.skyve.util.BeanVisitor;
import org.skyve.util.Binder;

public class DefaultBindingComparisonModel <T extends Bean, C extends Bean> extends ComparisonModel<T, C> {
	private Customer customer;
	private Document document;
	private C toCompareTo;
	private String[] excludedBindingPrefixes;
	
	public DefaultBindingComparisonModel(Document document,
											C toCompareTo,
											String[] excludedBindingPrefixes) {
		this.document = document;
		this.toCompareTo = toCompareTo;
		this.excludedBindingPrefixes = excludedBindingPrefixes;
	}
	
	@Override
	public void postConstruct(@SuppressWarnings("hiding") Customer customer, boolean runtime) {
		this.customer = customer;
	}
	
	@Override
	public ComparisonComposite getComparisonComposite(C boundBean)
	throws Exception {
		final Map<String, ComparisonComposite> bindingToNodes = new LinkedHashMap<>();
		
		// Visit the new bean and add in the model structure
		new BeanVisitor(false, false) {
			@Override
			protected boolean accept(String binding,
										Document currentDocument,
										Document owningDocument,
										Relation owningRelation,
										Bean bean)
			throws Exception {
				// stop recursive processing if we have matched an exclusion
				if (excluded(binding)) {
					return false;
				}
				
//System.out.println("OB = " + binding + " -> " + bean);
				bindingToNodes.put(binding, createNode(binding, owningRelation, currentDocument, bean, true));

				return true;
			}
		}.visit(document, boundBean, customer);

		// Visit oldBean and add/modify the resulting model.
		new BeanVisitor(false, false, false) {
			@Override
			protected boolean accept(String binding,
										Document currentDocument,
										Document owningDocument,
										Relation owningRelation,
										Bean bean)
			throws Exception {
				// stop recursive processing if we have matched an exclusion
				if (excluded(binding)) {
					return false;
				}
				
				ComparisonComposite node = bindingToNodes.get(binding);
				if (node == null) { // deleted entry
					bindingToNodes.put(binding, createNode(binding, owningRelation, currentDocument, bean, false));
				}
				else { // existing entry
					updateNode(bean, node);
				}

				return true;
			}
		}.visit(document, toCompareTo, customer);

		ComparisonComposite result = null;
		
		for (String binding : bindingToNodes.keySet()) {
			ComparisonComposite node = bindingToNodes.get(binding);
			if ("".equals(binding)) {
				result = node;
			}
			else {
				int lastDotIndex = binding.lastIndexOf('.');
				if (lastDotIndex > 0) {
					String parentBinding = binding.substring(0, lastDotIndex);
					bindingToNodes.get(parentBinding).getChildren().add(node);
				}
				else {
					if (result != null) {
						result.getChildren().add(node);
					}
				}
			}
		}
		
		return result;
	}
	
	private ComparisonComposite createNode(String binding,
											Relation owningRelation,
											Document currentDocument,
											Bean bean,
											boolean newNode)
	throws Exception {
		ComparisonComposite result = new ComparisonComposite();
		result.setBizId(bean.getBizId());
		result.setBusinessKeyDescription((bean instanceof PersistentBean) ? 
											((PersistentBean) bean).getBizKey() : 
											currentDocument.getLocalisedSingularAlias());
		result.setDocument(currentDocument);
		if (owningRelation == null) {
			result.setReferenceName(null);
			result.setRelationshipDescription(currentDocument.getLocalisedSingularAlias());
		}
		else {
			result.setReferenceName(owningRelation.getName());
			result.setRelationshipDescription(owningRelation.getLocalisedDisplayName());
		}
		result.setMutation(newNode ? Mutation.added : Mutation.deleted);
		addProperties(result, currentDocument, bean, newNode, binding);

		return result;
	}

	// fill in the oldValues JSON property for this entry
	// determine if there are differences between the old and new and indicate these
	private static void updateNode(Bean bean, ComparisonComposite node)
	throws Exception {
		boolean nodeDirty = false;

		for (ComparisonProperty property : node.getProperties()) {
			Object oldValue = Binder.get(bean, property.getName());
			property.setOldValue(oldValue);

			if ((! nodeDirty) && property.isDirty()) {
				nodeDirty = true;
			}
		}

		node.setMutation(nodeDirty ? Mutation.updated : Mutation.unchanged);
	}

	private void addProperties(ComparisonComposite node,
								Document beanDocument,
								Bean bean,
								boolean newEntry,
								String binding)
	throws Exception {
		// Get any inherited attributes here too.
		for (Attribute attribute : beanDocument.getAllAttributes(customer)) {
			String fqAttributeBinding = null;
			if (binding.isEmpty()) {
				fqAttributeBinding = attribute.getName();
			}
			else {
				fqAttributeBinding = new StringBuilder(128).append(binding).append('.').append(attribute.getName()).toString();
			}
			if ((! (attribute instanceof Relation)) && (! excluded(fqAttributeBinding))) {
				ComparisonProperty property = new ComparisonProperty();
				String name = attribute.getName();
				property.setName(name);
				property.setTitle(attribute.getLocalisedDisplayName());
				property.setWidget(attribute.getDefaultInputWidget());

				Object value = Binder.get(bean, name);
				property.setNewValue(newEntry ? value : null);
				property.setOldValue(newEntry ? null : value);

				node.getProperties().add(property);
			}
		}
	}

	public boolean excluded(String binding) {
		if (excludedBindingPrefixes != null) {
			String unindexedBinding = binding.replaceAll("\\[[0-9]*\\]", "");
			for (String excludedBindingPrefix : excludedBindingPrefixes) {
				if (unindexedBinding.startsWith(excludedBindingPrefix)) {
					return true;
				}
			}
		}
		
		return false;
	}
}
