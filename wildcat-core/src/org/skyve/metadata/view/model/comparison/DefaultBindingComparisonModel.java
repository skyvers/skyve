package org.skyve.metadata.view.model.comparison;

import java.util.LinkedHashMap;
import java.util.Map;

import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Reference;
import org.skyve.metadata.view.model.comparison.ComparisonComposite.Mutation;
import org.skyve.wildcat.bind.BindUtil;
import org.skyve.wildcat.util.BeanVisitor;

public class DefaultBindingComparisonModel <T extends Bean, C extends Bean> extends ComparisonModel<T, C> {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -256924803932268240L;

	private Customer customer;
	private Document document;
	private C toCompareTo;
	private String[] excludedBindingPrefixes;
	
	public DefaultBindingComparisonModel(Customer customer,
											Document document,
											C toCompareTo,
											String[] excludedBindingPrefixes) {
		this.customer = customer;
		this.document = document;
		this.toCompareTo = toCompareTo;
		this.excludedBindingPrefixes = excludedBindingPrefixes;
	}
	
	@Override
	public ComparisonComposite getComparisonComposite(C boundBean)
	throws Exception {
		final Map<String, ComparisonComposite> bindingToNodes = new LinkedHashMap<>();
		
		// Visit the new bean and add in the model structure
		new BeanVisitor() {
			@Override
			@SuppressWarnings("synthetic-access")
			protected boolean accept(String binding,
										Document currentDocument,
										Document owningDocument,
										Reference owningReference,
										Bean bean,
										boolean visitingInheritedDocument)
			throws Exception {
				// stop recursive processing if we have matched an exclusion
				if (excluded(binding)) {
					return false;
				}
				
//System.out.println("OB = " + binding + " -> " + bizId);
				bindingToNodes.put(binding, createNode(binding, owningReference, currentDocument, bean, true));

				return true;
			}
		}.visit(document, boundBean, customer);

		// Visit oldBean and add/modify the resulting model.
		new BeanVisitor() {
			@Override
			@SuppressWarnings("synthetic-access")
			protected boolean accept(String binding,
										Document currentDocument,
										Document owningDocument,
										Reference owningReference,
										Bean bean,
										boolean visitingInheritedDocument)
			throws Exception {
				// stop recursive processing if we have matched an exclusion
				if (excluded(binding)) {
					return false;
				}
				
				ComparisonComposite node = bindingToNodes.get(binding);
				if (node == null) { // deleted entry
					bindingToNodes.put(binding, createNode(binding, owningReference, currentDocument, bean, false));
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
											Reference owningReference,
											Document currentDocument,
											Bean bean,
											boolean newNode)
	throws Exception {
		ComparisonComposite result = new ComparisonComposite();
		result.setBizId(bean.getBizId());
		result.setBusinessKeyDescription((bean instanceof PersistentBean) ? 
											((PersistentBean) bean).getBizKey() : 
											currentDocument.getSingularAlias());
		result.setDocument(currentDocument);
		if (owningReference == null) {
			result.setReferenceName(null);
			result.setRelationshipDescription(currentDocument.getSingularAlias());
		}
		else {
			result.setReferenceName(owningReference.getName());
			result.setRelationshipDescription(owningReference.getDisplayName());
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
			Object oldValue = BindUtil.get(bean, property.getName());
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
		for (Attribute attribute : beanDocument.getAttributes()) {
			String fqAttributeBinding = null;
			if (binding.isEmpty()) {
				fqAttributeBinding = attribute.getName();
			}
			else {
				fqAttributeBinding = new StringBuilder(128).append(binding).append('.').append(attribute.getName()).toString();
			}
			if ((! (attribute instanceof Reference)) && (! excluded(fqAttributeBinding))) {
				ComparisonProperty property = new ComparisonProperty();
				String name = attribute.getName();
				property.setName(name);
				property.setTitle(attribute.getDisplayName());
				property.setWidget(attribute.getDefaultInputWidget());

				Object value = BindUtil.get(bean, name);
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
