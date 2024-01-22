package org.skyve.impl.bind;

import java.util.ArrayList;
import java.util.List;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.domain.HierarchicalBean;
import org.skyve.domain.PersistentBean;
import org.skyve.impl.metadata.model.document.InverseMany;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Collection;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.module.Module;
import org.skyve.util.Binder.TargetMetaData;
import org.skyve.util.ExpressionEvaluator;

abstract class MetaDataExpressionEvaluator extends ExpressionEvaluator {
	@SuppressWarnings("static-method")
	protected Attribute obtainAttribute(String expression, Bean bean) {
		Customer c = CORE.getCustomer();
		Module m = c.getModule(bean.getBizModule());
		Document d = m.getDocument(c, bean.getBizDocument());
		TargetMetaData target = BindUtil.getMetaDataForBinding(c, m, d, expression);
		if (target != null) {
			return target.getAttribute();
		}
		return null;
	}

	@Override
	public String validateWithoutPrefixOrSuffix(String expression,
													Class<?> returnType,
													Customer customer,
													Module module,
													Document document) {
		if (customer == null) {
			throw new IllegalArgumentException("customer can't be null for a binding expression");
		}
		if (module == null) {
			throw new IllegalArgumentException("module can't be null for a binding expression");
		}
		if (document == null) {
			throw new IllegalArgumentException("document can't be null for a binding expression");
		}

		String error = null;
		try {
			TargetMetaData target = BindUtil.validateBinding(customer, module, document, expression);
			Attribute attribute = target.getAttribute();
			Class<?> type = target.getType();
			if ((returnType != null) && (! returnType.isAssignableFrom(type))) {
				if (attribute == null) { // implicit type or condition
					if (Boolean.class.equals(type)) { // condition)
						throw new MetaDataException("Binding " + expression + " resolves to a condition that is incompatible with required type of " + returnType);
					}
					throw new MetaDataException("Binding " + expression + " resolves to implicit attribute of type " + type + 
													" that is incompatible with required type of " + returnType);
				}
				throw new MetaDataException("Binding " + expression + " resolves to an attribute of type " + type + 
												" that is incompatible with required type of " + returnType);
			}
		}
		catch (MetaDataException e) {
			error = e.getLocalizedMessage();
		}
		return error;
	}
	
	@Override
	public List<String> completeWithoutPrefixOrSuffix(String fragment,
														Customer customer,
														Module module,
														Document document) {
		List<String> result = new ArrayList<>();
		
		Document targetDocument = null;
		String simpleBindingFragment = fragment;
		String bindingPrefix = "";
		
		if (fragment == null) { // handle null fragment
			targetDocument = document;
		}
		else {
			// Check for compound binding prefixes
			int lastDotIndex = fragment.lastIndexOf('.');
			int lastOpeningSquareBracketIndex = fragment.lastIndexOf('[');
			int lastClosingSquareBracketIndex = fragment.lastIndexOf(']');
			int lastDelimiterIndex = Math.max(lastDotIndex, Math.max(lastOpeningSquareBracketIndex, lastClosingSquareBracketIndex));
			if (lastDelimiterIndex < 0) { // not compound
				targetDocument = document;
			}
			else { // compound
				bindingPrefix = fragment.substring(0, lastDelimiterIndex);
				simpleBindingFragment = (lastDelimiterIndex == (fragment.length() - 1)) ? 
											null : 
											fragment.substring(lastDelimiterIndex + 1);

				if (! bindingPrefix.isEmpty()) { // fragment = "." or "[" or "]"
					try {
						TargetMetaData target = BindUtil.getMetaDataForBinding(customer, module, document, bindingPrefix);
						if (target != null) {
							int lastBindingPrefixDotIndex = bindingPrefix.lastIndexOf('.');
							String lastAttributeBinding = (lastBindingPrefixDotIndex > 0) ?
															bindingPrefix.substring(lastBindingPrefixDotIndex + 1) :
															bindingPrefix;
							targetDocument = target.getDocument();
							Attribute targetAttribute = target.getAttribute();
							if (targetAttribute instanceof Relation) {
								Module owningModule = customer.getModule(targetDocument.getOwningModuleName());
								String relatedDocumentName = ((Relation) targetAttribute).getDocumentName();
								targetDocument = owningModule.getDocument(customer, relatedDocumentName);

								if ((targetAttribute instanceof Collection) ||
										(targetAttribute instanceof InverseMany)) {
									// If opening square bracket
									if (lastDelimiterIndex == lastOpeningSquareBracketIndex) {
										// If nothing after the opening square bracket, offer to close it
										if (simpleBindingFragment == null) {
											result.add(bindingPrefix + "[0]");
											result.add(bindingPrefix + "[1]");
											result.add(bindingPrefix + "[2]");
											result.add(bindingPrefix + "[3]");
											result.add(bindingPrefix + "[4]");
											result.add(bindingPrefix + "[5]");
											result.add(bindingPrefix + "[6]");
											result.add(bindingPrefix + "[7]");
											result.add(bindingPrefix + "[8]");
											result.add(bindingPrefix + "[9]");
										}
										// If something and its an integer, offer to close it
										else {
											try {
												Integer.parseInt(simpleBindingFragment);
												result.add(fragment + ']');
											}
											catch (@SuppressWarnings("unused") NumberFormatException e) {
												// nothing to do, the binding expression is malformed and can't be completed
											}
										}
										targetDocument = null; // complete the collection index notation and no more
									}
									// if its a close collection index expression followed by a '.' - eg "[0]."
									else if ((lastDelimiterIndex == lastDotIndex) && 
												(lastClosingSquareBracketIndex == (lastDelimiterIndex - 1))) {
										bindingPrefix += '.';
									}
									// its invalid
									else {
										targetDocument = null; // '.' & ']' are errors
									}
								}
								else {
									bindingPrefix += '.';
								}
							}
							else if (targetAttribute != null) { // scalar attribute - nothing more to complete
								targetDocument = null;
							}
							else if (BindUtil.isImplicit(lastAttributeBinding)) { // implicit attribute - nothing more to complete
								targetDocument = null;
							}
						}
					}
					catch (@SuppressWarnings("unused") Exception e) {
						// nothing to do, the binding expression is malformed and can't be completed
					}
				}
			}
		}
		
		if (targetDocument != null) {
			addAttributesAndConditions(bindingPrefix, simpleBindingFragment, customer, targetDocument, result);
		}
		
		return result;
	}
	
	static void addAttributesAndConditions(String bindingPrefix,
											String simpleBindingFragment,
											Customer customer,
											Document document,
											List<String> completions) {
		// Check document attributes
		for (Attribute a : document.getAllAttributes(customer)) {
			String name = a.getName();
			if ((simpleBindingFragment == null) || name.startsWith(simpleBindingFragment)) {
				completions.add(bindingPrefix + name);
			}
		}
		
		// Check conditions
		for (String name : document.getConditionNames()) {
			if (! (Bean.CREATED_KEY.equals(name) || Bean.PERSISTED_KEY.equals(name))) {
				if ((simpleBindingFragment == null) || name.startsWith(simpleBindingFragment)) {
					completions.add(bindingPrefix + name);
					completions.add(bindingPrefix + BindUtil.negateCondition(name));
				}
			}
		}

		// Check implicit bindings
		if ((simpleBindingFragment == null) || Bean.DOCUMENT_ID.startsWith(simpleBindingFragment)) {
			completions.add(bindingPrefix + Bean.DOCUMENT_ID);
		}
		if ((simpleBindingFragment == null) || Bean.BIZ_KEY.startsWith(simpleBindingFragment)) {
			completions.add(bindingPrefix + Bean.BIZ_KEY);
		}
		if ((simpleBindingFragment == null) || Bean.CHANGED_KEY.startsWith(simpleBindingFragment)) {
			completions.add(bindingPrefix + Bean.CHANGED_KEY);
		}
		if ((simpleBindingFragment == null) || Bean.NOT_CHANGED_KEY.startsWith(simpleBindingFragment)) {
			completions.add(bindingPrefix + Bean.NOT_CHANGED_KEY);
		}
		if ((simpleBindingFragment == null) || Bean.PERSISTED_KEY.startsWith(simpleBindingFragment)) {
			completions.add(bindingPrefix + Bean.PERSISTED_KEY);
		}
		if ((simpleBindingFragment == null) || Bean.NOT_PERSISTED_KEY.startsWith(simpleBindingFragment)) {
			completions.add(bindingPrefix + Bean.NOT_PERSISTED_KEY);
		}
		if ((simpleBindingFragment == null) || Bean.CUSTOMER_NAME.startsWith(simpleBindingFragment)) {
			completions.add(bindingPrefix + Bean.CUSTOMER_NAME);
		}
		if ((simpleBindingFragment == null) || Bean.DATA_GROUP_ID.startsWith(simpleBindingFragment)) {
			completions.add(bindingPrefix + Bean.DATA_GROUP_ID);
		}
		if ((simpleBindingFragment == null) || Bean.USER_ID.startsWith(simpleBindingFragment)) {
			completions.add(bindingPrefix + Bean.USER_ID);
		}
		
		String parentDocumentName = document.getParentDocumentName();
		if (parentDocumentName != null) {
			if (parentDocumentName.equals(document.getName())) { // hierarchical
				if ((simpleBindingFragment == null) || HierarchicalBean.PARENT_ID.startsWith(simpleBindingFragment)) {
					completions.add(bindingPrefix + HierarchicalBean.PARENT_ID);
				}
			}
			else {
				if ((simpleBindingFragment == null) || ChildBean.PARENT_NAME.startsWith(simpleBindingFragment)) {
					completions.add(bindingPrefix + ChildBean.PARENT_NAME);
				}
			}
		}
		
		if (document.isPersistable()) {
			if ((simpleBindingFragment == null) || PersistentBean.VERSION_NAME.startsWith(simpleBindingFragment)) {
				completions.add(bindingPrefix + PersistentBean.VERSION_NAME);
			}
			if ((simpleBindingFragment == null) || PersistentBean.LOCK_NAME.startsWith(simpleBindingFragment)) {
				completions.add(bindingPrefix + PersistentBean.LOCK_NAME);
			}
			if ((simpleBindingFragment == null) || PersistentBean.TAGGED_NAME.startsWith(simpleBindingFragment)) {
				completions.add(bindingPrefix + PersistentBean.TAGGED_NAME);
			}
			if ((simpleBindingFragment == null) || PersistentBean.FLAG_COMMENT_NAME.startsWith(simpleBindingFragment)) {
				completions.add(bindingPrefix + PersistentBean.FLAG_COMMENT_NAME);
			}
		}
	}
}
