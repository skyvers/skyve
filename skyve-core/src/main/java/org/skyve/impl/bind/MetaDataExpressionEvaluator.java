package org.skyve.impl.bind;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Extends;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.module.Module;
import org.skyve.util.Binder.TargetMetaData;

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
	public String validateWithoutPrefix(String expression,
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

		Document contextDocument = document;
		String ultimateBinding = expression;
		int lastDotIndex = expression.lastIndexOf('.');
		if (lastDotIndex > 0) {
			String penultimateBinding = expression.substring(0, lastDotIndex);
			ultimateBinding = expression.substring(lastDotIndex + 1);
			try {
				TargetMetaData target = BindUtil.getMetaDataForBinding(customer, module, document, penultimateBinding);
				if (target == null) {
					error = "Binding " + penultimateBinding + " does not resolve to a relation.";
				}
				else {
					Attribute relation = target.getAttribute();
					if (relation instanceof Relation) {
						String contextDocumentName = ((Relation) relation).getDocumentName();
						contextDocument = module.getDocument(customer, contextDocumentName);
					}
					else {
						if (ChildBean.PARENT_NAME.equals(penultimateBinding) || penultimateBinding.endsWith(ChildBean.CHILD_PARENT_NAME_SUFFIX)) {
							contextDocument = target.getDocument();
							contextDocument = contextDocument.getParentDocument(customer);
						}
						else {
							error = "Binding " + penultimateBinding + " does not resolve to a relation.";
						}
					}
				}
			}
			catch (Exception e) {
				error = e.getMessage();
				if (error == null) {
					error = "Binding " + penultimateBinding + " does not resolve to a document attribute.";
				}
				else {
					error = "Binding " + penultimateBinding + " does not resolve to a document attribute: " + error;
				}
			}
		}
		
		if (contextDocument.getCondition(ultimateBinding) != null) {
			if ((returnType != null) && (! returnType.isAssignableFrom(Boolean.class))) {
				error = "Binding " + expression + " resolves to a boolean condition that is incompatible with required type of " + returnType;
			}
		}
		else {
			Class<?> implicitClass = BindUtil.implicitAttributeType(ultimateBinding);
			if (implicitClass != null) {
				if ((returnType != null) && (! returnType.isAssignableFrom(implicitClass))) {
					error = "Binding " + expression + " resolves to implicit attribute " + ultimateBinding + 
								" of type " + implicitClass + 
								" that is incompatible with required type of " + returnType;
				}
			}
			else {
				// Check the document hierarchy for the attribute
				Attribute attribute = contextDocument.getAttribute(ultimateBinding);
				if (attribute == null) {
					Extends inherits = contextDocument.getExtends();
					while (inherits != null) {
						contextDocument = customer.getModule(contextDocument.getOwningModuleName()).getDocument(customer, inherits.getDocumentName());
						attribute = contextDocument.getAttribute(ultimateBinding);
						if (attribute == null) {
							inherits = contextDocument.getExtends();
						}
						else {
							inherits = null;
						}
					}
				}

				if (attribute == null) {
					error = "Binding " + expression + " does not resolve to a document attribute, condition or an implicit attribute.";
				}
				else {
					if (returnType != null) {
						Class<?> attributeClass = attribute.getAttributeType().getImplementingType();
						if (! returnType.isAssignableFrom(attributeClass)) {
							error = "Binding " + expression + " resolves to an attribute of type " + attributeClass + 
										" that is incompatible with required type of " + returnType;
						}
					}
				}
			}
		}

		return error;
	}
}
