package org.skyve.wildcat.util;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.domain.messages.DomainException;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Extends;
import org.skyve.metadata.model.document.Association;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.module.Module;
import org.skyve.wildcat.bind.BindUtil;

public abstract class BeanVisitor {
	/**
	 * Visit a bean's relations including relations that are not defined. 
	 * In this method call, the bean can be null, which will visit all bindings defined in the document. 
	 * Each component instance is visited ONCE, if a component instance is null 
	 * then the document metadata is used for traversal.
	 * 
	 * @param document The document to visit.
	 * @param bean The bean instance to visit, can be null.
	 * @param customer
	 * @throws DomainException
	 * @throws MetaDataException
	 */
	public void visitAll(Document document, Bean bean, Customer customer) 
	throws DomainException, MetaDataException {
		Set<Bean> visitedBeans = new HashSet<>();
		visit("", document, null, null, bean, customer, visitedBeans, true, false);
	}

	/**
	 * Visit a bean excluding relations that are null. 
	 * Each instance is visited ONCE.
	 * 
	 * @param document
	 * @param bean
	 * @param customer
	 * @throws DomainException
	 * @throws MetaDataException
	 */
	public void visit(Document document, Bean bean, Customer customer)
	throws DomainException, MetaDataException {
		Set<Bean> visitedBeans = new HashSet<>();
		visit("", document, null, null, bean, customer, visitedBeans, false, false);
	}

	private void visit(String binding,
						Document document,
						Document owningDocument,
						Relation owningRelation,
						Bean bean,
						Customer customer,
						Set<Bean> visitedBeans,
						boolean visitNulls,
						boolean visitingInheritedDocument) 
	throws DomainException, MetaDataException {
		if (! visitingInheritedDocument) {
			if (bean == null) {
				if (owningRelation != null) {
					String owningRelationName = owningRelation.getName();
					String bindingWithoutThisRelationName = binding.substring(0, binding.length() - owningRelationName.length());
					// have we seen a relation with this name before
					int index = bindingWithoutThisRelationName.lastIndexOf(owningRelationName);
					if (index >= 0) {
						index += owningRelationName.length() + 1;
						String potentialCircularBinding = binding.substring(index) + '.';
						if ((index - potentialCircularBinding.length()) >= 0) { // enough in the binding to create the prior binding
							String bindingPriorToPotentialCircularBinding = binding.substring(index - potentialCircularBinding.length(), index);
							if (bindingPriorToPotentialCircularBinding.equals(potentialCircularBinding)) {
								return;
							}
						}
					}
				}
			}
			else {
				if (visitedBeans.contains(bean)) {
					return;
				}
				visitedBeans.add(bean);
			}
		}
		
		StringBuilder sb = new StringBuilder(64);
		try {
			if (accept(binding, document, owningDocument, owningRelation, bean, visitingInheritedDocument)) {
				// NB visit relations in the order they are defined in the document.
				for (Attribute attribute : document.getAttributes()) {
					if (attribute instanceof Relation) {
						String relationName = attribute.getName();
						Document referencedDocument = document.getRelatedDocument(customer, relationName);
						Relation childRelation = (Relation) attribute;
						if (childRelation instanceof Association) {
							Bean child = (bean == null) ? null : (Bean) BindUtil.get(bean, relationName);
							if ((child != null) || visitNulls) {
								sb.setLength(0);
								if (binding.length() != 0) {
									sb.append(binding).append('.');
								}
								sb.append(relationName);
								visit(sb.toString(), 
										referencedDocument,
										document, 
										childRelation, 
										child, 
										customer, 
										visitedBeans, 
										visitNulls,
										false);
							}
						}
						else { // collection or inverse
							@SuppressWarnings("unchecked")
							List<Bean> children = (bean == null) ? null : (List<Bean>) BindUtil.get(bean, relationName);
							if (children != null) {
								int i = 0;
								for (Bean child : children) {
									if (child != null) {
										sb.setLength(0);
										if (binding.length() != 0) {
											sb.append(binding).append('.');
										}
										sb.append(relationName).append('[').append(i).append(']');
										visit(sb.toString(), 
												referencedDocument, 
												document, 
												childRelation, 
												child, 
												customer,
												visitedBeans, 
												visitNulls,
												false);
									}
									i++;
								}
								if ((i == 0) && visitNulls) { // no elements in the collection
									sb.setLength(0);
									if (binding.length() != 0) {
										sb.append(binding).append('.');
									}
									sb.append(relationName);
									visit(sb.toString(), 
											referencedDocument,
											document, 
											childRelation, 
											null, 
											customer, 
											visitedBeans, 
											visitNulls,
											false);
								}
							}
							else {
								sb.setLength(0);
								if (binding.length() != 0) {
									sb.append(binding).append('.');
								}
								sb.append(relationName);
								visit(sb.toString(), 
										referencedDocument,
										document, 
										childRelation, 
										null, 
										customer, 
										visitedBeans, 
										visitNulls,
										false);
							}
						}
					}
				}

				Document parentDocument = document.getParentDocument(customer); 
				if ((parentDocument != null) && 
						// child document, not a hierarchical document
						(! document.getName().equals(parentDocument.getName()))) {
					Bean parent = (bean == null) ? null : (Bean) BindUtil.get(bean, ChildBean.PARENT_NAME);
					if ((parent != null) || visitNulls) {
						sb.setLength(0);
						if (binding.length() != 0) {
							sb.append(binding).append('.');
						}
						sb.append(ChildBean.PARENT_NAME);
						visit(sb.toString(), 
								parentDocument,
								document, 
								null, 
								parent, 
								customer, 
								visitedBeans, 
								visitNulls,
								false);
					}
				}
				
				Extends inherits = document.getExtends();
				if (inherits != null) {
					Module module = customer.getModule(document.getOwningModuleName());
					Document baseDocument = module.getDocument(customer, inherits.getDocumentName());
					visit(binding, baseDocument, owningDocument, owningRelation, bean, customer, visitedBeans, visitNulls, true);
				}
			}
		}
		catch (MetaDataException e) {
			throw e;
		}
		catch (DomainException e) {
			throw e;
		}
		catch (Exception e) {
			throw new DomainException(e);
		}
	}

	/**
	 * Accept the reference.
	 * 
	 * @param binding
	 * @param document
	 * @param owningDocument The owning document that got us here (by recursion)
	 * @param owningRelation The owning document's relation that got us here (by recursion)
	 * @param bean
	 * @return <code>false</code> to terminate, <code>true</code> to continue.
	 * @throws Exception
	 */
	protected abstract boolean accept(String binding,
										Document document,
										Document owningDocument,
										Relation owningRelation,
										Bean bean,
										boolean visitingInheritedDocument) 
	throws Exception;
}
