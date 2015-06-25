package org.skyve.wildcat.util;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.domain.messages.DomainException;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Extends;
import org.skyve.metadata.model.document.Collection;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Reference;
import org.skyve.metadata.module.Module;
import org.skyve.wildcat.bind.BindUtil;

public abstract class BeanVisitor {
	/**
	 * Visit a bean's references including references that are not defined. 
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
	 * Visit a bean excluding references that are not null. 
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
						Reference owningReference,
						Bean bean,
						Customer customer,
						Set<Bean> visitedBeans,
						boolean visitNulls,
						boolean visitingInheritedDocument) 
	throws DomainException, MetaDataException {
		if (! visitingInheritedDocument) {
			if (bean == null) {
				if (owningReference != null) {
					String owningReferenceName = owningReference.getName();
					String bindingWithoutThisReferenceName = binding.substring(0, binding.length() - owningReferenceName.length());
					// have we seen a reference with this name before
					int index = bindingWithoutThisReferenceName.lastIndexOf(owningReferenceName);
					if (index >= 0) {
						index += owningReferenceName.length() + 1;
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
			if (accept(binding, document, owningDocument, owningReference, bean, visitingInheritedDocument)) {
				for (String referenceName : document.getReferenceNames()) {
					Document referencedDocument = document.getReferencedDocument(customer, referenceName);
					Reference childReference = document.getReferenceByName(referenceName);
					if (childReference instanceof Collection) {
						@SuppressWarnings("unchecked")
						List<Bean> children = (bean == null) ? null : (List<Bean>) BindUtil.get(bean, referenceName);
						if (children != null) {
							int i = 0;
							for (Bean child : children) {
								if (child != null) {
									sb.setLength(0);
									if (binding.length() != 0) {
										sb.append(binding).append('.');
									}
									sb.append(referenceName).append('[').append(i).append(']');
									visit(sb.toString(), 
											referencedDocument, 
											document, 
											childReference, 
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
								sb.append(referenceName);
								visit(sb.toString(), 
										referencedDocument,
										document, 
										childReference, 
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
							sb.append(referenceName);
							visit(sb.toString(), 
									referencedDocument,
									document, 
									childReference, 
									null, 
									customer, 
									visitedBeans, 
									visitNulls,
									false);
						}
					}
					else {
						Bean child = (bean == null) ? null : (Bean) BindUtil.get(bean, referenceName);
						if ((child != null) || visitNulls) {
							sb.setLength(0);
							if (binding.length() != 0) {
								sb.append(binding).append('.');
							}
							sb.append(referenceName);
							visit(sb.toString(), 
									referencedDocument,
									document, 
									childReference, 
									child, 
									customer, 
									visitedBeans, 
									visitNulls,
									false);
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
					visit(binding, baseDocument, owningDocument, owningReference, bean, customer, visitedBeans, visitNulls, true);
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
	 * @param owningReference The owning document's reference that got us here (by recursion)
	 * @param bean
	 * @return <code>false</code> to terminate, <code>true</code> to continue.
	 * @throws Exception
	 */
	protected abstract boolean accept(String binding,
										Document document,
										Document owningDocument,
										Reference owningReference,
										Bean bean,
										boolean visitingInheritedDocument) 
	throws Exception;
}
