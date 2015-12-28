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
import org.skyve.metadata.model.document.Association;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Relation;
import org.skyve.wildcat.bind.BindUtil;
import org.skyve.wildcat.metadata.model.document.Inverse;

public abstract class BeanVisitor {
	private boolean visitNulls;
	private boolean visitInverses;
	private boolean vectorCyclicDetection;
	
	/**
	 * Visit the structure of a bean (and it's related graph).
	 * 
	 * @param visitNulls	Visit a bean's relations including relations that are not defined.
	 * 						In this method call, the bean can be null, which will visit all bindings defined in the document.
	 * 						If a component instance is null then the document metadata is used for traversal.
	 * @param visitInverses	Visit any inverses defined in the document.
	 * 						This option is available as inverses are weakly referenced (not validated, not cascaded).
	 * @param vectorCyclicDetection	Cyclic dependencies are detected by keeping a breadcrumb list of beans visited.
	 * 								If the bean has been visited before, it is not visited again.
	 * 								This guarantees that a bean is visited ONLY ONCE.
	 * 								But some processing types need to know where the bean was traversed from.
	 * 								It then keeps a vector of the bean and its traversal direction.
	 * 								eg visited contact "mike" from User "mike" through the association "contact".
	 * 								In vector mode, a bean may be visited MORE THAN ONCE if it has multiple references
	 * 								within the same object graph.
	 * 
	 * This visit method is thread-safe - the same instance can be used in multiple threads.
	 */
	public BeanVisitor(boolean visitNulls,
						boolean visitInverses,
						boolean vectorCyclicDetection) {
		this.visitNulls = visitNulls;
		this.visitInverses = visitInverses;
		this.vectorCyclicDetection = vectorCyclicDetection;
	}
	
	/**
	 * Visit a bean excluding relations that are null. 
	 * This method is thread-safe.
	 * 
	 * @param document
	 * @param bean
	 * @param customer
	 * @throws DomainException
	 * @throws MetaDataException
	 */
	public void visit(Document document, 
						Bean bean, 
						Customer customer)
	throws DomainException, MetaDataException {
		Set<String> visited = new HashSet<>();
		visit("", document, null, null, bean, customer, visited);
	}

	private String determineVisitedKey(Bean bean, 
										Document owningDocument,
										Relation owningRelation) {
		if (vectorCyclicDetection) {
			StringBuilder sb = new StringBuilder(128).append(bean.getBizId());
			if (owningRelation != null) {
				sb.append(owningRelation.getName());
			}
			if (owningDocument != null) {
				sb.append(owningDocument.getName()).append(owningDocument.getOwningModuleName());
			}
			return sb.toString();
		}
		return bean.getBizId();
	}
	
	private void visit(String binding,
						Document document,
						Document owningDocument,
						Relation owningRelation,
						Bean bean,
						Customer customer,
						Set<String> visited) 
	throws DomainException, MetaDataException {
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
			String key = determineVisitedKey(bean, owningDocument, owningRelation);
			if (visited.contains(key)) {
				return;
			}
			visited.add(key);
		}
		
		StringBuilder sb = new StringBuilder(64);
		try {
			if (accept(binding, document, owningDocument, owningRelation, bean)) {
				// NB visit relations in the order they are defined in the documents.
				for (Attribute attribute : document.getAllAttributes()) {
					if (attribute instanceof Relation) {
						if ((! visitInverses) && (attribute instanceof Inverse)) {
							continue;
						}

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
										visited);
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
												visited);
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
											visited);
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
										visited);
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
								visited);
					}
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
										Bean bean) 
	throws Exception;
}
