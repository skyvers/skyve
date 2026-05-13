package org.skyve.util;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.messages.SkyveException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Association;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Inverse;
import org.skyve.metadata.model.document.Inverse.InverseCardinality;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.module.Module;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * This class provides the internals to visit a bean graph defined by its relations.
 * It can also visit through null relations by following the document metadata if activated.
 * If visitNulls() is called (by extend class constructors) it will delegate to acceptNulls()
 * rather than accept().
 */
abstract class BeanVisitorImpl {
	private boolean visitNulls = false;
	private boolean visitInverses;
	private boolean vectorCyclicDetection;
	private boolean acceptVisited;

	/**
	 * Make the visitor call acceptNulls() instead of accept().
	 */
	void visitNulls() {
		visitNulls = true;
	}
	
	/**
	 * Convenience constructor that enabled instance cyclic detection.
	 * See {{@link #BeanVisitor(boolean, boolean, boolean)}
	 */
	BeanVisitorImpl(boolean visitInverses, boolean vectorCyclicDetection) {
		this(visitInverses, vectorCyclicDetection, false);
	}

	/**
	 * Visit the structure of a bean (and it's related graph).
	 * 
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
	 * @param acceptVisited	Set true to continue to visit bean instances that have been visited before circumventing cyclic detection.
	 * 
	 * This visit method is thread-safe - the same instance can be used in multiple threads.
	 */
	BeanVisitorImpl(boolean visitInverses,
					   boolean vectorCyclicDetection,
					   boolean acceptVisited) {
		this.visitInverses = visitInverses;
		this.vectorCyclicDetection = vectorCyclicDetection;
		this.acceptVisited = acceptVisited;
	}
	
	/**
	 * Visit a bean.
	 * This method is thread-safe.
	 * 
	 * @param document Document of the bean.
	 * @param bean	The bean to visit.
	 * @param customer	The current customer.
	 */
	public void visit(@Nonnull Document document, 
						@Nullable Bean bean, 
						@Nonnull Customer customer) {
		Set<String> visited = new HashSet<>();
		visit("", document, null, null, bean, customer, visited);
	}

	/**
	 * Recursively visit each bean (or when nulls, the document) honoring the constructor parameters.
	 * 
	 * @param binding	Binding of this visit with respect to the root bean.
	 * @param document	The document being visited.
	 * @param owningDocument	The document that owns this relation being visited. Null for top level root bean.
	 * @param owningRelation	The relation being visited. Null for top level root bean.
	 * @param bean	The bean being visited.
	 * @param customer	The current customer.
	 * @param visited	The set of breadcrumbs that have been visited in the past (for cyclic detection)
	 */
	private void visit(@Nonnull String binding,
						@Nonnull Document document,
						@Nullable Document owningDocument,
						@Nullable Relation owningRelation,
						@Nullable Bean bean,
						@Nonnull Customer customer,
						@Nonnull Set<String> visited) {
		// If we have no bean, still do cyclic detection
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
		// If we have turned off cyclic detection (acceptVisited is true), call the accept method anyways, and bug out
		// NB We still add the key to our breadcrumb in case subsequent beans to visit are null.
		else {
			String key = determineVisitedKey(bean, owningDocument, owningRelation);
			if (visited.contains(key)) {
				if (acceptVisited) {
					try {
						accept(binding, document, owningDocument, owningRelation, bean);
					} 
					catch (SkyveException e) {
						throw e;
					}
					catch (Exception e) {
						throw new DomainException(e);
					}
				}
				return;
			}
			visited.add(key);
		}
		
		// We are performing cyclic detection
		// If accept() returns true, we recurse, otherwise stop recursion for this branch
		StringBuilder sb = new StringBuilder(64);
		try {
			// Do the callback and recurse (or not)
			boolean recurse = false;
			if (visitNulls) {
				recurse = acceptNulls(binding, document, owningDocument, owningRelation, bean);
			}
			else if (bean != null) {
				recurse = accept(binding, document, owningDocument, owningRelation, bean);
			}
			
			if (recurse) {
				Module owningModule = customer.getModule(document.getOwningModuleName());

				// NB visit relations in the order they are defined in the documents.
				for (Attribute attribute : document.getAllAttributes(customer)) {
					if (attribute instanceof Relation relation) {
						// Don't visit inverses if not required
						if ((! visitInverses) && (attribute instanceof Inverse)) {
							continue;
						}

						String relationName = attribute.getName();
						Document relatedDocument = owningModule.getDocument(customer, relation.getDocumentName());
						Relation childRelation = relation;
						// association or one to one inverse
						if ((childRelation instanceof Association) ||
								((childRelation instanceof Inverse inverse) && 
									InverseCardinality.one.equals(inverse.getCardinality()))) {
							Bean child = null;
							if (bean != null) {
								try {
									child = (Bean) Binder.get(bean, relationName);
								}
								catch (ClassCastException e) {
									throw new DomainException("Is relation " + relationName + " property getter overridden in the document extension class?" +
																	" Possible bean accessor clash with is/get methods?", e);
								}
							}
							if ((child != null) || visitNulls) {
								// If we have a child bean instance, check for a polymorphic reference
								if (child != null) {
									String childBizModule = child.getBizModule();
									String childBizDocument = child.getBizDocument();
									if (! (relatedDocument.getOwningModuleName().equals(childBizModule) &&
											relatedDocument.getName().equals(childBizDocument))) {
										relatedDocument = customer.getModule(childBizModule).getDocument(customer, childBizDocument);
										child = Util.deproxy(child);
									}
								}
								
								sb.setLength(0);
								if (binding.length() != 0) {
									sb.append(binding).append('.');
								}
								sb.append(relationName);
								visit(sb.toString(), 
										relatedDocument,
										document, 
										childRelation, 
										child, 
										customer, 
										visited);
							}
						}
						else { // collection or many-sided inverse
							List<Bean> children = null;
							if (bean != null) {
								try {
									@SuppressWarnings("unchecked")
									List<Bean> temp = (List<Bean>) Binder.get(bean, relationName);
									children = temp;
								}
								catch (ClassCastException e) {
									throw new DomainException("Is relation " + relationName + " property getter overridden in the document extension class?" +
																	" Possible bean accessor clash with is/get methods?", e);
								}
							}
							if (children != null) {
								int i = 0;
								for (Bean child : children) {
									if (child != null) {
										// Check for a polymorphic child reference
										String childBizModule = child.getBizModule();
										String childBizDocument = child.getBizDocument();
										if (! (relatedDocument.getOwningModuleName().equals(childBizModule) &&
												relatedDocument.getName().equals(childBizDocument))) {
											relatedDocument = customer.getModule(childBizModule).getDocument(customer, childBizDocument);
											child = Util.deproxy(child);
										}
										
										sb.setLength(0);
										if (binding.length() != 0) {
											sb.append(binding).append('.');
										}
										sb.append(relationName).append('[').append(i).append(']');
										visit(sb.toString(), 
												relatedDocument, 
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
											relatedDocument,
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
										relatedDocument,
										document, 
										childRelation, 
										null, 
										customer, 
										visited);
							}
						}
					}
				}

				// Visit the parent of child documents (but not hierarchical documents which are loosely coupled)
				Document parentDocument = document.getParentDocument(customer); 
				if ((parentDocument != null) && 
						// child document, not a hierarchical document
						(! document.getName().equals(parentDocument.getName()))) {
					Bean parent = null;
					if (bean != null) {
						try {
							parent = (Bean) Binder.get(bean, ChildBean.PARENT_NAME);
						}
						catch (ClassCastException e) {
							throw new DomainException("Is parent property getter overridden in the document extension class?" +
															" Possible bean accessor clash with is/get methods?", e);
						}
					}
					if ((parent != null) || visitNulls) {
						// If we have a parent bean instance, check for a polymorphic reference
						if (parent != null) {
							String parentBizModule = parent.getBizModule();
							String parentBizDocument = parent.getBizDocument();
							if (! (parentDocument.getOwningModuleName().equals(parentBizModule) &&
									parentDocument.getName().equals(parentBizDocument))) {
								parentDocument = customer.getModule(parentBizModule).getDocument(customer, parentBizDocument);
								parent = Util.deproxy(parent);
							}
						}
						
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
		catch (SkyveException e) {
			throw e;
		}
		catch (Exception e) {
			throw new DomainException(e);
		}
	}

	/**
	 * @return The bizId of the instance unless vectorCyclicDetection is on, then return a path through the relation, module and document to the bean.
	 */
	private String determineVisitedKey(@Nonnull Bean bean, 
										@Nullable Document owningDocument,
										@Nullable Relation owningRelation) {
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

	/**
	 * Accept the reference.
	 * 
	 * @param binding	The visited binding.
	 * @param document	
	 * @param owningDocument The owning document that got us here (by recursion).
	 * @param owningRelation The owning document's relation that got us here (by recursion).
	 * 							This is null when top level bean or parent binding.
	 * @param bean	The visited bean.
	 * @return <code>false</code> to terminate, <code>true</code> to continue.
	 * @throws Exception
	 */
	abstract boolean accept(@Nonnull String binding,
								@Nonnull Document document,
								@Nullable Document owningDocument,
								@Nullable Relation owningRelation,
								@Nonnull Bean bean) 
	throws Exception;

	/**
	 * Accept the reference.
	 * 
	 * @param binding	The visited binding.
	 * @param document	
	 * @param owningDocument The owning document that got us here (by recursion).
	 * @param owningRelation The owning document's relation that got us here (by recursion).
	 * 							This is null when top level bean or parent binding.
	 * @param bean	The visited bean. This can be null.
	 * @return <code>false</code> to terminate, <code>true</code> to continue.
	 * @throws Exception
	 */
	abstract boolean acceptNulls(@Nonnull String binding,
											@Nonnull Document document,
											@Nullable Document owningDocument,
											@Nullable Relation owningRelation,
											@Nullable Bean bean) 
	throws Exception;
}
