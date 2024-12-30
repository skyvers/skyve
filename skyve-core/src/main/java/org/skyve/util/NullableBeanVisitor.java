package org.skyve.util;

import org.skyve.domain.Bean;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Relation;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * This class is used to visit a bean graph defined by its relations.
 * It will also visit through null relations by following the document metadata.
 */
public abstract class NullableBeanVisitor extends BeanVisitorImpl {
	/**
	 * Convenience constructor that enabled instance cyclic detection.
	 * See {{@link #NullableBeanVisitor(boolean, boolean, boolean)}
	 */
	public NullableBeanVisitor(boolean visitInverses, boolean vectorCyclicDetection) {
		super(visitInverses, vectorCyclicDetection);
		visitNulls();
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
	public NullableBeanVisitor(boolean visitInverses, boolean vectorCyclicDetection, boolean acceptVisited) {
		super(visitInverses, vectorCyclicDetection, acceptVisited);
		visitNulls();
	}

	/**
	 * Delegates to acceptNullable() in this implementation.
	 */
	@Override
	final boolean accept(String binding,
							Document document,
							Document owningDocument,
							Relation owningRelation,
							Bean bean)
	throws Exception {
		return acceptNulls(binding, document, owningDocument, owningRelation, bean);
	}

	/**
	 * Accept the reference.
	 * This class visits a bean's relations including relations that are not defined.
	 * In this method call, the bean can be null, which will visit all bindings defined in the document.
	 * If a related instance is null then the document metadata is used for traversal.
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
	@Override
	protected abstract boolean acceptNulls(@Nonnull String binding,
											@Nonnull Document document,
											@Nullable Document owningDocument,
											@Nullable Relation owningRelation,
											@Nullable Bean bean) 
	throws Exception;
}
