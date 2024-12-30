package org.skyve.impl.util;

import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.metadata.model.document.Association.AssociationType;
import org.skyve.metadata.model.document.Collection.CollectionType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Reference;
import org.skyve.metadata.model.document.Reference.ReferenceType;
import org.skyve.metadata.model.document.Relation;
import org.skyve.util.BeanVisitor;

public abstract class CascadeDeleteBeanVisitor extends BeanVisitor {
	public CascadeDeleteBeanVisitor() {
		super(false, true);
	}
	
	@Override
	protected final boolean accept(String binding,
									Document visitedDocument,
									Document owningDocument,
									Relation owningRelation,
									Bean visitedBean)
	throws Exception {
		// No use checking referential integrity on a bean that is not persisted as nothing will have referenced it yet.
		if (visitedBean.isPersisted()) {
			// check if top document (or parent reference) or we have a persistent reference
			boolean validate = (owningRelation == null) || owningRelation.isPersistent();

			// check if binding isn't a parent binding - parent beans are not cascaded
			validate = validate && (! binding.endsWith(ChildBean.CHILD_PARENT_NAME_SUFFIX));

			// don't check aggregations or embedded documents as they are not cascaded
			if (validate && (owningRelation != null)) {
				// Can safely cast to Reference here as we should not be visiting inverses
				Reference owningReference = (Reference) owningRelation;
				ReferenceType referenceType = owningReference.getType();
				validate = (! AssociationType.aggregation.equals(referenceType)) && 
							(! AssociationType.embedded.equals(referenceType)) && 
							(! CollectionType.aggregation.equals(referenceType));
			}

			if (validate) {
				preDeleteProcessing(visitedDocument, visitedBean);
				// continue looking for composed objects to validate
				return true;
			}
		}

		// stop traversing this object graph branch as this is an aggregated or a parent object
		return false;
	}
	
	public abstract void preDeleteProcessing(Document documentToCascade, Bean beanToCascade) throws Exception;
}
