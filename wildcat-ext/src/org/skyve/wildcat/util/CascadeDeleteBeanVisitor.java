package org.skyve.wildcat.util;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.document.Association;
import org.skyve.metadata.model.document.Collection;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Reference;
import org.skyve.metadata.model.document.Association.AssociationType;
import org.skyve.metadata.model.document.Collection.CollectionType;

public abstract class CascadeDeleteBeanVisitor extends BeanVisitor {
	private static final String CHILD_PARENT_NAME_SUFFIX = "." + ChildBean.PARENT_NAME;
	
	@Override
	protected final boolean accept(String binding,
									Document visitedDocument,
									Document owningDocument,
									Reference owningReference,
									Bean visitedBean,
									boolean visitingInheritedDocument)
	throws Exception {
		// No use checking referential integrity on a bean that is not persisted as
		// nothing will have referenced it yet.
		Persistent persistent = visitedDocument.getPersistent();
		if ((persistent != null) && 
				(persistent.getName() != null) &&
				CORE.getPersistence().isPersisted(visitedBean)) { // persistent document and persisted bean
			// check if top document or we have a persistent reference
			boolean validate = (owningReference == null) || owningReference.isPersistent();

			// check if binding isn't a parent binding - parent beans are not cascaded
			validate = validate && (! binding.endsWith(CHILD_PARENT_NAME_SUFFIX));

			// don't check aggregations as they are not cascaded
			if (validate && (owningReference instanceof Association)) {
				validate = (! AssociationType.aggregation.equals(owningReference.getType())); // not an aggregation
			}
			if (validate && (owningReference instanceof Collection)) {
				validate = (! CollectionType.aggregation.equals(owningReference.getType())); // not an aggregation
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
