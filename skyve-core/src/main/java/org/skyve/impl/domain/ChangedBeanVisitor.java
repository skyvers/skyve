package org.skyve.impl.domain;

import org.skyve.domain.Bean;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Inverse;
import org.skyve.metadata.model.document.Relation;
import org.skyve.util.BeanVisitor;
import org.skyve.util.logging.Category;
import org.slf4j.Logger;

/**
 * Recurse a bean to determine if anything has changed
 */
class ChangedBeanVisitor extends BeanVisitor {
    private static final Logger DIRTY_LOGGER = Category.DIRTY.logger();

    private boolean changed = false;

	ChangedBeanVisitor() {
		// Check inverses for the cascade attribute
		super(true, false);
	}

	@Override
	protected boolean accept(String binding,
								Document documentAccepted,
								Document owningDocument,
								Relation owningRelation,
								Bean beanAccepted) {
		// Process an inverse if the inverse is specified as cascading.
		if ((owningRelation instanceof Inverse inverse) && 
				(! Boolean.TRUE.equals(inverse.getCascade()))) {
			return false;
		}

		if (beanAccepted.isChanged()) {
			changed = true;
            if (UtilImpl.DIRTY_TRACE) {
                DIRTY_LOGGER.info("UtilImpl.hasChanged(): Bean {} with binding {} is DIRTY",
                					beanAccepted.toString(),
                					binding);
			}
			return false;
		}
		return true;
	}

	boolean isChanged() {
		return changed;
	}
}
