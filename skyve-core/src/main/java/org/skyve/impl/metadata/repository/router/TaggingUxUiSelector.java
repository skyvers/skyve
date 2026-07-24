package org.skyve.impl.metadata.repository.router;

import java.io.Serializable;

/**
 * Marks UX/UI selector implementations that require web-tier request context.
 *
 * Tagging interface.
 * The actual interface is defined in UxUiSelector but this has a dependency on HttpServletRequest.
 * 
 * <p>The functional selector contract is defined by
 * {@code org.skyve.metadata.router.UxUiSelector}. This marker exists so router
 * bootstrap and dispatch code can recognise implementations that are coupled to
 * servlet request state and must be created in the web runtime.
 */
public interface TaggingUxUiSelector extends Serializable {
	// nothing to see here
}
