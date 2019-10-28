package org.skyve.impl.cdi;

import javax.enterprise.inject.Alternative;

import org.skyve.EXT;
import org.skyve.addin.AddInManager;

/**
 * A proxy that can be Serialized with no state and continue to work after deserialization.
 * 
 * @author mike
 */
@Alternative
public class AddInManagerInjectable implements AddInManager {

	@Override
	public <T extends Object> T getExtension(Class<T> type) {
		return EXT.getAddInManager().getExtension(type);
	}
}
