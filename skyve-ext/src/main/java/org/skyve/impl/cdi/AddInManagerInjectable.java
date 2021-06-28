package org.skyve.impl.cdi;

import java.io.Serializable;

import javax.enterprise.inject.Alternative;

import org.skyve.EXT;
import org.skyve.addin.AddInManager;

/**
 * A proxy that can be Serialized with no state and continue to work after deserialization.
 * 
 * @author mike
 */
@Alternative
public class AddInManagerInjectable implements AddInManager, Serializable {
	private static final long serialVersionUID = 3294370979190313613L;

	@Override
	public void startup() {
		EXT.getAddInManager().startup();
	}

	@Override
	public void shutdown() {
		EXT.getAddInManager().shutdown();
	}

	@Override
	public <T extends Object> T getExtension(Class<T> type) {
		return EXT.getAddInManager().getExtension(type);
	}
}
