package org.skyve.impl.cdi;

import java.io.Serializable;

import org.skyve.EXT;
import org.skyve.addin.AddInManager;

import jakarta.enterprise.inject.Alternative;

/**
 * Stateless CDI proxy for {@link AddInManager}.
 *
 * <p>Delegates each call to {@link EXT#getAddInManager()} so a serialized proxy can
 * be rehydrated without carrying runtime state.
 *
 * <p>Threading: thread-safe as long as the backing add-in manager is thread-safe.
 */
@Alternative
public class AddInManagerInjectable implements AddInManager, Serializable {
	private static final long serialVersionUID = 3294370979190313613L;

	/**
	 * Starts add-in discovery and extension registration.
	 */
	@Override
	public void startup() {
		EXT.getAddInManager().startup();
	}

	/**
	 * Stops add-in infrastructure and releases related resources.
	 */
	@Override
	public void shutdown() {
		EXT.getAddInManager().shutdown();
	}

	/**
	 * Returns the extension implementation for the requested contract.
	 *
	 * @param type the extension contract type.
	 * @return the resolved extension instance, or {@code null} when none is registered.
	 */
	@Override
	public <T extends Object> T getExtension(Class<T> type) {
		return EXT.getAddInManager().getExtension(type);
	}
}
