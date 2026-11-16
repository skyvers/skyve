package org.skyve.impl.dialog;

import org.skyve.dialog.Dialogs;

import jakarta.annotation.Nonnull;

/**
 * A singleton for the {@link Dialogs} implementation that {@link org.skyve.EXT#getDialogs()} returns.
 * NB This doesn't need to be thread safe as it is set at startup only.
 *
 * <p>Tests can install an implementation that answers questions without a request with {@link #set(Dialogs)},
 * and must restore the default with {@link #setDefault()} afterwards. Such tests must not run in parallel
 * threads in the same JVM.
 */
public class DialogsStaticSingleton {
	/** The installed implementation; {@code null} until startup sets it. */
	private static Dialogs instance;

	/**
	 * Disallows instantiation.
	 */
	private DialogsStaticSingleton() {
		// nothing to see here
	}

	/**
	 * Returns the installed implementation.
	 *
	 * @return	The implementation; never {@code null}.
	 * @throws IllegalStateException	if no implementation has been set at startup.
	 */
	public static @Nonnull Dialogs get() {
		if (instance == null) {
			throw new IllegalStateException("Dialogs has not been initialised.");
		}
		return instance;
	}

	/**
	 * Installs an implementation.
	 *
	 * @param instance	The implementation to install.
	 */
	public static void set(@Nonnull Dialogs instance) {
		DialogsStaticSingleton.instance = instance;
	}

	/**
	 * Installs a new {@link DefaultDialogs}.
	 */
	public static void setDefault() {
		instance = new DefaultDialogs();
	}
}
